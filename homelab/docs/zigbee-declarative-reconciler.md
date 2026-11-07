# Design sketch: declarative Zigbee reconciler (evaluate before building)

**Status:** proposal / not built. This documents a design so it can be judged
before any code is written.

**Related but distinct:** `docs/zigbee-config-snapshot.md` (implemented) captures
the UI-editable `devices.yaml`/`groups.yaml` so a deploy is a config backup. That
covers *naming/grouping config*; this doc covers the *live mesh* (membership +
bindings) that config files don't hold.

## Problem

We want Kubernetes-style declarative management of the Zigbee mesh: assert desired
**groups**, **group membership**, and especially **bindings** (device→device /
device→group links that keep local control working when the coordinator/z2m/HA are
down), and have a control loop converge the live network to match. See
`docs/home-assistant.md` for the coordinator-down rationale.

The blocker is not that this state is "runtime" — it's that z2m ships the
*imperative* half (idempotent request APIs) but **no reconciler**, and one slice
(the physical join) has an irreducible manual precondition.

## Prior art (searched Aug 2026 — build on these, don't reinvent)

| Thing | Fit | Notes |
|---|---|---|
| z2m `bridge/request` + retained `bridge/devices`/`bridge/groups` | **the substrate to build on** | Standardized read + write API; this is the reconciler's foundation. |
| HA Terraform providers (Mikescops, Fabianoshz) | ✗ wrong layer | Manage HA *entities* (lights/media players), not the Zigbee mesh or bindings. |
| z2m native declarative config | ✗ removed | 2.0 dropped declarative groups; open requests to restore ([#27619], bindings-in-yaml [#1716]). **Watch these** — if they land, parts of this become native `settings`. |
| k8s operators / operator-sdk | ✗ overkill | Right *pattern*, wrong substrate for a single NixOS host. Borrow the reconcile-loop idea, not a CRD/etcd/controller-runtime. |
| Generic MQTT clients (paho-mqtt, aiomqtt) | ✓ use directly | The reconciler is "just" an MQTT client with logic. |

**Conclusion:** no turnkey tool. Build a small reconciler on the z2m bridge API,
with the **desired-state spec generated from Nix** (repo = spec store, like
git/etcd; a systemd service = the controller). That realizes the k8s analogy on a
NixOS box without dragging in k8s.

## What is / isn't reconcilable

- **Reconcilable** (idempotent z2m APIs, readable current state → drift + converge):
  friendly_name, device options, group create/delete, group membership, **bindings**.
- **Irreducibly manual:** the physical **join** (reset + permit-join window + key
  exchange). No API materializes an unpaired device — the analog of bare-metal node
  provisioning in k8s. The loop can *declare intent and report drift* ("pending
  join") but cannot press the button.
- **Set-once:** network channel / PAN ID / network key (changing them re-forms the
  network). Treat as immutable spec.

## Spec schema (rendered from Nix to JSON)

Identify devices by **IEEE address** (drift-stable); `friendly_name` is a *managed
attribute*, not an identity key.

```jsonc
{
  "groups": [
    { "id": 6, "friendly_name": "living_room_lights",
      "members": ["0x0011..bulb1/1", "0x0022..bulb2/1"] }
  ],
  "devices": [
    { "ieee": "0x00aa..remote", "friendly_name": "living_room_remote",
      "options": { } }
  ],
  "bindings": [
    { "from": "0x00aa..remote/1", "to": { "group": 6 } },
    { "from": "0x00bb..switch/1", "to": { "ieee": "0x0011..bulb1", "endpoint": 1 },
      "clusters": ["genOnOff", "genLevelCtrl"] }
  ],
  "prune": true          // remove *managed* resources not in spec (see ownership)
}
```

## Reconcile loop

1. **Read actual state** from retained topics: `zigbee2mqtt/bridge/devices`
   (IEEE + `endpoints.<ep>.bindings[]`), `zigbee2mqtt/bridge/groups`
   (`members[]`), `bridge/info`.
2. **Diff** per resource type against the spec.
3. **Converge**, in dependency order, via `bridge/request/*`, correlating each
   `bridge/response/*` on a unique `transaction` id (`status: ok|error`):
   - rename (`device/rename`) → device options (`device/options`) →
     group create (`group/add`) → membership (`group/members/add|remove`) →
     bindings (`device/bind|unbind`).
   - A device in the spec but absent from `bridge/devices` → record **pending
     join** drift, skip (do not error).
4. **Idempotent:** compare-then-apply; steady state = no writes.
5. **Trigger:** `After=zigbee2mqtt.service` one-shot on (re)start, on retained-topic
   change (event-driven, like a watch, debounced), and a periodic `.timer` as a
   safety net. Single instance (flock / non-overlapping oneshot).
6. **Report:** publish a summary (converged / pending-join / errors) to an MQTT
   status topic so it can surface as an HA sensor.

### Ownership & pruning (the key safety decision)
Only touch **managed** resources so the loop never deletes hand-made groups/bindings.
Options: a managed-id namespace (e.g. group ids ≥ 1000), or a marker in
friendly_name, or track a managed-set. Default `prune` to converge presence only;
enable delete-pruning within the managed set once trusted. (This is k8s
labels/ownerRefs, scaled down.)

### Failure modes to handle
- **Sleeping end devices:** bind/options fail while asleep → retry with backoff;
  best converged *event-triggered* right after the device reports. Bind targets
  should be routers (mains), not battery devices (see `docs/home-assistant.md`).
- **Ordering:** create group before members before binding-to-group; rename before
  anything references the new name (or just always key by IEEE).
- **State lag:** z2m republishes `bridge/devices` after a bind — re-read before
  declaring success.
- **Not atomic:** partial convergence is fine; the loop re-runs.

## NixOS wiring (fits this repo)

- Keep engine config, group *definitions*, and HA-discovery overrides in
  `services.zigbee2mqtt.settings` (native declarative where z2m still supports it).
- Add to `nixos/modules/home-automation.nix`:
  - the spec as a Nix attrset → `pkgs.writeText "zigbee-spec.json" (builtins.toJSON spec)`;
  - `systemd.services.zigbee-reconcile` running a `pkgs.python3.withPackages
    (ps: [ ps.paho-mqtt ])` script, `After=zigbee2mqtt.service`, MQTT creds from the
    existing `mqtt-password*` agenix secret;
  - a `systemd.timers.zigbee-reconcile` (e.g. every 10 min) + socket/exec trigger on
    topic change.
- Result: desired state is version-controlled and reviewed in the flake; the service
  reconciles it — declarative-with-reconciliation, gated only on the physical join.

## Phased plan & build-vs-wait

- **MVP (~80 lines):** one-shot, additive-only, **bindings + group membership**
  (the coordinator-down value that is *not* natively declarative). No pruning, no
  rename/options. Run `After=zigbee2mqtt` + timer.
- **v1 (~250–400 lines):** drift reporting, event-triggered runs, pruning within a
  managed set, retries for sleeping devices, MQTT status topic / HA sensor.
- **Skip / low value:** rename & options (easy in the frontend; small payoff).

**Build-vs-wait:** the group/binding gap is exactly what upstream requests [#27619]
and [#1716] target. If z2m re-adds declarative groups/bindings, the MVP's scope
collapses into native `settings`. Recommendation: **wait to build until you have
devices and a concrete binding you want to hold stable**, keep the MVP tiny, and
re-check those two issues first — don't build v1 speculatively.

[#27619]: https://github.com/Koenkk/zigbee2mqtt/issues/27619
[#1716]: https://github.com/Koenkk/zigbee2mqtt/issues/1716
