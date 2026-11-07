# Zigbee Coordinator Transition Playbook

Runbook for moving aida's Zigbee network to a **new coordinator** (or recovering a
flaky one) **without re-pairing devices**. Written from the Sept 2026 SLZB-MR5U →
Sonoff MG24 migration — every gotcha below was hit for real, in the order listed.

---

## TL;DR

1. z2m can adopt a new coordinator and keep **all** devices **iff** the new radio
   joins the **same network parameters** (IEEE, PAN ID, extended PAN ID, network
   key), restored from `coordinator_backup.json`. Same adapter family (both
   `ember`/EmberZNet) makes it seamless — no re-pair.
2. **Write the backup IEEE onto the new dongle BEFORE z2m's first start on it**
   (§2 step 5). z2m's restore records the dongle's *current* IEEE as the network's
   **trust-center address**. Restore under the factory IEEE and that stale address
   survives a later IEEE fix (Gotcha #4).
3. Five things bite **after** the swap, in this order:
   - **IEEE clone silently fails** on EFR32 → the new radio keeps its factory IEEE
     → `ROUTE_ERROR_ADDRESS_CONFLICT` storm + broken reporting. **Fix:** write the
     backup IEEE onto the dongle.
   - **Stale routes** cached in mains routers don't self-heal → `ADDRESS_CONFLICT`
     persists. **Fix:** whole-house power cycle (all mains routers at once, leave
     aida up).
   - **A whole-house power cut wipes coordinator bindings** on switches/relays →
     scene buttons stop triggering HA + local/sensor load changes stop syncing.
     **Fix:** z2m Reconfigure the affected switches/relays.
   - **Trust-center address stuck at the factory IEEE** (if the IEEE was fixed
     *after* the first restore) → existing devices fine, but **new devices can't
     stay joined**. **Fix:** make the dongle leave its network so z2m re-restores
     from backup (§7).
   - **Bindings to the coordinator's byte-reversed IEEE** → nothing visibly broken,
     but every report floods the mesh with an unanswerable address lookup (~70% of all
     traffic here). **Fix:** unbind via a temporary placeholder device (§7a).
4. After all that, **"Permit join (All)" still fails when *re-pairing* a device that
   was joined before**: its previous parent router won't take it back. That's
   pairing behavior, not network damage. Joins through any router work. **Workaround:**
   permit-join on the coordinator or a router that wasn't its last parent (§11).

---

## 0. Where everything lives

**Repo (this config):**
- `modules/home-automation.nix` — z2m `serial` block, systemd resilience
  (`Restart=always`, `RestartSec=30`, `startLimitIntervalSec=0`), `DeviceAllow`
  for the USB tty, and the `devices.yaml`/`groups.yaml` seed + drift-check.
- `hosts/aida.nix` — gatus watchdog (z2m `:8080` probe → ntfy via aroldo) + agenix
  secret declarations.
- `hosts/aida/zigbee2mqtt/{devices.yaml,groups.yaml}` — repo baseline of the
  UI-owned z2m files. Capture live drift with **`z2m-foldin aida`**, then commit.

**On aida, NOT in the repo (`/var/lib/zigbee2mqtt/`):**
- `coordinator_backup.json` — the **network identity** (coordinator IEEE, PAN,
  ext PAN, channel, network key). This is what lets a new radio adopt the network.
- `database.db` — device list, bindings, reporting config (z2m's view).
- `configuration.yaml` — generated from the Nix `settings` (don't hand-edit).
- `devices.yaml` / `groups.yaml` — UI-owned; seeded-if-missing from the repo.

**Secrets / access (paths, not secrets):**
- MQTT: user `mqtt`, broker `mqtt://127.0.0.1:1883`. Password in
  `/run/agenix/mqtt-password-env` (var `ZIGBEE2MQTT_CONFIG_MQTT_PASSWORD`).
- HA REST/WS token: `/run/agenix/home-assistant-matter-hub-token` → HA on
  `http://127.0.0.1:8123` (loopback; use `sudo cat` to read).
- `mosquitto_sub`/`_pub`: no clients on `$PATH` — they live in the running
  mosquitto build's bin dir: `dirname "$(systemctl show mosquitto -p ExecStart --value | grep -oE '/nix/store/[^ ]*/bin/mosquitto')"`.

---

## 1. Know your failure modes (why we left the SLZB)

The SLZB-MR5U was network-attached (ESP32 core + EFR32 radio, z2m over TCP
`:6638`). It degraded and was RMA'd. Signatures seen — any **recurring** set of
these means "coordinator is dying, plan a transition":

| Signature (in `journalctl -u zigbee2mqtt`) | Meaning |
|---|---|
| `connect EHOSTUNREACH …:6638` / `HOST_FATAL_ERROR` | TCP coordinator app-layer wedged (web UI may still 200) |
| `RESET_SOFTWARE` / `ASH_NCP_FATAL_ERROR` on `VERSION` | radio NCP resetting on handshake (power brownout / fw / HW) |
| `ERROR_EXCEEDED_MAXIMUM_ACK_TIMEOUT_COUNT` on `SEND_UNICAST` | ASH serial-over-TCP transport failing mid-op |

Triage notes:
- **Probe from aida, not a Wi-Fi host** on another subnet (ICMP/ping is unreliable
  to these devices anyway).
- **`systemctl stop zigbee2mqtt` before probing** a TCP coordinator's `:6638` — z2m
  hogs the single ser2net client socket, so `:6638` looks "flappy" while z2m
  crashloops even if the coordinator is fine.
- The gatus **`:8080` (z2m) probe is the reliable down-detector**; a bare TCP
  `:6638` probe under-detects (it accepts even while the NCP is crashing).
- Ruling out external causes before calling it HW: swap to a solid ≥2A 5V supply +
  short thick cable, verify temps, check firmware. If a **clean reflash still
  degrades within minutes on good power at normal temps → internal HW → replace.**

**Lesson:** a **USB-attached** EFR32 stick straight into the co-located host
deletes the entire network-attached-ESP32 layer that caused every failure mode
above. Prefer it over another network coordinator.

---

## 2. Pre-flight (before touching anything)

1. **Pick a same-family radio.** ember→ember (EmberZNet/EFR32) restores the network
   with no re-pair. A different stack means a harder migration.
2. **Save the network identity.** Back up the two binary files aside:
   ```bash
   ssh aida 'sudo cp -a /var/lib/zigbee2mqtt/coordinator_backup.json{,.premigrate} \
                    && sudo cp -a /var/lib/zigbee2mqtt/database.db{,.premigrate}'
   ```
3. **Record the current coordinator IEEE + params** (you'll verify against these):
   ```bash
   ssh aida 'sudo jq "{ieee:.coordinator_ieee, pan:.pan_id, ext:.extended_pan_id, ch:.channel}" \
                    /var/lib/zigbee2mqtt/coordinator_backup.json'
   ```
   ⚠️ **Byte-order gotcha:** the backup stores the IEEE **little-endian (reversed)**.
   z2m *displays* it MSB-first. e.g. backup `24d660fffe725ca4` == displayed
   `a45c72feff60d624` (reverse the byte pairs). You'll need the **displayed
   (MSB-first)** form for the IEEE write in §4.
4. **Get the new dongle's stable path.** Plug it into a **rear/direct USB port (not
   a hub)** and:
   ```bash
   ssh aida 'ls -l /dev/serial/by-id/'
   ```
5. **Write the backup IEEE onto the new dongle now, before z2m ever starts on
   it.** Use the `write-ieee` commands from §4 (z2m stopped, IEEE in displayed
   MSB-first form). z2m's automatic clone is unreliable on EFR32 (Gotcha #1). If
   the first restore runs under the factory IEEE, the trust-center address is
   recorded wrong and only a full re-restore fixes it (Gotcha #4). Doing this first
   avoids both Gotchas #1 and #4, and the `ADDRESS_CONFLICT` pollution of Gotcha #2.

---

## 3. Migration procedure

1. **Stop z2m** (also frees the old coordinator socket for clean probing):
   ```bash
   ssh aida 'sudo systemctl stop zigbee2mqtt'
   ```
2. **Edit the repo serial block** (`modules/home-automation.nix`) to the new
   dongle. Current known-good block:
   ```nix
   serial = {
     port = "/dev/serial/by-id/usb-SONOFF_SONOFF_Dongle_Plus_MG24_…-if00-port0";
     adapter = "ember";
     baudrate = 115200;
     disable_led = false;
   };
   ```
   And the systemd bits that make z2m tolerate a briefly-absent coordinator (keep
   these — they are why z2m self-recovers instead of crashlooping to death):
   ```nix
   systemd.services.zigbee2mqtt = {
     serviceConfig = {
       DeviceAllow = lib.mkAfter [ "char-ttyUSB rw" "char-ttyACM rw" ];  # DevicePolicy=closed
       Restart = lib.mkForce "always";
       RestartSec = lib.mkForce 30;
     };
     startLimitIntervalSec = 0;   # never trip the start-rate limiter
   };
   ```
   If leaving a TCP coordinator, also drop its dead gatus `tcp://…:6638` endpoint in
   `hosts/aida.nix` (keep the z2m `:8080` one).
3. **Stage new files** (`git add`) if any, then eval + deploy:
   ```bash
   task eval                        # or: nix eval …#nixosConfigurations.aida…drvPath
   nixos-deploy aida                # nixos-rebuild switch --flake …#aida --target-host aida …
   ```
4. **Start z2m** and watch it adopt the network from the backup:
   ```bash
   ssh aida 'sudo systemctl start zigbee2mqtt; journalctl -u zigbee2mqtt -f'
   ```
   Look for the `ember` handshake reaching "Coordinator ready" and devices
   republishing. Control should work immediately.

---

## 4. GOTCHA #1 — IEEE clone silently fails on EFR32 (z2m #32477)

**On EmberZNet/EFR32, z2m's automatic IEEE-clone often silently no-ops**, so the new
dongle keeps its **factory IEEE** instead of adopting the backup's. The network
"works" for control, but you get:

- `ROUTE_ERROR_ADDRESS_CONFLICT for "0"` **storm** (tens/sec), and
- **broken device→coordinator reporting bindings** (most devices stop reporting;
  control still works because commands are addressed outbound).

**Detect the mismatch:**
```bash
# live coordinator IEEE (MSB-first) — compare to the backup's reversed value
ssh aida '<mosquitto_sub> -h 127.0.0.1 -u mqtt -P "$PW" -t zigbee2mqtt/bridge/info -C 1 -W 5 \
          | jq -r .coordinator.ieee_address'
# conflict rate
ssh aida 'journalctl -u zigbee2mqtt --since -60s | grep -c ADDRESS_CONFLICT'
```
If the live IEEE is the dongle's **factory** value (not the backup's), fix it:

**Fix — write the backup IEEE onto the dongle** with `universal-silabs-flasher`
(not in nixpkgs; use a throwaway venv). z2m **must be stopped** first.
```bash
ssh aida 'sudo systemctl stop zigbee2mqtt'
# one-time venv:
ssh aida 'nix shell nixpkgs#python3 -c bash -lc "
  python -m venv /tmp/usf-venv && /tmp/usf-venv/bin/pip -q install universal-silabs-flasher"'
# OPTIONAL non-destructive read first (should show the factory IEEE):
ssh aida 'sudo /tmp/usf-venv/bin/universal-silabs-flasher --device <by-id> probe'
# write the backup IEEE — pass it in DISPLAY order (MSB-first), no 0x, no colons:
ssh aida 'sudo /tmp/usf-venv/bin/universal-silabs-flasher -v --device <by-id> \
          write-ieee --ieee <COORD_IEEE_MSB_FIRST>'
ssh aida 'sudo systemctl start zigbee2mqtt'
```
Notes:
- The tool handles the little-endian conversion; **pass the IEEE exactly as z2m
  displays it.**
- It writes `NVM3KEY_STACK_RESTORED_EUI64` (a **rewritable** NV3 token) — this is
  reversible, *not* the one-shot `MFG_CUSTOM_EUI_64` manufacturing token.
- On some Sonoff sticks `write-ieee` can fail harmlessly (bug #64) → retry / patched
  flasher. Last resort if it truly can't clone: re-pair all devices.

**Verify:** live coordinator IEEE now matches the backup (reversed), devices report
again. The `ADDRESS_CONFLICT` storm will *persist* for now — that's Gotcha #2.

> ⚠️ **If you fixed the IEEE *after* z2m had already restored the network** (as
> happened in Sept 2026), the dongle's stored **trust-center address is still the
> factory IEEE**. Existing devices won't notice, but new devices can't join. Check
> it and fix it per §7 *before* calling the migration done.

---

## 5. GOTCHA #2 — `ADDRESS_CONFLICT` storm that won't self-heal

After the IEEE is correct, the conflict **does not converge on its own** (watched
for hours: oscillates, doesn't decay). Why:

- During the wrong-IEEE window, every mains **router** cached a stale mapping of
  `0x0000` (the coordinator's short address) → the *old/factory* IEEE in its own
  firmware neighbor/route tables. Those live in the routers, not in z2m /
  `database.db` / the coordinator, so a z2m restart or NVM edit can't flush them.
- The EZSP callback (`ezspIncomingNetworkStatusHandler … target=0`) reports only the
  *target* (the coordinator), **never which router emitted** the conflict — so you
  **can't identify** the polluted routers from logs. Piecemeal cycling = whack-a-mole.

**Fix — one simultaneous whole-house power cycle:** turn **every** breaker off for
30+ seconds, then back on. (Cycling all mains routers *at once* is what clears every
router's stale cache together; doing circuits one-at-a-time doesn't converge.)
Battery end-devices don't route, so they're irrelevant.

- Confirmed result: `ADDRESS_CONFLICT` 45–77/s → **0**, 0 delivery failures, all
  devices reporting, coordinator IEEE persisted, 0 NCP faults.
- **Measure before/after:** `journalctl -u zigbee2mqtt --since -60s | grep -c ADDRESS_CONFLICT`.
- If you can't cut the whole house: this stays as background RF noise; it's mostly
  cosmetic once reporting works (few/no delivery failures), just not a clean steady
  state.

---

## 6. GOTCHA #3 — the power cut wipes coordinator bindings on switches/relays

The whole-house cycle that fixes Gotcha #2 has a side effect: **it wipes the
coordinator-binding tables on some mains routers** (device-NVM state). Symptoms look
device-specific, **not** power-related, and a quiet mesh does NOT fix them:

- **Inovelli scene/config buttons stop triggering HA automations** — and **no LED
  feedback** on the config press. The `manuSpecificInovelli` cluster is no longer
  bound to the coordinator, so button/scene events go nowhere.
- **A switch/dimmer's local or sensor-driven load change stops syncing to HA** (e.g.
  the Kitchen VZM32 mmwave turning on the load) — its `genOnOff`/occupancy reports
  have no destination. (Reporting *intervals* still show configured, which makes it
  look half-working.)

**Detect — audit the whole mesh for the fingerprint** "cluster has a
`configured_reporting` but is NOT bound to the coordinator":
```bash
COORD=<COORD_IEEE_MSB_FIRST>   # e.g. 0xa45c72feff60d624 (with 0x)
ssh aida "<mosquitto_sub> -h 127.0.0.1 -u mqtt -P \"\$PW\" \
  -t zigbee2mqtt/bridge/devices -C 1 -W 5" | jq -r --arg coord "$COORD" '
  .[] | select(.type!="Coordinator") | . as $d
  | ([ $d.endpoints[]?
       | (.configured_reportings // [] | map(.cluster) | unique) as $rep
       | (.bindings // [] | map(select(.target.ieee_address==$coord) | .cluster)) as $bound
       | ($rep - $bound)[]
     ]) as $missing
  | select(($missing|length)>0)
  | [$d.friendly_name, ($d.power_source//"?"), ($missing|unique|join(","))] | @tsv' | sort
```

**Interpret (important — the detector over-flags):**
- **FIX these:** mains-powered **switches, dimmers, relays** flagged for
  `genOnOff`/`genLevelCtrl`/metering/`manuSpecificInovelli`/occupancy — real wiped
  bindings (Inovelli VZM31/VZM32, Sonoff ZBMINIR2, DG15S dimmer, etc.).
- **Ignore (normal, not a defect):**
  - **Bulbs** (Hue `9290031346`, Gledopto `GL-C-008P`, Third Reality `3RCB…`) — z2m
    configures their reporting *without* a coordinator binding; Reconfigure will
    **not** add one and they report fine anyway.
  - **Battery end-devices** flagged only for `genPowerCfg` (door/contact sensors,
    valve, SOS, the Tuya knob) — report via attribute/IAS without a binding, and a
    Reconfigure on a sleepy device just times out.
  - **`SNZB-06P24` / `PS-S04D` presence sensors** — occupancy is attribute-only and
    can't be bound.

**Fix — z2m Reconfigure each affected switch/relay** (idempotent; preserves custom
direct-bindings, e.g. paddle/config-button EP2/EP3 bound to other devices/groups):
```bash
# single device
ssh aida '<mosquitto_pub> -h 127.0.0.1 -u mqtt -P "$PW" \
  -t zigbee2mqtt/bridge/request/device/configure -m "{\"id\":\"Kitchen Overhead Lights\"}"'
```
For several, do them **sequentially with ~20s between each** (avoid an airtime burst
on a freshly-stable mesh) and watch `zigbee2mqtt/bridge/response/device/configure`
for `status:"ok"`. **Do NOT blanket-reconfigure everything** — battery devices fail,
bulbs gain nothing, and it just churns the mesh.

**Verify a fix functionally:** capture the device's topic while triggering it —
```bash
ssh aida '<mosquitto_sub> -h 127.0.0.1 -u mqtt -P "$PW" -v -W 60 \
  -t "zigbee2mqtt/<name>/#"'   # press config button / trigger sensor; expect action/state reports
```
then confirm HA reflects it (`GET /api/states/<entity>`). Re-run the §6 audit —
switches/relays should drop off the list (bulbs/battery/attr-only stay, expected).

> Takeaway: **after ANY whole-house or circuit-wide power cut, expect to Reconfigure
> the switches/relays.** It's a quick targeted sweep, not a full-mesh reconfigure.

---

## 7. GOTCHA #4 — trust-center address stuck at the factory IEEE (new devices can't join)

The coordinator is also the network's **Trust Center** (TC). EmberZNet records the
TC's address in its security state when the network is **formed or restored**. If
z2m restored the network while the dongle still had its factory IEEE (Gotcha #1),
the recorded TC address is the factory IEEE. Fixing the IEEE afterwards (§4) changes
the dongle's own EUI64, but every later z2m start just **resumes**
(`[INIT TC] Adapter network matches config` → `started (resumed)`), so the TC
address is never rewritten. **Nothing in the z2m logs flags it.** In Sept 2026 it
went unnoticed for 9 days, until the first attempt to pair something new.

**Symptoms** (seen 2026-10-03 pairing Third Reality Smart Plug Gen3 `3RSP02064Z`):
- **Existing devices: completely fine.** They finished their key exchange long ago.
- **Permit-join "All":** a new device in pairing mode produces **no z2m log line at
  all**, even when it's right next to the coordinator. At debug level there are
  **zero** `TRUST_CENTER_JOIN_HANDLER` / `CHILD_JOIN_HANDLER` EZSP callbacks.
  Joins relayed by routers die silently.
- **Permit-join "Coordinator":** the device joins and interviews, then **leaves on
  its own ~20–40 s later** (`Device '…' left the network`) and goes back to pairing
  mode (LED blinking). Configure fails with `Delivery failed` /
  `Tried to get unknown/deleted device`. Zigbee 3.0 devices must complete a post-join
  trust-center link-key exchange or leave.
- **Lenient devices can stick anyway** (an Aqara T2 relay joined fine on Sep 25),
  which hides the problem.
- **Easy to misdiagnose as a dead device.** The first plug was written off as
  defective until a second, brand-new plug reproduced the failure exactly.

**Detect: read the NCP's security state.** z2m must be stopped, so expect a ~10 s
outage. The bellows CLI's `info` prints the same data (its 5th line), but its
`leave` command is broken on EZSP v13, so build one small script that does both,
using the bellows CLI's own interpreter and site paths:
```bash
# one-time: create /tmp/ncp.py + /tmp/ncp.python on aida
ssh aida 'W=$(nix build --no-link --print-out-paths nixpkgs#python3Packages.bellows)/bin/.bellows-wrapped
  head -1 "$W" | sed "s/^#!//" > /tmp/ncp.python   # pinned python
  sed -n 3p "$W" > /tmp/ncp.py                     # its site-path setup line
  cat >> /tmp/ncp.py <<"EOF"
import asyncio, sys
from bellows.cli import util
PORT = "/dev/serial/by-id/usb-SONOFF_SONOFF_Dongle_Plus_MG24_9ecc6e632b8cf0119f9e2eb9d9065118-if00-port0"
async def main(leave):
    s = await util.setup(PORT, 115200)
    print("networkInit:", await s.initialize_network())
    print("eui64:", await s.getEui64())
    print("state:", await s.networkState())
    print("security:", await s.getCurrentSecurityState())
    if leave:
        print("leaveNetwork:", await s.leaveNetwork())
        await asyncio.sleep(5)
        print("state after:", await s.networkState())
    await s.disconnect()
asyncio.run(main(len(sys.argv) > 1 and sys.argv[1] == "leave"))
EOF'
# read-only check (z2m stopped ~10 s; the restart is chained unconditionally)
ssh aida 'sudo systemctl stop zigbee2mqtt; sleep 3; sudo $(cat /tmp/ncp.python) /tmp/ncp.py; sudo systemctl start zigbee2mqtt'
```
**Healthy:** `trustCenterLongAddress` == the coordinator IEEE
(`a4:5c:72:fe:ff:60:d6:24`). **Broken:** it shows the dongle's factory IEEE
(`f0:44:d3:ff:fe:42:9e:b9` on this MG24).

> aida's login shell is **zsh**. In remote commands, avoid words starting with `=`
> (`echo ===` fails with `zsh: == not found`; that once killed a `trap` and left z2m
> stopped). Chain the `systemctl start` with `;` instead of relying on a trap. If you
> redact bellows output, **don't filter on "hashed"**: the security line contains
> `TRUST_CENTER_USES_HASHED_LINK_KEY` and would vanish.

**Fix: make the NCP leave its network so z2m re-restores from backup**, now with
the correct IEEE:
```bash
ssh aida 'sudo systemctl stop zigbee2mqtt; sleep 3   # z2m writes a FRESH coordinator_backup.json on stop
  B=/var/lib/zigbee2mqtt/backup-pre-tc-fix-$(date +%Y%m%d-%H%M%S); sudo mkdir -p $B
  sudo cp -a /var/lib/zigbee2mqtt/{coordinator_backup.json,database.db,configuration.yaml,state.json} $B/
  sudo $(cat /tmp/ncp.python) /tmp/ncp.py leave       # expect: state after: NO_NETWORK
  sudo systemctl start zigbee2mqtt'
ssh aida 'journalctl -u zigbee2mqtt --since -2min | grep -E "INIT TC|INIT FORM|herdsman started"'
```
Expect `[INIT TC] Forming from backup.` → `[INIT FORM] New network formed!` (same
PAN/ext PAN/channel) → `zigbee-herdsman started (restored)`. Then re-run the read:
the TC address should equal the coordinator IEEE.
- **Precondition:** the backup must match z2m's configured `pan_id` /
  `ext_pan_id` / `network_key`, or z2m forms a *new* network from config (= re-pair
  everything). Check before running it: z2m logs `[INIT TC] Adapter network matches
  config` on a normal start when they match.
- z2m writes a fresh backup on every stop, so the restored frame counter is current.
  The `hashed_tclk` seed and network key are carried over (hashes verified identical
  before/after on 2026-10-03).
- **`bellows leave` (CLI, 0.49.1) crashes on EZSP v13** before doing anything
  (`TypeError: 'sl_Status' object is not subscriptable`). Use the script above.
- Observed cost: ~16 s outage. Existing devices carried on with no re-pair (46/46
  reporting, no conflicts or new delivery failures).

**Result (2026-10-03):** **joins fixed.** A brand-new plug joined, configured, and
stayed. "All" pairing still looked broken afterwards. The 2026-10-04 testing in §11
showed that joins through routers work fine; what fails is a *re-paired* device's
previous parent refusing to take it back.

**Pairing advice:** for a device that has been joined before, use **Permit join ▾ →
Coordinator** (or a router other than its last parent), or publish
`zigbee2mqtt/bridge/request/permit_join` `{"time":254,"device":"Coordinator"}`. Wait
for `Successfully configured` before moving it.

---

## 7a. GOTCHA #5 — bindings to the byte-reversed coordinator IEEE (found 2026-11-05)

**Symptom:** nothing visibly broken. A sniffer shows that most of the mesh's traffic is
ZDO `NWK_addr_req` broadcasts for `24:d6:60:ff:fe:72:5c:a4`, which is the coordinator's
IEEE `a4:5c:72:fe:ff:60:d6:24` **byte-reversed**. No device has that address, so the
lookup is never answered and repeats. Each broadcast is relayed by every router. Here it
was **~80,000 of ~112,000 frames an hour**.

**Cause:** devices hold reporting bindings (and IAS sensors a CIE address) pointing at
the reversed IEEE, alongside their correct coordinator bindings. Every report sends one
copy to the coordinator (works) and triggers an address lookup for the other. Most
likely created during the migration, when the IEEE byte order was in flux (§4); not
verified. Bulbs that had **only** the reversed binding never reported state to z2m at
all.

**Detect:** `sudo grep -a -c 24d660fffe725ca4 /var/lib/zigbee2mqtt/database.db`
(z2m's records; not proof of what's on the device), or count `NWK_addr_req` frames with
the sniffer (§11 OPEN 2, "Sniffer on aida").

**Fix (what worked):**
1. z2m's `bridge/request/device/binds/clear` with `ieee_list` uses ZDO
   Clear_All_Bindings, an R23 command: the Inovelli switches answered `NOT_SUPPORTED`.
2. z2m's normal `device/unbind` needs the target to be a known device. So, with z2m
   stopped and `database.db` backed up, **add a placeholder device record** with the
   reversed IEEE (type `EndDevice`, an unused `nwkAddr` such as 65520,
   `interviewCompleted: true`, endpoint 1 listing every cluster to unbind in both
   `inClusterList` and `outClusterList`). `database.db` is one JSON object per line and
   also holds `Group` lines that have no `ieeeAddr`.
3. Start z2m, then for each device and endpoint:
   `bridge/request/device/unbind`
   `{"from":"<ieee>","from_endpoint":<ep>,"to":"0x24d660fffe725ca4","to_endpoint":1,"clusters":[<ids>],"skip_disable_reporting":true}`.
   With debug logging on, the device's answer shows: `SUCCESS`, or
   `no entry on device` (`NO_ENTRY`: the record was database-only). Sleeping battery
   devices just fail.
4. Force-remove the placeholder (`device/remove` with `force`; this also clears its HA
   discovery entities). Then, with z2m stopped, strip any remaining reversed bind
   records from `database.db`, so they don't trigger the wrong-record deletion bug (§10)
   on later unbinds.
5. Bind bulbs that had no coordinator binding: `device/bind`
   `{"from":"<bulb>","from_endpoint":11,"to":"Coordinator","clusters":["genOnOff","genLevelCtrl"]}`
   ("Failed to setup reporting for 'Coordinator/1'" afterwards is harmless).

Before any direct `database.db` edit, see the systemd-run note in §11 OPEN 2 (transient
units have no coreutils on `PATH`; a backup step silently failed once because of it).

**Leftovers here:** the Kitchen Overhead VZM32 refused one unbind (illuminance,
`NOT_AUTHORIZED`), so its illuminance reporting was slowed instead (§11). Eight IAS
door/contact sensors (TS0203, TS0215A) have the reversed IEEE cached as `iasCieAddr`.
Their events still arrive; a Reconfigure while each is awake would rewrite it.

---

## 8. Post-transition verification checklist

- [ ] `journalctl -u zigbee2mqtt`: `ember` handshake OK, **0** `RESET_SOFTWARE` /
      `NCP_FATAL` / `ACK_TIMEOUT`, `NRestarts` stable.
- [ ] Live coordinator IEEE == backup IEEE (reversed). (`bridge/info`)
- [ ] **Coordinator trust-center address == coordinator IEEE** (§7 read). Existing
      devices working does NOT prove this.
- [ ] **Pair a brand-new Zigbee 3.0 device both ways:** permit-join "Coordinator"
      *and* "All". It must configure and still be joined after a couple of minutes.
- [ ] `ADDRESS_CONFLICT` count over 60s == **0**.
- [ ] Device count reporting ≈ full inventory; 0 delivery failures over ~10 min.
- [ ] Scene/config buttons trigger their HA automations (Inovelli).
- [ ] Sensor/local load changes sync to HA (e.g. Kitchen mmwave → `light` state).
- [ ] gatus z2m `:8080` endpoint green; ntfy RESOLVED received if it had fired.
- [ ] No bindings to the coordinator's **byte-reversed** IEEE (§7a):
      `grep -a -c <reversed ieee> database.db` == 0, and no `NWK_addr_req` floods for it.
- [ ] Commit the repo change; run `z2m-foldin aida` if devices/groups drifted.

---

## 9. Handy runtime controls (via HA `mqtt.publish` or `mosquitto_pub`)

- Permit join (pairing): `zigbee2mqtt/bridge/request/permit_join` `{"time":254}`
  (close with `{"time":0}`). Restrict to one parent with `"device"`: e.g.
  `{"time":254,"device":"Coordinator"}`, or a router's friendly name / IEEE.
- Force-remove a dead device: `zigbee2mqtt/bridge/request/device/remove`
  `{"id":"<ieee or name>","force":true}` (also drops its HA discovery entities).
- Reconfigure a device: `zigbee2mqtt/bridge/request/device/configure` `{"id":"<name>"}`.
- Re-interview: `zigbee2mqtt/bridge/request/device/interview` `{"id":"<name>"}`.
- Toggle debug at runtime (no restart): `zigbee2mqtt/bridge/request/options`
  `{"options":{"advanced":{"log_level":"debug"}}}` (set back to `"info"`).
- Bridge topics of note: `bridge/info`, `bridge/devices` (retained), `bridge/health`,
  `bridge/response/*`. `bridge/devices` can lag after bind/unbind. For bindings,
  `/var/lib/zigbee2mqtt/database.db` (`grep -a '"ieeeAddr":"<ieee>"' | jq`) is fresher.
- **Open several specific routers at once:** send one targeted permit-join per router.
  Each stays open for its own timer (z2m doesn't close the previous one), while the
  coordinator stays closed. The router should ack with `PERMIT_JOINING_RESPONSE,
  status=SUCCESS` (debug log). Close everything with `{"time":0}`.
- **Which router a device joined through:** at debug level, `ezspTrustCenterJoinHandler:
  … status=STANDARD_SECURITY_UNSECURED_JOIN … parentOfNewNodeId=<nwk>`. Map the
  `<nwk>` to a device with the network map.
- **Network map** (neighbor tables of every router, about 1–2 min, no downtime):
  publish `zigbee2mqtt/bridge/request/networkmap` `{"type":"raw","routes":false}` and
  read `bridge/response/networkmap`. In `links[]`, `source` is the neighbor heard by
  `target`; `relationship` 1 = child. To find routers the coordinator can't hear, look
  for routers that list the coordinator while the coordinator's (max 26-entry) table
  doesn't list them. Expect the weakest routers to be missing when the table is full.
- **Read an attribute directly** (bypasses reporting):
  `zigbee2mqtt/<name>/get` `{"occupancy":""}`. A good way to tell "sensor not
  detecting" from "sensor not reporting".
- **Radio (NCP) counters:** herdsman logs `[NCP COUNTERS] a,b,c,…` **hourly at info**
  (read-and-clear; a z2m restart resets the window). Column *n* = `EmberCounterType`
  *n*: 16 JOIN_INDICATION, 21 NWK_FRAME_COUNTER_FAILURE, 25 **NWK_DECRYPTION_FAILURE**,
  26 APS_DECRYPTION_FAILURE, 40 ADDRESS_CONFLICT_SENT. Table:
  ```bash
  ssh aida 'journalctl -u zigbee2mqtt --since -1d -o cat | sed -E "s/\x1b\[[0-9;]*m//g" \
    | grep -F "[NCP COUNTERS]" | sed -E "s/^\[([0-9-]+ [0-9:]+)\].*\] /\1,/" \
    | awk -F, "{print \$1, \"JOIN=\"\$18, \"NWKdec=\"\$27, \"APSdec=\"\$28}"'
  ```

---

## 10. Known-bug cheat-sheet

- **`DatabaseEntry with ID 'N' does not exist`** when re-pairing a recently-removed
  device (z2m/herdsman #14135/#20670, "undelete not concurrency-safe"): the device
  joins but the interview fails and it's dropped, repeatedly. Not firmware. Options:
  z2m version bump, or route the device another way (e.g. the Shelly Dimmer G4 was
  moved to **Wi-Fi** via the HA Shelly integration and its Zigbee radio disabled).
- **IEEE clone no-op on EFR32** — §4 (#32477).
- **Trust-center address stuck at the factory IEEE** after a late IEEE fix: new
  devices silently fail to join, or join then leave ~20–40 s later — §7.
- **`bellows leave` CLI crash on EZSP v13** (`'sl_Status' object is not
  subscriptable`, bellows 0.49.1) — use the library script in §7.
- **Re-pairing through the previous parent fails** (seen 2026-10-04 with Third
  Reality Gen3 plugs): after a factory reset, a device won't rejoin through the router
  it was last joined through. Nothing reaches the coordinator; it blinks until its
  3-minute window ends. "All" always includes that router, so it fails. Pair via the
  coordinator or another router. Whether it's the plug or the router is still open
  (§11).
- **z2m unbind side effects** (z2m 2.14 / herdsman 10.9.1): unbinding
  `source → target` `genOnOff` also removes the **target's** `genOnOff → coordinator`
  reporting binding. And herdsman can delete the wrong database entries when a device
  has older unresolvable bind records. z2m still logs "Successfully unbound" (only
  debug shows the ZDO status). **Reconfigure both devices afterwards.**
- **SNZB-06P24 can silently stop sending occupancy reports** while illuminance keeps
  flowing. Fix: z2m Reconfigure. Diagnose with a direct `get` of `occupancy`.
- **Newer device models raise `NWK_DECRYPTION_FAILURE` by ~270–380 an hour each**
  (Third Reality Gen3 plug, SONOFF S61; older models joined the same way stay at 0).
  The frames are valid on the air (sniffer, 2026-11-05). Cutting broadcast traffic by
  ~70% (§7a) lowered them only ~29%. Functionally harmless — §11 OPEN 2.
- **RESET_SOFTWARE / ACK-timeout on network-attached coordinators** — widely reported
  for serial-over-TCP; strongly favors going **USB**.

---

## 11. Current state & open follow-ups (as of 2026-10-04 ~16:10; updated 2026-11-06)

### Network
- USB Sonoff MG24 coordinator, IEEE `a4:5c:72:fe:ff:60:d6:24`, PAN `0x1a62`,
  channel 11. Trust-center address fixed 2026-10-03 22:47 (§7) and re-verified with
  bellows.
- 46 devices reporting, 0 `ADDRESS_CONFLICT`, z2m log level `info`.
- The network uses z2m's built-in PAN `0x1a62` and ext PAN `dd…dd` (both visible
  over the air anyway). A neighboring network on the same
  values and channel would look like part of this one over the air. Setting explicit
  network parameters means a new network and re-pairing everything. Separate project
  (details in private notes).
- Traffic (sniffer): ~33,000 frames an hour after the 2026-11-05 cleanup (§7a), down
  from ~112,000; no reversed-IEEE address lookups seen in a 5-minute sample.
- Backups on aida (`/var/lib/zigbee2mqtt/`): `backup-pre-tc-diag-20261003-223957/`,
  `backup-pre-tc-fix-20261003-224354/`, and `backup-pre-tc-fix2-20261003-224646/`
  (just before the §7 fix); `backup-revbind-20261105-230129/` (with the §7a placeholder
  added) and `backup-pre-revbind-strip-20261105-232934/` (before the final §7a database
  strip). No SLZB-era backup survives.

### Devices touched
- **Third Reality Smart Plug Gen3 (`3RSP02064Z`, fw 1.00.47) 4-pack: retired; none
  left in z2m.**
  - **#2 `0x4ce175537eba0000`** (was "Play Room Lamp"): force-removed 2026-11-05
    (unplugged since 2026-10-16).
  - **#1 `0x4ce1755369fc0000`**: force-removed 2026-11-05 (unplugged since
    2026-10-04).
  - **#3 `0x4ce175b487160000`, #4 `0x4ce1755372010000`**: removed 2026-10-04
    (diagnostics only).
  - All four have been joined before, so if one is ever re-paired, use the
    coordinator or a specific router, not "All". To tell them apart, match the IEEE on
    the label (…7eba / …69fc / …b48716 / …7201).
- **SONOFF S61SZBTPB `0xa4c1381f2ed8ffff`** (bought as a Gen3 replacement, 2026-10-16):
  paired (joined via "All", parent coordinator, so brand-new devices pair fine with
  "All"). **Deployed 2026-11-05 as "Play Room Lamp"** (renamed with
  `homeassistant_rename`, so it took over the `*.play_room_lamp*` entity ids) and set
  to power-on behavior `previous`, matching plug #2. On/off and power reporting work (0 W off, ~8.4 W with the lamp,
  ~121.7 V; current reads 0 A at small loads). "Failed to configure" is cosmetic
  (`genOnOff` reporting `UNSUPPORTED_ATTRIBUTE`). Adds NWK decryption failures like the
  Gen3 plugs (OPEN 2).
- **Dining Room Night Light (`3RSNL02043Z`):** removed and re-paired via the
  coordinator 2026-10-16 21:00 (new short address 2747), renamed back; HA entities
  unchanged.
- **Play Room Light Switch (ZBMINIR2) → Play Room Lamp:** a Zigbee binding can't do
  this. The ZBMINIR2 only sends commands (`toggle`) to bound devices in
  detach-relay mode. In normal mode it just reports state. So the HA automation
  `automation.play_room_lamp_follows_play_room_light_switch` (queued; mirrors on/off,
  re-syncs after restarts) does it until the ZBMINIR2 is replaced. It targets
  `switch.play_room_lamp`, which is the S61 since 2026-11-05. The test binding
  was removed. Its stale database bind records (to plug #2 and to the reversed IEEE)
  were cleared 2026-11-05 (§7a).
- **Laundry Room Presence Sensor (SNZB-06P24):** occupancy reports had silently
  stopped (illuminance still flowing); fixed by Reconfigure 2026-10-04 (§10).
- **Kitchen Overhead Lights (Inovelli VZM32-SN, fw 1.02):** still holds an illuminance
  binding to the reversed IEEE (unbind refused, `NOT_AUTHORIZED`). Its illuminance
  reporting was slowed 2026-11-05 to min 600 s / max 3600 s / change 500 (was
  10 s / 3600 s / 5), so it triggers a lookup a few times an hour instead of ~640.
  Nothing in HA uses that sensor. **A z2m Reconfigure of this switch restores the old
  reporting**; re-apply with `bridge/request/device/reporting/configure`
  `{"id":"Kitchen Overhead Lights","endpoint":1,"cluster":"msIlluminanceMeasurement","attribute":"measuredValue","minimum_report_interval":600,"maximum_report_interval":3600,"reportable_change":500}`.
- **Bulbs bound to the coordinator 2026-11-05:** Living room overhead lights 1–5 (Hue),
  Living room floor lamp, Kitchen Counter Lights (`genOnOff` + `genLevelCtrl`). Before,
  their only reporting binding pointed at the reversed IEEE, so they never pushed state
  to z2m (§7a).

### What "All" pairing actually does: test results (2026-10-04)
Joiner: plug #1 (and #3, #4 for A and B). Each trial opened permit-join on only the
listed routers (coordinator closed unless listed), with z2m at debug level.
"Previous parent" = the router the plug was joined through just before the factory
reset.

| Trial | Open | Previous parent open? | Result |
|---|---|---|---|
| A | plug #2 | n/a (new plug) | ✅ |
| B | Aqara T2 (joined Sep 25, during the wrong-TC window) | n/a (new plug) | ✅ |
| C | Play Room Light Switch (ZBMINIR2, joined before the migration) | no | ✅ |
| D | Living room overhead light 1 (Hue) | no | ✅ |
| "All" ×2 | everything | **yes** | ❌ |
| Round 1 | 7 Hue bulbs | **yes** (light 2) | ❌ |
| Round 2 | Lower/Upper Stair, Foyer | no | ✅ (via Foyer) |
| Round 3 | lights 3, 4, 5 | **yes** (light 5) | ❌ |
| Round 4 | light 4 | no | ✅ |
| Round 5 | light 3 | **yes** (light 3) | ❌ |
| Round 6 | light 4 | no | ✅ |
| **Round 7** (predicted ✅) | light 3 | no (prev = light 4) | **✅** |
| **Round 8** (predicted ❌) | light 3 | **yes** (prev = light 3) | **❌** |
| Soft rejoins ×3 (right after a z2m remove) | various | no each time | ✅ |

**Finding:** after a factory reset, the plug won't rejoin through its previous parent.
Nothing reaches the coordinator; it blinks until its 3-minute window ends. Through any
other router it joins in seconds. "All" always includes the previous parent, so
re-pairing a previously joined device with "All" fails.

What this rules out:
- the coordinator's trust-center handling (key handshake succeeds every time);
- routers being unable to forward joins: each tested router forwarded someone's
  join, and a factory reset produces about 25–40 `status=DEVICE_LEFT` notifications,
  meaning that many routers reached the coordinator;
- the "lost key seed" idea: herdsman restored `hashed_tclk` on Sep 24.

Seen with Hue parents directly (light 3, twice), and inferred for light 2, light 5,
and the ZBMINIR2.

### OPEN 1 (resolved enough): is the re-join refusal the plug's firmware, router behavior, or leftover network state?
Hypotheses:
- **J (plug-side):** the Gen3 plug's firmware (1.00.47) mishandles re-joining its last
  parent after a reset. For example, it tries a stale rejoin, or insists on that
  parent.
- **P (router-side, generic):** routers keep a stale entry for a departed child and
  mishandle its next association. That would hit any device, regardless of this
  network's history.
- **S (leftover network state):** something from the migration or trust-center saga
  in the routers or coordinator causes it.

What we already know: each failure involves exactly one device and its previous
parent, and the same router accepts the same plug once it's no longer the previous
parent (rounds 5 vs 7). That points to state created by the join and leave itself,
not damage spread across the network. But only one joiner model was tested, and the
directly verified parent was a Hue bulb.

Experiments, cheapest first. Each takes about 5–10 minutes, uses the targeted
permit-join and debug logging from §9, and has the plug start from "joined via X":

1. **Post-fix router as the previous parent (tests S).** Join plug #1 via **plug #2**,
   which joined only after the §7 fix, then factory-reset it with only plug #2 open.
   Plug #2 has no migration history, so a **failure rules out S**. A success points
   at S, and the next step is comparing routers by when they joined.
2. **Coordinator as the previous parent (tests J vs P, with visibility).** Join plug #1
   via "Coordinator" only, then reset it with only the coordinator open. The
   coordinator is a different stack (Ember) and was re-formed clean on 2026-10-03.
   Because it's the parent, its debug log shows what the plug sends, even on failure.
   **Failure:** points to J (or generic Zigbee behavior shared by every stack).
   **Success:** points to P (the router stacks' handling).
3. **Power-cycle the previous parent (characterizes P).** After a refused attempt,
   air-gap or power-cycle that router, then retry with only it open. **Success:** the
   stale state lives in the router's memory. Also try **waiting 30–60 minutes**
   instead, to see whether it ages out.
4. **Different joiner (separates J from P).** Repeat "join via router X, then reset
   with only X open" using a device from another vendor (a new cheap router device,
   or a low-stakes existing one with no custom bindings; factory-reset it without
   removing it from z2m, so it keeps its name). **Failure:** P (affects everything).
   **Success:** J (specific to these plugs). A Third Reality *bulb* as joiner would
   separate "this plug model" from "this vendor's stack".
5. **Sniffer (definitive).** A spare 802.15.4 stick running sniffer firmware, with
   Wireshark on channel 11, the network key (from `coordinator_backup.json` on aida), and
   ZigBeeAlliance09. It shows whether the plug sends an association request or a
   rejoin to its old parent, how the parent answers, and whether it ever forwards
   the join.

**Reading the results:** 1 fails, 2 fails, 4 succeeds → **plug firmware** (report to
Third Reality, and just pair via the coordinator). 1 fails, 2 succeeds, 4 fails →
**generic router behavior** (same workaround; power-cycling the old parent may clear
it). 1 succeeds → **leftover network state** (dig into which routers refuse, by when
they joined).

**Results (2026-10-04; tests 3–5 not run, by choice):**

| # | Previous parent | Setup join | Factory reset, only the previous parent open |
|---|---|---|---|
| 1 | plug #2 (joined after the §7 fix; Third Reality firmware) | ✅ 15:24:11, parent 44651 | ❌ left (39 leave notices), no rejoin in the ~2.5 min left |
| 2 | coordinator (Ember; re-formed clean 2026-10-03) | ✅ 15:57:16, parent 0 | ✅ **but 91 s after the leave** (15:59:07 → 16:00:38). Normal joins take 2–20 s |

**Conclusion (good enough to stop):**
- **Leftover network state is ruled out.** A router whose whole history is post-fix
  refused its former child just like the older Hue bulbs.
- **The plug *can* rejoin its previous parent.** The coordinator accepted it, after a
  91-second delay. During that delay the coordinator's software saw nothing from the
  plug.
- **Most consistent explanation: router-side behavior, with a stack-dependent
  timeout.** The old parent keeps a stale entry for the departed device and won't
  accept its fresh association until that clears. Ember on the coordinator clears it
  in about 90 s. The Hue (fw 1.163.1) and Third Reality plug (fw 1.00.47) routers
  hold it longer than a pairing window. The plug-side alternative (it spends ~90 s
  trying its old parent in some way the parent ignores, then falls back) isn't fully
  excluded. Test 3 (power-cycle the old parent) or the sniffer would settle it.
- **Practical upshot:** none of this is damage. **Re-pair previously joined devices
  via the coordinator** (or a router that wasn't their last parent), or expect "All"
  to fail for them. Possible extra workarounds, untested: power-cycle the old parent
  first, or wait well past a pairing window before re-pairing.

### OPEN 2: undecryptable frames from newer device models (since 2026-10-03)
**Status (2026-11-06): the frames are valid. The sniffer decrypted every frame on the
air, and the coordinator's failures line up with broadcast-relay bursts, not with frames
from the newer devices. Most of those bursts turned out to be self-inflicted (§7a);
removing them cut traffic ~70% but failures only ~29% (406 → 289 an hour with the S61
powered). Powering a newer device still adds them. Cause of the remainder unknown;
functionally harmless.**

**The symptom.** `NWK_DECRYPTION_FAILURE` (NCP counter 25, §9) was 0–5 a day for weeks:
through the Sep 24 migration, the Sep 25 breaker cycle, and up to Oct 2. From Oct 3 it
ran at about 230–540 an hour, and about 860–1,170 an hour during the 2026-10-04 join
and leave testing. The NCP drops these frames internally; no callback says which
device sent them, so attribution is by elimination over hourly counter rows.

**Attribution by elimination (hourly rows):**

| Powered (beyond the ~46 older devices) | NWK decryption failures / hour |
|---|---|
| none (weeks before Oct 3) | ~0 |
| Third Reality Gen3 plug ×1 (#2, overnight) | ~230–415, average ~375 |
| Gen3 plugs ×2 | 723 |
| Gen3 plugs ×4 | up to 1,166 |
| Gen3 plug #1 unplugged, #2 left | 421 |
| **SONOFF S61SZBTPB** only (2026-10-16 16:09–17:09) | **274** |
| S61 unplugged ~40 s into the hour | 14 |
| nothing joined after Oct 3 | 0, 0 |

- **Every device that joined on or after 2026-10-03 adds roughly 270–380 an hour,
  regardless of vendor:** Third Reality Gen3 plug (fw 1.00.47 and 1.00.63, stack
  revision R22) and SONOFF S61 (fw 1.0.3, R21). Newer Gen3 firmware doesn't fix it.
  The ~46 devices that joined earlier add about 0.
- **Ruled out:** the MG24 dongle itself (9 clean days before Oct 3); z2m, herdsman,
  or NCP firmware changes (2.14.0 / 10.9.1 / EmberZNet 7.4.5 throughout); Zigbee R23;
  the coordinator's own address mapping (the 2026-10-04 network map listed the failing
  plugs by IEEE in the coordinator's neighbor table).

**Network state vs. device model: settled by re-pairing an OLD model (2026-10-16).**
Joiner: Dining Room Night Light (Third Reality `3RSNL02043Z`, `0xb40ecfd7ff8b0000`,
originally joined long before the migration), with every post-Oct 3 device unplugged.

| Test | Coordinator state for the device | Short address | Failures |
|---|---|---|---|
| a. factory reset + permit-join "Coordinator" only, no z2m remove (18:29) | possibly remembered | 52335 (same as before) | 0, 0 (two full hours) |
| b. z2m normal remove (leave acked) → unplugged 18 min → permit-join "Coordinator" only (21:00:57) | forgotten | **2747 (new)** | 0 (first 8 min), **0** (first full hour, 81 messages) |

Test b makes the night light exactly as new to the coordinator as the S61 or the Gen3
plugs, yet it stays clean. So **joining after the Oct 3 re-restore doesn't cause it;
the newer device models do.** (Test a alone wasn't conclusive: getting the same short
address back shows the coordinator still remembered the device.)

Note: a normal z2m remove also drops the friendly name. Rename back with
`bridge/request/device/rename` `{"from":"<ieee>","to":"<name>","homeassistant_rename":true}`
and HA re-creates the same entity ids.

**Sniffer results (2026-11-05).** Setup and reruns: see "Sniffer on aida" below.

1. **70-minute capture with the S61 powered** (18:43–19:53, after a re-join with its
   old short address `0xE05F`). The coordinator counted **307** failures in the
   18:54–19:54 row. The sniffer captured **130,622** frames, and **every secured frame
   decrypted** with the network key (MIC verified). The S61's frames match an older
   Hue bulb's byte for byte: security control `0x28`, network key, key sequence 0,
   extended nonce present with its correct IEEE, frame counters strictly increasing
   with no reuse. That rules out a stale key sequence, a missing nonce, unknown
   senders, and a neighbor network (the only foreign PAN on channel 11, `0xad30`, is a
   **Thread** network). Telink silicon isn't the factor either (the S61 is Telink, but
   so are the clean SNZB-06P24 and Gledopto devices), and its broadcast relaying is
   normal (~4–5 copies per broadcast, ~355 ms median relay delay like other Telink
   routers).
2. **Per-second correlation** (21:04–21:16; z2m stopped, coordinator counters polled
   every 0.5 s with bellows `readCounters`, sniffer running). 44 failures in
   12 minutes, 0 undecryptable frames among 30,874 captured.
   - About half the failures came within ~1.3 s of a **route-request flood** starting
     (every flood was followed by 1–3); the rest came in pairs ~1.5 s apart every
     ~30–60 s (a periodic broadcast and its relays). Failure windows averaged
     ~88 frames/s against ~38 overall.
   - Against busy-matched random windows, the S61 is **not** over-represented: its own
     frames are in 2.5% of failure windows (3.8% control), any S61 frame 77.5%
     (81.8%), its frame-counter gaps (frames the sniffer didn't capture) 87.5%
     (90.1%). No single device is present in every failure window.

3. **Where the bursts came from: bindings to the byte-reversed coordinator IEEE**
   (§7a). ~990 address lookups an hour (all five Inovelli switches, Hue light 5, the
   ZBMINIR2s, a few battery devices) became ~80,000 relayed frames an hour, ~70% of
   all traffic. The coordinator's many-to-one route request (~63 an hour) is the other
   recurring flood; that one is normal.
4. **After the cleanup** (2026-11-05 23:00–23:30; the S61 stayed powered):

   | | Before | After |
   |---|---|---|
   | All frames (sniffer) | ~112,000/h | ~33,000/h |
   | Reversed-IEEE lookup frames | ~80,000/h | 0 in a 5-minute sample |
   | NWK decryption failures (hourly row) | 406 (21:15–22:15) | **289** (23:29–00:29) |

**Interpretation (not proven):** the coordinator drops some valid frames during
broadcast bursts and counts them as decryption failures. Removing most of the burst
load helped, but much less than proportionally, so burst load isn't the whole story.
Each newer router still adds failures, and why these models do and older ones (like
the night light) don't isn't explained by anything visible on the air. Possible next
levers, if it ever matters: newer EmberZNet firmware for the MG24 (same family, no
re-pair), or a report to zigbee-herdsman/EmberZNet with these numbers (share summaries
only; the captures are decryptable with this network's key).

**Sniffer on aida** (all in `~/nrfsniff`; nothing in the repo; captures and keys stay
private):
- Hardware: Nordic nRF52840 dongle (PCA10059) on aida's USB, flashed with Nordic's
  nRF Sniffer for 802.15.4 firmware (v0.8.0, `nrf802154_sniffer_nrf52840dongle.hex`).
  It shows up as `/dev/serial/by-id/usb-Nordic_Semiconductor_ASA_nRF_802154_Sniffer_…`.
  The Open DFU bootloader is untouched (to reflash, press the side RESET button: the
  device reappears as "Open DFU Bootloader").
- Flashing: nixpkgs `nrfutil` with the `nrfutil-nrf5sdk-tools` extension, overriding
  `segger-jlink-headless = emptyDirectory` (the dongle doesn't need J-Link, so this
  avoids the J-Link license). Import nixpkgs with `overlays = [ ]` on aida, or
  `<nixos-config>` lookup fails. Then
  `nrfutil nrf5sdk-tools pkg generate --hw-version 52 --sd-req 0x00
  --application-version 1 --application <hex> sniffer.zip` and
  `nrfutil nrf5sdk-tools dfu usb-serial -pkg sniffer.zip -p <bootloader port>`.
- Capture: Nordic's `nrf802154_sniffer.py` (Wireshark extcap script) with a Python that
  has `pyserial`, run headless into a pcap file, e.g.
  `sudo systemd-run --unit nrfsniff-capture --property RuntimeMaxSec=3600
  --property KillSignal=SIGINT <python> nrf802154_sniffer.py --capture
  --extcap-interface <sniffer port> --channel 11 --fifo <out.pcap>`.
  Timestamps are pinned to host time at the first frame, so they line up with host
  logs to within tens of milliseconds.
- Decryption: `tshark` (nixpkgs `wireshark-cli`) with `~/.config/wireshark/zigbee_pc_keys`
  (mode 600) holding the network key from `coordinator_backup.json` and
  ZigBeeAlliance09. Useful fields: `zbee_sec.encrypted_payload` (present only when
  decryption failed), `zbee.sec.src64`, `zbee.sec.counter`, `zbee.sec.key_seqno`,
  `zbee.sec.ext_nonce`. tshark exits with an error on an unknown field name, so check
  names with `tshark -G fields` and don't trust an empty result. Filter key fields out
  of `-V` dumps.
- Per-second counters: stop z2m, connect with bellows (`bellows.cli.util.setup(port,
  115200)` → `initialize_network()`), poll `readCounters` (non-clearing), then restart
  z2m. Run it as a `systemd-run` wrapper that restarts z2m unconditionally. Transient
  units have no coreutils on `PATH` (`timeout`, `date`, `mkdir` and `cp` all fail), so
  set `PATH=/run/current-system/sw/bin` in the wrapper and make each later step check
  that the backup step succeeded. z2m comes back "resumed".
