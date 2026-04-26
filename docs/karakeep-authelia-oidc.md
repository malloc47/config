# Karakeep ↔ Authelia OIDC Integration (aida)

## Approach

Configure Authelia as an OIDC provider, register Karakeep as a confidential
client, point Karakeep at the Authelia issuer, and drop Caddy's `forward_auth`
from the `bookmarks.*` vhost. The auth boundary moves from the proxy into
Karakeep itself.

## Trade-offs

- **Per-app OIDC client** instead of one Caddy gate: more boilerplate per
  service, but native API/mobile/extension support — Karakeep's mobile app and
  browser extension can do real OAuth instead of fighting Authelia cookies.
- **`DISABLE_PASSWORD_AUTH`** is left **off** initially. If Authelia goes down
  with it on, you'd be locked out of Karakeep. Flip it once OIDC is verified.
- **`OAUTH_ALLOW_DANGEROUS_EMAIL_ACCOUNT_LINKING=true`**: required if a local
  Karakeep account already has the same email as your Authelia user, otherwise
  OIDC creates a fresh empty account.
- **Authelia claims-policy escape hatch**: per Authelia's Karakeep doc,
  Karakeep's OIDC implementation is incomplete and needs `id_token: ['email']`
  injected via a claims policy.
- **Public repo / secrets**: client_secret PBKDF2 digest is one-way, but it's
  still routed through agenix + Authelia's template filter so nothing
  auth-related lands in the public repo. The NixOS Authelia module enables
  `X_AUTHELIA_CONFIG_FILTERS=template` automatically when
  `oidcIssuerPrivateKeyFile` is set, so `{{ secret "..." }}` interpolation just
  works.
- **Healthcheck**: Caddy previously carved out `/api/healthcheck` to bypass
  Authelia. With the gate gone, no carve-out is needed.

## Changes Already Made

### `hosts/aida.nix`

- **Authelia** (`services.authelia.instances.main`): added
  `identity_providers.oidc` with the `karakeep` client, claims-policy escape
  hatch (`id_token: [email]`), PKCE+S256, and templated `client_secret`. Added
  `oidcHmacSecretFile` + `oidcIssuerPrivateKeyFile` to `secrets` (this
  auto-enables Authelia's template filter).
- **Karakeep** (`services.karakeep`): added `environmentFile` (for
  `OAUTH_CLIENT_SECRET`) plus `NEXTAUTH_URL`, `OAUTH_WELLKNOWN_URL`,
  `OAUTH_CLIENT_ID`, `OAUTH_PROVIDER_NAME`,
  `OAUTH_ALLOW_DANGEROUS_EMAIL_ACCOUNT_LINKING=true`,
  `DISABLE_PASSWORD_AUTH=false`.
- **Caddy `bookmarks.home.malloc47.com`**: collapsed to a plain
  `reverse_proxy http://127.0.0.1:3001`. Healthcheck carve-out removed (no
  longer needed).
- **agenix**: four new `age.secrets` entries — `authelia-oidc-hmac`,
  `authelia-oidc-private-key`, `authelia-karakeep-secret-digest` (owner
  `authelia-main`), and `karakeep-oauth-env` (owner `karakeep`).

### `secrets/secrets.nix`

Added four matching `*.age` entries, each encrypted to `aida` + `malloc47`.

## Outstanding Steps

Run from the repo root with your agenix key loaded:

```bash
# 1. HMAC secret (any random string ≥64 chars)
openssl rand -hex 64 | (cd secrets && EDITOR='tee' agenix -e authelia-oidc-hmac.age)

# 2. OIDC issuer private key (RSA 4096 PEM)
openssl genrsa 4096 | (cd secrets && EDITOR='tee' agenix -e authelia-oidc-private-key.age)

# 3. Generate plaintext + PBKDF2 digest for Karakeep's client_secret
nix run nixpkgs#authelia -- crypto hash generate pbkdf2 \
    --variant sha512 --random --random.length 72 --random.charset rfc3986
# - Copy the printed "Random Password" → goes into karakeep-oauth-env.age (step 4)
# - Copy the printed "Digest"           → goes into authelia-karakeep-secret-digest.age (this step)
(cd secrets && agenix -e authelia-karakeep-secret-digest.age)
# Paste the Digest line as the file contents.

# 4. Karakeep env file with the plaintext secret
(cd secrets && agenix -e karakeep-oauth-env.age)
# Contents:
# OAUTH_CLIENT_SECRET=<plaintext from step 3>
```

Then stage the new secret files (agenix won't load files unknown to git) and
rebuild aida:

```bash
git add secrets/authelia-oidc-hmac.age \
        secrets/authelia-oidc-private-key.age \
        secrets/authelia-karakeep-secret-digest.age \
        secrets/karakeep-oauth-env.age
# rebuild aida via your usual deploy path
```

## Verification Checklist

1. `https://auth.home.malloc47.com/.well-known/openid-configuration` returns
   JSON (issuer is up).
2. `https://bookmarks.home.malloc47.com` is reachable without an Authelia gate
   and the "Sign in with Authelia" button appears.
3. Clicking it completes the login and lands you back in Karakeep,
   authenticated as the matching email.
4. Once verified, flip `DISABLE_PASSWORD_AUTH = "true"` in `hosts/aida.nix`
   for SSO-only login, then rebuild.

## References

- Authelia ↔ Karakeep integration: <https://www.authelia.com/integration/openid-connect/clients/karakeep/>
- Karakeep auth env vars: <https://docs.karakeep.app/configuration#authentication--signup>
