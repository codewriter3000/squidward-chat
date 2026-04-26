# OAuth 2.0 / OpenID Connect Integration Guide

This guide describes how a third-party application can integrate with this IAM server as an OAuth 2.0 Authorization Server and OpenID Connect Provider.

---

## Discovery

Fetch server metadata automatically via the standard discovery endpoints:

| Endpoint | Description |
|---|---|
| `GET /.well-known/openid-configuration` | Full OpenID Connect Provider metadata |
| `GET /.well-known/oauth-authorization-server` | OAuth 2.0 Authorization Server metadata |
| `GET /.well-known/jwks.json` | RSA public keys for verifying ID tokens (RS256) |

**Example response from `/.well-known/openid-configuration`:**
```json
{
  "issuer": "http://localhost:8001",
  "authorization_endpoint": "http://localhost:8001/oauth/authorize",
  "token_endpoint": "http://localhost:8001/oauth/token",
  "userinfo_endpoint": "http://localhost:8001/oauth/userinfo",
  "end_session_endpoint": "http://localhost:8001/oauth/logout",
  "jwks_uri": "http://localhost:8001/.well-known/jwks.json",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "client_credentials", "refresh_token"],
  "id_token_signing_alg_values_supported": ["RS256"],
  "scopes_supported": ["openid", "profile", "email", "read", "write", "admin"],
  "token_endpoint_auth_methods_supported": ["client_secret_basic", "client_secret_post", "none"]
}
```

---

## Client Registration

Before your application can use OAuth, an administrator must register it via the Admin API.

**`POST /api/oauth/client`**

```json
{
  "client_name": "My App",
  "redirect_uris": ["https://myapp.example.com/oauth/callback"],
  "grant_types": ["authorization_code", "refresh_token"],
  "scopes": ["openid", "profile", "email"],
  "token_endpoint_auth_method": "client_secret_basic"
}
```

The response includes the assigned `client_id` and `client_secret`. Store these securely.

### Token Endpoint Authentication Methods

| Method | Description |
|---|---|
| `client_secret_basic` | Credentials sent as HTTP Basic Auth header (recommended for confidential clients) |
| `client_secret_post` | Credentials sent as `client_id` and `client_secret` in the POST body |
| `none` | No secret required; for public clients (e.g., SPAs, mobile apps) using the authorization code flow |

---

## Grant Types

### 1. Authorization Code Flow (user-facing apps)

Use this flow when your application acts on behalf of a logged-in user.

#### Step 1 — Redirect the user to the authorization endpoint

```
GET /oauth/authorize
  ?response_type=code
  &client_id=<your_client_id>
  &redirect_uri=https://myapp.example.com/oauth/callback
  &scope=openid profile email
  &state=<random_csrf_token>
```

If the user has no active session, they will be redirected to the login page automatically. After authenticating, they will be redirected back to your `redirect_uri` with an authorization code.

**Optional parameters:**
- `prompt=login` — Force the user to re-authenticate even if a session exists.
- `max_age=0` — Same effect as `prompt=login`.

#### Step 2 — Exchange the code for tokens

```
POST /oauth/token
Content-Type: application/x-www-form-urlencoded
Authorization: Basic <base64(client_id:client_secret)>

grant_type=authorization_code
&code=<authorization_code>
&redirect_uri=https://myapp.example.com/oauth/callback
```

**Response:**
```json
{
  "access_token": "...",
  "token_type": "Bearer",
  "expires_in": 900,
  "refresh_token": "...",
  "id_token": "...",
  "scope": "openid profile email"
}
```

| Field | TTL |
|---|---|
| `access_token` | 15 minutes |
| `refresh_token` | 30 days |
| `id_token` | 15 minutes (JWT, RS256) |
| Authorization code | 5 minutes (single-use) |

---

### 2. Client Credentials Flow (machine-to-machine)

Use this flow when your application needs to call the API on its own behalf, with no user involved.

```
POST /oauth/token
Content-Type: application/x-www-form-urlencoded
Authorization: Basic <base64(client_id:client_secret)>

grant_type=client_credentials
&scope=read write
```

**Response:**
```json
{
  "access_token": "...",
  "token_type": "Bearer",
  "expires_in": 900,
  "scope": "read write"
}
```

No `refresh_token` or `id_token` is issued for this grant.

---

### 3. Refresh Token Flow

Exchange a refresh token for a new access token (and rotated refresh token).

```
POST /oauth/token
Content-Type: application/x-www-form-urlencoded
Authorization: Basic <base64(client_id:client_secret)>

grant_type=refresh_token
&refresh_token=<your_refresh_token>
```

**Response:**
```json
{
  "access_token": "...",
  "token_type": "Bearer",
  "expires_in": 900,
  "refresh_token": "<new_refresh_token>",
  "scope": "..."
}
```

> **Note:** Refresh tokens are rotated on each use. The previous refresh token is invalidated immediately after a successful exchange.

---

## Using Access Tokens

Include the access token as a Bearer token in the `Authorization` header of requests to protected APIs:

```
GET /api/some-resource
Authorization: Bearer <access_token>
```

---

## ID Token

The ID token is a signed JWT (RS256). Its claims include:

| Claim | Description |
|---|---|
| `iss` | Token issuer (server base URL) |
| `sub` | Subject (username) |
| `aud` | Audience (your `client_id`) |
| `iat` | Issued-at (Unix timestamp) |
| `exp` | Expiry (Unix timestamp) |
| `user_id` | Internal user ID |
| `username` | Username |
| `preferred_username` | Username (OIDC standard alias) |
| `email` | User email address |
| `first_name` / `given_name` | First name (if set) |
| `last_name` / `family_name` | Last name (if set) |
| `roles` | Array of role names assigned to the user |
| `permissions` | Array of permission names assigned to the user |

Verify the token signature using the public key from `GET /.well-known/jwks.json`. The key ID (`kid`) in the token header matches the key in the JWKS response.

---

## UserInfo Endpoint

Retrieve the current user's profile using their access token:

```
GET /oauth/userinfo
Authorization: Bearer <access_token>
```

**Response:**
```json
{
  "sub": "john.doe",
  "user_id": "42",
  "username": "john.doe",
  "preferred_username": "john.doe",
  "email": "john.doe@example.com",
  "first_name": "John",
  "last_name": "Doe",
  "roles": ["admin", "editor"],
  "permissions": ["users:read", "users:write"]
}
```

---

## Token Introspection

Your resource server can verify an access or refresh token:

```
POST /oauth/introspect
Content-Type: application/x-www-form-urlencoded
Authorization: Basic <base64(client_id:client_secret)>

token=<token_to_inspect>
```

**Active token response:**
```json
{
  "active": true,
  "client_id": "my-app",
  "username": "john.doe",
  "scope": "openid profile",
  "token_type": "Bearer",
  "exp": 1714150000,
  "iat": 1714149100,
  "sub": "john.doe"
}
```

**Inactive/expired token response:**
```json
{
  "active": false
}
```

---

## Token Revocation

Revoke an access or refresh token (e.g., on user logout):

```
POST /oauth/revoke
Content-Type: application/x-www-form-urlencoded
Authorization: Basic <base64(client_id:client_secret)>

token=<token_to_revoke>
```

Returns `200 OK` regardless of whether the token existed.

---

## Logout

Redirect the user to the logout endpoint to clear their session server-side:

```
GET /oauth/logout?client_id=<your_client_id>&state=<optional_state>
```

The server will clear the session cookie and redirect the user to your application's login page (derived from the registered redirect URI).

---

## Scopes

| Scope | Description |
|---|---|
| `openid` | Required for OpenID Connect flows; enables the ID token |
| `profile` | Access to `username`, `first_name`, `last_name` |
| `email` | Access to the user's email address |
| `read` | Read access to application resources |
| `write` | Write access to application resources |
| `admin` | Administrative access |

Request only the scopes your application needs. The server will reject requests for scopes not assigned to your client.

---

## Error Responses

All token/authorize endpoint errors follow the OAuth 2.0 error format:

```json
{
  "error": "invalid_client"
}
```

Common error codes:

| Code | Meaning |
|---|---|
| `invalid_client` | Client authentication failed |
| `invalid_grant` | Authorization code or refresh token is invalid, expired, or already used |
| `invalid_scope` | Requested scope not permitted for this client |
| `unauthorized_client` | Client is not authorized for the requested grant type |
| `unsupported_grant_type` | The `grant_type` value is not supported |
| `unsupported_response_type` | Only `code` is supported |
