import { createSignal, onMount, Show } from 'solid-js';
import CW3Auth from './components/CW3Auth';
import Chat from './components/Chat';

function App() {
  const [username, setUsername] = createSignal(null);
  const [authError, setAuthError] = createSignal('');
  const [loading, setLoading] = createSignal(true);

  onMount(async () => {
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');
    const state = params.get('state');

    if (code && state) {
      const storedState = sessionStorage.getItem('oauth_state');
      sessionStorage.removeItem('oauth_state');
      // Remove the OAuth params from the URL without a page reload
      window.history.replaceState({}, document.title, '/');

      if (state !== storedState) {
        setAuthError('OAuth state mismatch — possible CSRF. Please try again.');
        setLoading(false);
        return;
      }

      try {
        const REDIRECT_URI = (import.meta.env.VITE_OAUTH_REDIRECT_URI || window.location.origin).replace(/\/$/, '');
        const response = await fetch('/api/oauth/callback', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          credentials: 'include',
          body: JSON.stringify({ code, redirect_uri: REDIRECT_URI })
        });
        const data = await response.json();
        if (data.success) {
          setUsername(data.username);
        } else {
          setAuthError(data.message || 'OAuth login failed. Please try again.');
        }
      } catch (err) {
        setAuthError('Connection error during OAuth login. Please try again.');
      } finally {
        setLoading(false);
      }
    } else {
      // No OAuth code — check for an existing valid iam-session cookie
      try {
        const OAUTH_BASE = import.meta.env.VITE_OAUTH_BASE_URL || 'https://cw3admin.amicharskilabs.com';
        const response = await fetch(`${OAUTH_BASE}/api/auth/session`, { credentials: 'include' });
        if (response.ok) {
          const data = await response.json();
          const user = data.username || data.preferred_username || data.sub;
          if (user) {
            setUsername(user);
          }
        }
      } catch (_) {
        // No active session, show login
      } finally {
        setLoading(false);
      }
    }
  });

  const handleLogout = async () => {
    try {
      await fetch('/api/logout', { method: 'POST', credentials: 'include' });
    } catch (_) {}
    setUsername(null);
  };

  return (
    <div class="app">
      <Show when={loading()}>
        <div class="auth-container">
          <div class="auth-box">
            <h1>Squidward Chat</h1>
            <p>Signing in...</p>
          </div>
        </div>
      </Show>
      <Show when={!loading()}>
        <Show
          when={username()}
          fallback={<CW3Auth error={authError()} />}
        >
          <Chat username={username()} onLogout={handleLogout} />
        </Show>
      </Show>
    </div>
  );
}

export default App;
