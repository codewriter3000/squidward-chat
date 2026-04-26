import { createSignal, onMount, Show } from 'solid-js';
import CW3Auth from './components/CW3Auth';
import Chat from './components/Chat';

const API_BASE = import.meta.env.VITE_API_BASE_URL || '';

function App() {
  const [token, setToken] = createSignal(localStorage.getItem('token') || null);
  const [username, setUsername] = createSignal(localStorage.getItem('username') || null);
  const [authError, setAuthError] = createSignal('');
  const [loading, setLoading] = createSignal(false);

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
        return;
      }

      setLoading(true);
      try {
        const redirectUri = window.location.origin + '/';
        const response = await fetch(`${API_BASE}/api/oauth/callback`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ code, redirect_uri: redirectUri })
        });
        const data = await response.json();
        if (data.success) {
          localStorage.setItem('token', data.token);
          localStorage.setItem('username', data.username);
          setToken(data.token);
          setUsername(data.username);
        } else {
          setAuthError(data.message || 'OAuth login failed. Please try again.');
        }
      } catch (err) {
        setAuthError('Connection error during OAuth login. Please try again.');
      } finally {
        setLoading(false);
      }
    }
  });

  const handleLogout = () => {
    localStorage.removeItem('token');
    localStorage.removeItem('username');
    setToken(null);
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
          when={token()}
          fallback={<CW3Auth error={authError()} />}
        >
          <Chat token={token()} username={username()} onLogout={handleLogout} />
        </Show>
      </Show>
    </div>
  );
}

export default App;
