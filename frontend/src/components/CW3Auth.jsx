import { createSignal } from 'solid-js';

const OAUTH_BASE = import.meta.env.VITE_OAUTH_BASE_URL || 'https://squidwardchat.amicharskilabs.com';
const CLIENT_ID = import.meta.env.VITE_OAUTH_CLIENT_ID || 'squidward-chat';
const REDIRECT_URI = (import.meta.env.VITE_OAUTH_REDIRECT_URI || window.location.origin).replace(/\/$/, '');

function CW3Auth(props) {
  const [loading, setLoading] = createSignal(false);

  const handleOAuth = () => {
    setLoading(true);

    // Generate a random state value for CSRF protection
    const state = Array.from(crypto.getRandomValues(new Uint8Array(16)))
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
    sessionStorage.setItem('oauth_state', state);

    const params = new URLSearchParams({
      response_type: 'code',
      client_id: CLIENT_ID,
      redirect_uri: REDIRECT_URI,
      scope: 'openid profile email',
      state: state
    });

    window.location.href = `${OAUTH_BASE}/oauth/authorize?${params}`;
  };

  return (
    <div class="auth-container">
      <div class="auth-box">
        <h1>Squidward Chat</h1>

        {props.error && <div class="error">{props.error}</div>}

        <button onClick={handleOAuth} disabled={loading()}>
          {loading() ? 'Redirecting...' : 'Login with CW3'}
        </button>
      </div>
    </div>
  );
}

export default CW3Auth;
