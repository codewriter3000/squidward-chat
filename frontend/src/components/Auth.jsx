import { createSignal } from 'solid-js';

function Auth(props) {
  const [isLogin, setIsLogin] = createSignal(true);
  const [username, setUsername] = createSignal('');
  const [password, setPassword] = createSignal('');
  const [error, setError] = createSignal('');
  const [loading, setLoading] = createSignal(false);

  const handleSubmit = async (e) => {
    e.preventDefault();
    setError('');
    setLoading(true);

    const endpoint = isLogin() ? '/api/login' : '/api/register';
    
    try {
      const response = await fetch(endpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          username: username(),
          password: password()
        })
      });

      const data = await response.json();

      if (data.success) {
        if (isLogin()) {
          props.onLogin(data.token, data.username);
        } else {
          setError('Registration successful! Please login.');
          setIsLogin(true);
          setPassword('');
        }
      } else {
        setError(data.message || 'An error occurred');
      }
    } catch (err) {
      setError('Connection error. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div class="auth-container">
      <div class="auth-box">
        <h1>Squidward Chat</h1>
        <h2>{isLogin() ? 'Login' : 'Register'}</h2>
        
        <form onSubmit={handleSubmit}>
          <input
            type="text"
            placeholder="Username"
            value={username()}
            onInput={(e) => setUsername(e.target.value)}
            required
          />
          <input
            type="password"
            placeholder="Password"
            value={password()}
            onInput={(e) => setPassword(e.target.value)}
            required
          />
          
          {error() && <div class="error">{error()}</div>}
          
          <button type="submit" disabled={loading()}>
            {loading() ? 'Please wait...' : (isLogin() ? 'Login' : 'Register')}
          </button>
        </form>
        
        <p class="toggle">
          {isLogin() ? "Don't have an account? " : 'Already have an account? '}
          <a onClick={() => {
            setIsLogin(!isLogin());
            setError('');
          }}>
            {isLogin() ? 'Register' : 'Login'}
          </a>
        </p>
      </div>
    </div>
  );
}

export default Auth;
