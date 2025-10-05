import { createSignal, onCleanup, Show } from 'solid-js';
import Auth from './components/Auth';
import Chat from './components/Chat';

function App() {
  const [token, setToken] = createSignal(localStorage.getItem('token') || null);
  const [username, setUsername] = createSignal(localStorage.getItem('username') || null);

  const handleLogin = (newToken, newUsername) => {
    localStorage.setItem('token', newToken);
    localStorage.setItem('username', newUsername);
    setToken(newToken);
    setUsername(newUsername);
  };

  const handleLogout = () => {
    localStorage.removeItem('token');
    localStorage.removeItem('username');
    setToken(null);
    setUsername(null);
  };

  return (
    <div class="app">
      <Show
        when={token()}
        fallback={<Auth onLogin={handleLogin} />}
      >
        <Chat token={token()} username={username()} onLogout={handleLogout} />
      </Show>
    </div>
  );
}

export default App;
