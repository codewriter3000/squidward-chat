import { createSignal, onMount, onCleanup, For } from 'solid-js';

function Chat(props) {
  const [messages, setMessages] = createSignal([]);
  const [messageInput, setMessageInput] = createSignal('');
  const [connected, setConnected] = createSignal(true);
  let pollInterval;

  onMount(() => {
    fetchMessages();
    pollInterval = setInterval(fetchMessages, 1000);
  });

  onCleanup(() => {
    if (pollInterval) {
      clearInterval(pollInterval);
    }
  });

  const fetchMessages = async () => {
    try {
      const response = await fetch('/api/messages', {
        headers: {
          'Authorization': `Bearer ${props.token}`
        }
      });

      if (response.ok) {
        const data = await response.json();
        if (data.success) {
          setMessages(data.messages || []);
          setConnected(true);
          
          // Auto-scroll to bottom
          setTimeout(() => {
            const messagesEl = document.querySelector('.messages');
            if (messagesEl) {
              messagesEl.scrollTop = messagesEl.scrollHeight;
            }
          }, 0);
        }
      } else if (response.status === 401) {
        props.onLogout();
      }
    } catch (err) {
      console.error('Fetch error:', err);
      setConnected(false);
    }
  };

  const sendMessage = async (e) => {
    e.preventDefault();
    
    if (!messageInput().trim()) {
      return;
    }

    try {
      const response = await fetch('/api/messages/send', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${props.token}`
        },
        body: JSON.stringify({
          message: messageInput()
        })
      });

      if (response.ok) {
        setMessageInput('');
        fetchMessages();
      } else if (response.status === 401) {
        props.onLogout();
      }
    } catch (err) {
      console.error('Send error:', err);
    }
  };

  const formatTime = (timestamp) => {
    const date = new Date(timestamp);
    return date.toLocaleTimeString();
  };

  return (
    <div class="chat-container">
      <div class="chat-header">
        <h1>Squidward Chat</h1>
        <div class="user-info">
          <span>Welcome, {props.username}!</span>
          <span class={`status ${connected() ? 'connected' : 'disconnected'}`}>
            {connected() ? '● Connected' : '○ Disconnected'}
          </span>
          <button onClick={props.onLogout}>Logout</button>
        </div>
      </div>

      <div class="messages">
        <For each={messages()}>
          {(msg) => (
            <div class={`message ${msg.type}`}>
              {msg.type === 'system' ? (
                <span class="system-text">{msg.message}</span>
              ) : (
                <>
                  <span class="username">{msg.username}:</span>
                  <span class="text">{msg.message}</span>
                  <span class="time">{formatTime(msg.timestamp)}</span>
                </>
              )}
            </div>
          )}
        </For>
      </div>

      <form class="message-form" onSubmit={sendMessage}>
        <input
          type="text"
          placeholder="Type a message..."
          value={messageInput()}
          onInput={(e) => setMessageInput(e.target.value)}
          disabled={!connected()}
        />
        <button type="submit" disabled={!connected() || !messageInput().trim()}>
          Send
        </button>
      </form>
    </div>
  );
}

export default Chat;
