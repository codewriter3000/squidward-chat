# squidward-chat

A minimal barebones instant chat messenger app built with Solid.JS (frontend) and Erlang (backend).

## Features

- 🔐 In-house authentication system (username/password)
- 💬 Real-time messaging via HTTP polling
- 🚀 Built for scalability with extensible OAuth 2.0 architecture
- ⚡ Lightweight and fast - no external dependencies
- 🎨 Clean, modern UI

## Architecture

### Backend (Erlang)
- **Built-in TCP/HTTP** server using only standard library modules
- **OTP** application structure for reliability
- **gen_server** based authentication and chat room management
- Token-based authentication (easily extensible to OAuth 2.0)
- No external dependencies - uses only Erlang/OTP built-ins

### Frontend (Solid.JS)
- Reactive UI with minimal bundle size
- HTTP polling for real-time updates (1 second interval)
- Local storage for session persistence

## Prerequisites

- **Erlang/OTP 24+** - [Installation guide](https://www.erlang.org/downloads)
- **Rebar3** - [Installation guide](https://rebar3.org/docs/getting-started/)
- **Node.js 18+** - [Installation guide](https://nodejs.org/)
- **npm** or **yarn**

## Installation

### 1. Clone the repository
```bash
git clone https://github.com/codewriter3000/squidward-chat.git
cd squidward-chat
```

### 2. Build the backend
```bash
rebar3 compile
```

### 3. Build the frontend
```bash
cd frontend
npm install
npm run build
cd ..
```

## Running the Application

### Start the Erlang backend
```bash
rebar3 shell
```

The server will start on `http://localhost:8080`

### Development mode (optional)
For frontend development with hot reload:
```bash
cd frontend
npm run dev
```
This will start Vite dev server on `http://localhost:5173` with proxy to backend.

## Usage

1. Open your browser to `http://localhost:8080`
2. Register a new account with a username and password
3. Login with your credentials
4. Start chatting in real-time!

## API Endpoints

### Authentication
- `POST /api/register` - Register a new user
  - Body: `{"username": "string", "password": "string"}`
- `POST /api/login` - Login user
  - Body: `{"username": "string", "password": "string"}`
  - Returns: `{"success": true, "token": "string", "username": "string"}`

### Messaging
- `POST /api/messages/send` - Send a chat message (requires Bearer token)
  - Body: `{"message": "string"}`
- `GET /api/messages` - Get recent messages (requires Bearer token)
  - Returns: `{"success": true, "messages": [...]}`

## Extending with OAuth 2.0

The authentication module (`squidward_chat_auth.erl`) is designed to be extensible. To add OAuth 2.0:

1. Add OAuth library dependency to `rebar.config`
2. Extend `squidward_chat_auth` module with OAuth provider functions
3. Add new HTTP handlers for OAuth callbacks
4. Update frontend Auth component to include OAuth buttons

Example structure:
```erlang
% In squidward_chat_auth.erl
oauth_login(Provider, Code) ->
    % Exchange code for token with OAuth provider
    % Verify token
    % Create or retrieve user
    % Generate internal token
    {ok, Token, Username}.
```

## Project Structure

```
squidward-chat/
├── src/                          # Erlang backend source
│   ├── squidward_chat_app.erl   # Main application
│   ├── squidward_chat_sup.erl   # Supervisor
│   ├── squidward_chat_auth.erl  # Authentication module
│   ├── squidward_chat_room.erl  # Chat room manager
│   ├── squidward_chat_ws_handler.erl      # WebSocket handler
│   ├── squidward_chat_login_handler.erl   # Login HTTP handler
│   └── squidward_chat_register_handler.erl # Register HTTP handler
├── frontend/                     # Solid.JS frontend
│   ├── src/
│   │   ├── components/          # UI components
│   │   │   ├── Auth.jsx        # Login/Register component
│   │   │   └── Chat.jsx        # Chat interface component
│   │   ├── App.jsx             # Main app component
│   │   ├── index.jsx           # Entry point
│   │   └── styles.css          # Styles
│   ├── index.html              # HTML template
│   ├── package.json            # Node dependencies
│   └── vite.config.js          # Vite configuration
├── rebar.config                # Erlang dependencies
└── README.md                   # This file
```

## Security Notes

⚠️ **This is a minimal implementation for demonstration purposes:**

- Passwords are hashed with SHA-256 (use bcrypt in production)
- Tokens are simple hashes (use proper JWT in production)
- No HTTPS enforcement (enable in production)
- No rate limiting (add in production)
- No input validation (add comprehensive validation in production)

## License

Apache 2.0

## Contributing

Pull requests are welcome! For major changes, please open an issue first to discuss what you would like to change.