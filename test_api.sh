#!/bin/bash
# Simple API test script for Squidward Chat

echo "🧪 Testing Squidward Chat API"
echo "================================"
echo ""

BASE_URL="http://localhost:8080"

# Test 1: Register a user
echo "📝 Test 1: Register user 'testuser'"
REGISTER_RESP=$(curl -s -X POST $BASE_URL/api/register \
  -H "Content-Type: application/json" \
  -d '{"username": "testuser", "password": "test123"}')
echo "Response: $REGISTER_RESP"
echo ""

# Test 2: Login
echo "🔐 Test 2: Login as 'testuser'"
LOGIN_RESP=$(curl -s -X POST $BASE_URL/api/login \
  -H "Content-Type: application/json" \
  -d '{"username": "testuser", "password": "test123"}')
echo "Response: $LOGIN_RESP"

# Extract token (basic parsing)
TOKEN=$(echo $LOGIN_RESP | grep -oP '(?<="token":")[^"]+')
echo "Token: $TOKEN"
echo ""

# Test 3: Send a message
echo "💬 Test 3: Send a message"
SEND_RESP=$(curl -s -X POST $BASE_URL/api/messages/send \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"message": "Hello from the test script!"}')
echo "Response: $SEND_RESP"
echo ""

# Test 4: Get messages
echo "📬 Test 4: Get messages"
MESSAGES_RESP=$(curl -s -X GET $BASE_URL/api/messages \
  -H "Authorization: Bearer $TOKEN")
echo "Response: $MESSAGES_RESP"
echo ""

echo "✅ API tests completed!"
