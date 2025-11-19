#!/bin/bash

echo "========================================"
echo "Business Rules Extraction Framework"
echo "========================================"
echo ""

echo "Creating upload directory..."
mkdir -p uploads

echo ""
echo "Starting Backend Server..."
cd backend
source venv/bin/activate 2>/dev/null || true
python main.py &
BACKEND_PID=$!
cd ..

sleep 3

echo ""
echo "Starting Frontend Server..."
cd frontend
npm run dev &
FRONTEND_PID=$!
cd ..

echo ""
echo "========================================"
echo "Both servers are starting..."
echo ""
echo "Backend: http://localhost:8000"
echo "Frontend: http://localhost:3000"
echo "API Docs: http://localhost:8000/docs"
echo ""
echo "Press Ctrl+C to stop both servers"
echo "========================================"

trap "kill $BACKEND_PID $FRONTEND_PID 2>/dev/null" EXIT

wait
