@echo off
echo ========================================
echo Business Rules Extraction Framework
echo ========================================
echo.

echo Creating upload directory...
if not exist "uploads" mkdir uploads

echo.
echo Starting Backend Server...
start cmd /k "cd backend && python main.py"

timeout /t 3 /nobreak > nul

echo.
echo Starting Frontend Server...
start cmd /k "cd frontend && npm run dev"

echo.
echo ========================================
echo Both servers are starting...
echo.
echo Backend: http://localhost:8000
echo Frontend: http://localhost:3000
echo API Docs: http://localhost:8000/docs
echo.
echo ========================================
