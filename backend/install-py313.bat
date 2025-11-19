@echo off
echo ========================================
echo BRE Backend - Python 3.13 Installation
echo ========================================
echo.

echo Detected Python 3.13 - Using compatible dependencies
echo.

echo Cleaning up old installation...
if exist venv rmdir /s /q venv

echo.
echo Creating virtual environment...
python -m venv venv

echo.
echo Activating virtual environment...
call venv\Scripts\activate

echo.
echo Upgrading pip...
python -m pip install --upgrade pip

echo.
echo Installing dependencies (Python 3.13 compatible)...
pip install fastapi>=0.115.0
pip install uvicorn>=0.32.0
pip install python-multipart>=0.0.12
pip install python-dotenv>=1.0.1
pip install aiofiles>=24.1.0
pip install sqlparse>=0.5.1
pip install pygments>=2.18.0

echo.
echo Installing optional code analysis tools...
pip install radon>=6.0.1 || echo Warning: radon failed to install
pip install lizard>=1.17.10 || echo Warning: lizard failed to install

echo.
echo Installing AI packages (optional - skip if you don't have API keys)...
set /p install_ai="Install AI packages (OpenAI/Anthropic)? (y/n): "
if /i "%install_ai%"=="y" (
    pip install openai>=1.54.0
    pip install anthropic>=0.39.0
)

echo.
echo Creating .env file...
if not exist .env (
    copy .env.example .env
    echo Created .env file - please edit it to add your API keys
) else (
    echo .env file already exists
)

echo.
echo Creating uploads directory...
cd ..
if not exist uploads mkdir uploads
cd backend

echo.
echo ========================================
echo Installation Complete!
echo ========================================
echo.
echo Next steps:
echo 1. Edit .env file and add your API keys (optional)
echo 2. Run: python main.py
echo 3. Open: http://localhost:8000
echo.
echo To activate this environment in the future:
echo   venv\Scripts\activate
echo.
echo ========================================
pause
