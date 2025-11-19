@echo off
echo ========================================
echo BRE Backend Setup
echo ========================================
echo.

:: Check Python version
python --version
echo.

echo Creating virtual environment...
python -m venv venv

echo.
echo Activating virtual environment...
call venv\Scripts\activate

echo.
echo Installing dependencies...
echo.
echo Note: Using simplified requirements for Python 3.13 compatibility
pip install --upgrade pip
pip install -r requirements.txt

echo.
echo ========================================
echo Setup Complete!
echo ========================================
echo.
echo To activate the virtual environment:
echo   venv\Scripts\activate
echo.
echo To run the server:
echo   python main.py
echo.
echo Don't forget to create a .env file from .env.example
echo ========================================
pause
