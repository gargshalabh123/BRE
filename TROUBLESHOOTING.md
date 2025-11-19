# Troubleshooting Guide - BRE Framework

## Python 3.13 Compatibility Issue

### Problem
If you're using Python 3.13, you may encounter compilation errors with pandas and numpy due to Cython compatibility issues.

### Solution
I've updated the `requirements.txt` to remove pandas and numpy dependencies, which aren't essential for the BRE framework.

### Installation Steps for Python 3.13

1. **Clean Installation**
   ```bash
   cd backend

   # Remove old virtual environment if it exists
   rmdir /s /q venv

   # Create new virtual environment
   python -m venv venv

   # Activate it
   venv\Scripts\activate

   # Upgrade pip
   python -m pip install --upgrade pip

   # Install dependencies
   pip install -r requirements.txt
   ```

2. **If You Still Have Issues**

   Use the minimal requirements file:
   ```bash
   pip install -r requirements-minimal.txt
   ```

## Alternative: Use Python 3.11 or 3.12

If you need the full feature set with all dependencies:

1. **Install Python 3.11 or 3.12**
   - Download from [python.org](https://www.python.org/downloads/)
   - Python 3.11.x or 3.12.x are recommended for best compatibility

2. **Create Virtual Environment with Specific Python Version**
   ```bash
   # Using Python 3.11
   py -3.11 -m venv venv

   # Or Python 3.12
   py -3.12 -m venv venv

   # Activate and install
   venv\Scripts\activate
   pip install -r requirements.txt
   ```

## Common Issues and Solutions

### Issue 1: Module Not Found Errors

**Problem:**
```
ModuleNotFoundError: No module named 'radon'
```

**Solution:**
```bash
# Make sure virtual environment is activated
venv\Scripts\activate

# Reinstall dependencies
pip install -r requirements.txt
```

### Issue 2: Port Already in Use

**Problem:**
```
Address already in use: ('0.0.0.0', 8000)
```

**Solution:**

Option 1 - Kill the process:
```bash
# Find process using port 8000
netstat -ano | findstr :8000

# Kill it (replace PID with actual process ID)
taskkill /PID <PID> /F
```

Option 2 - Use different port:
```bash
# Edit .env file
PORT=8001

# Or run with custom port
python -c "import uvicorn; from main import app; uvicorn.run(app, port=8001)"
```

### Issue 3: AI Features Not Working

**Problem:**
```
AI provider 'anthropic' not available
```

**Solution:**
1. Check if AI packages are installed:
   ```bash
   pip list | findstr -i "openai anthropic"
   ```

2. Install AI packages if missing:
   ```bash
   pip install openai anthropic
   ```

3. Add API keys to `.env`:
   ```env
   OPENAI_API_KEY=sk-your-key-here
   ANTHROPIC_API_KEY=sk-ant-your-key-here
   ```

### Issue 4: Upload Directory Errors

**Problem:**
```
FileNotFoundError: [WinError 3] The system cannot find the path specified: '../uploads'
```

**Solution:**
```bash
# Create uploads directory
mkdir uploads

# Or from backend directory
cd ..
mkdir uploads
cd backend
```

### Issue 5: CORS Errors in Frontend

**Problem:**
```
Access to XMLHttpRequest blocked by CORS policy
```

**Solution:**

1. Make sure backend is running on port 8000
2. Make sure frontend proxy is configured correctly in `frontend/vite.config.ts`
3. Check that backend CORS middleware allows your frontend origin

### Issue 6: Import Errors After Activation

**Problem:**
After activating venv, Python still can't find installed packages.

**Solution:**
```bash
# Deactivate current environment
deactivate

# Delete and recreate virtual environment
rmdir /s /q venv
python -m venv venv
venv\Scripts\activate

# Reinstall
pip install -r requirements.txt
```

## Dependency Installation Issues

### Radon Installation Fails

**Solution:**
Radon is optional. If it fails, comment it out from requirements.txt and the code will gracefully handle its absence.

```python
# In code_analyzer.py, radon import is already wrapped in try/except
try:
    from radon.complexity import cc_visit
except ImportError:
    print("Warning: radon not installed")
```

### Lizard Installation Fails

**Solution:**
Similar to radon, lizard is optional:

```bash
# Install without lizard
pip install -r requirements.txt --ignore-installed lizard
```

## Frontend Issues

### Issue 1: npm install fails

**Problem:**
```
npm ERR! network request failed
```

**Solution:**
```bash
# Clear npm cache
npm cache clean --force

# Delete node_modules and package-lock.json
rmdir /s /q node_modules
del package-lock.json

# Reinstall
npm install
```

### Issue 2: Port 3000 Already in Use

**Solution:**

Edit `frontend/vite.config.ts`:
```typescript
export default defineConfig({
  server: {
    port: 3001  // Change to any available port
  }
})
```

### Issue 3: Module not found in React

**Problem:**
```
Module not found: Can't resolve 'recharts'
```

**Solution:**
```bash
cd frontend
npm install recharts react-dropzone react-markdown react-syntax-highlighter lucide-react
```

## Running Without Optional Dependencies

If you want to run with minimal dependencies (core functionality only):

1. **Backend (minimal)**
   ```bash
   pip install fastapi uvicorn python-multipart python-dotenv aiofiles
   ```

2. **What works without optional deps:**
   - ✅ File upload
   - ✅ Basic file scanning
   - ✅ LOC counting (basic)
   - ✅ Dependency extraction
   - ✅ SQL query detection
   - ✅ Business rule extraction
   - ❌ Complexity metrics (needs radon)
   - ❌ AI features (needs openai/anthropic)

## Environment Variables

Make sure your `.env` file is properly configured:

```env
# Required
UPLOAD_DIR=../uploads
MAX_UPLOAD_SIZE_MB=100
HOST=0.0.0.0
PORT=8000

# Optional - Only if using AI features
OPENAI_API_KEY=sk-...
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_MODEL=gpt-4-turbo-preview
ANTHROPIC_MODEL=claude-3-5-sonnet-20241022
```

## Testing the Installation

Run this test to verify everything works:

```bash
# In backend directory with venv activated
python -c "from main import app; print('✓ Backend imports OK')"

# Test API
python main.py
# In another terminal:
curl http://localhost:8000
```

## Getting Help

If none of these solutions work:

1. Check Python version: `python --version`
2. Check installed packages: `pip list`
3. Check backend logs for specific errors
4. Review browser console for frontend errors
5. Ensure all ports are available (8000 for backend, 3000 for frontend)

## Quick Reset (Nuclear Option)

If everything is broken, start fresh:

```bash
# Backend
cd backend
rmdir /s /q venv
python -m venv venv
venv\Scripts\activate
pip install --upgrade pip
pip install -r requirements-minimal.txt
copy .env.example .env

# Frontend
cd ..\frontend
rmdir /s /q node_modules
del package-lock.json
npm install

# Test
cd ..\backend
python main.py
# In another terminal:
cd frontend
npm run dev
```

## Python Version Recommendation

**Best compatibility:** Python 3.11 or 3.12

**Working but limited:** Python 3.13 (use requirements-minimal.txt)

**Not recommended:** Python 3.9 or older
