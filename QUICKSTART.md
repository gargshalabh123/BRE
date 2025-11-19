# Quick Start Guide - BRE Framework

## You Have Python 3.13 - Here's What to Do

### Step 1: Install Backend Dependencies

**Option A - Automated (Recommended)**
```bash
cd backend
install-py313.bat
```

**Option B - Manual**
```bash
cd backend

# Clean install
rmdir /s /q venv
python -m venv venv
venv\Scripts\activate

# Install core packages
pip install fastapi uvicorn python-multipart python-dotenv aiofiles sqlparse pygments radon lizard

# Optional: Install AI packages if you have API keys
pip install openai anthropic
```

### Step 2: Configure Environment

```bash
# Copy example env file
copy .env.example .env

# Edit .env and add your API keys (optional)
notepad .env
```

### Step 3: Start Backend

```bash
# Make sure you're in backend directory with venv activated
python main.py
```

You should see:
```
INFO:     Uvicorn running on http://0.0.0.0:8000
```

### Step 4: Install Frontend

**In a NEW terminal:**
```bash
cd frontend
npm install
npm run dev
```

### Step 5: Open Browser

Navigate to: **http://localhost:3000**

## What You Can Do Now

1. **Upload a File or ZIP**
   - Drag and drop your legacy codebase
   - Supported: COBOL, Java, Python, C/C++, SQL, JavaScript, etc.

2. **View Analysis**
   - Overview: File counts, LOC, charts
   - Metrics: Detailed file statistics
   - Dependencies: Import analysis
   - Database: SQL query extraction
   - Business Rules: Pattern-based rule detection
   - AI Insights: Generate AI summaries (requires API keys)

3. **Export Results**
   - Click "Export JSON" to download analysis

## Troubleshooting

### Backend won't start?

```bash
# Check if port 8000 is in use
netstat -ano | findstr :8000

# Kill the process if needed
taskkill /PID <PID> /F

# Or use different port
# Edit .env: PORT=8001
```

### Missing packages?

```bash
venv\Scripts\activate
pip list  # Check what's installed
pip install -r requirements.txt  # Reinstall
```

### Frontend issues?

```bash
cd frontend
rmdir /s /q node_modules
del package-lock.json
npm install
```

### Want AI features but getting errors?

Make sure you:
1. Installed AI packages: `pip install openai anthropic`
2. Added keys to `.env` file
3. Restarted the backend server

### Still stuck?

See [TROUBLESHOOTING.md](TROUBLESHOOTING.md) for detailed solutions.

## Testing Without a Real Codebase

Create a test file to try it out:

**test_code.py**
```python
# Sample business logic
def calculate_discount(price, customer_age):
    """Calculate discount based on age"""
    if customer_age >= 65:
        return price * 0.20  # Senior discount
    elif customer_age < 18:
        return price * 0.10  # Youth discount
    return 0

# Database operation
import sqlite3
conn = sqlite3.connect('customers.db')
cursor = conn.execute("SELECT * FROM customers WHERE age > 65")
```

Upload this file and see the analysis!

## What Works Without AI Keys

You can use the framework WITHOUT OpenAI/Anthropic API keys:

✅ Works:
- File upload and scanning
- LOC counting
- Dependency extraction
- SQL query detection
- Business rule patterns
- All visualizations

❌ Requires API keys:
- AI-powered code explanations
- Codebase summarization
- Natural language insights

## Next Steps

1. ✅ Get the basic framework running
2. ✅ Upload a test file
3. ✅ Explore the analysis tabs
4. ⭐ (Optional) Add AI API keys for advanced features
5. ⭐ Customize business rule patterns for your domain
6. ⭐ Export results for documentation

## Common Commands Reference

**Backend:**
```bash
cd backend
venv\Scripts\activate        # Activate environment
python main.py               # Start server
deactivate                   # Deactivate environment
```

**Frontend:**
```bash
cd frontend
npm run dev                  # Start dev server
npm run build                # Build for production
```

**Full Stack:**
```bash
# Use the start script (from root directory)
start.bat
```

## File Locations

- Backend API: `http://localhost:8000`
- Frontend UI: `http://localhost:3000`
- API Docs: `http://localhost:8000/docs`
- Uploaded files: `uploads/` directory
- Configuration: `backend/.env`

## Questions?

- Read the detailed [SETUP.md](SETUP.md)
- Check [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- Review [USAGE.md](USAGE.md) for features
- Check backend console for errors
- Check browser console (F12) for frontend errors
