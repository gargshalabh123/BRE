# Business Rules Extraction Framework - Setup Guide

## Prerequisites

- Python 3.8 or higher
- Node.js 16 or higher
- npm or yarn
- Git (optional)

## Backend Setup

### 1. Navigate to Backend Directory
```bash
cd backend
```

### 2. Create Virtual Environment
```bash
# On Windows
python -m venv venv
venv\Scripts\activate

# On macOS/Linux
python3 -m venv venv
source venv/bin/activate
```

### 3. Install Dependencies
```bash
pip install -r requirements.txt
```

### 4. Configure Environment Variables
Create a `.env` file in the backend directory:

```bash
cp .env.example .env
```

Edit `.env` and add your API keys:

```env
# Optional: Add AI API keys for NLP features
OPENAI_API_KEY=sk-your-openai-key-here
ANTHROPIC_API_KEY=sk-ant-your-anthropic-key-here

# Upload Configuration
MAX_UPLOAD_SIZE_MB=100
UPLOAD_DIR=../uploads

# Server Configuration
HOST=0.0.0.0
PORT=8000

# AI Model Configuration
OPENAI_MODEL=gpt-4-turbo-preview
ANTHROPIC_MODEL=claude-3-5-sonnet-20241022
```

**Note:** AI features are optional. The framework will work without API keys, but AI-powered code explanations won't be available.

### 5. Create Upload Directory
```bash
mkdir ../uploads
```

### 6. Run the Backend Server
```bash
# Make sure virtual environment is activated
python main.py

# Or use uvicorn directly
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

The backend API will be available at: `http://localhost:8000`

API documentation (Swagger UI): `http://localhost:8000/docs`

## Frontend Setup

### 1. Navigate to Frontend Directory
```bash
cd frontend
```

### 2. Install Dependencies
```bash
npm install
```

### 3. Start Development Server
```bash
npm run dev
```

The frontend will be available at: `http://localhost:3000`

### 4. Build for Production (Optional)
```bash
npm run build
npm run preview
```

## Verification

### Test Backend
1. Open browser to `http://localhost:8000`
2. You should see JSON response with API information
3. Visit `http://localhost:8000/docs` for interactive API documentation

### Test Frontend
1. Open browser to `http://localhost:3000`
2. You should see the upload page
3. Try uploading a sample code file or ZIP archive

## Testing the Complete System

### 1. Prepare Sample Code
Create a test file or use existing code:

```python
# sample.py
def calculate_discount(price, customer_age):
    """Calculate discount based on customer age"""
    if customer_age >= 65:
        return price * 0.20  # 20% senior discount
    elif customer_age < 18:
        return price * 0.10  # 10% youth discount
    return 0

# Database query example
SELECT * FROM customers WHERE age > 65;
```

### 2. Upload via UI
1. Go to `http://localhost:3000`
2. Drag and drop your file or click to browse
3. Wait for upload to complete
4. View analysis results

### 3. Test via API (Optional)
```bash
# Upload file
curl -X POST "http://localhost:8000/api/upload" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@sample.py"

# Analyze (replace UPLOAD_ID with the ID from previous response)
curl -X POST "http://localhost:8000/api/analysis/UPLOAD_ID/full"
```

## Troubleshooting

### Backend Issues

**Port Already in Use**
```bash
# Change port in .env file or run with different port
uvicorn main:app --port 8001
```

**Module Not Found Errors**
```bash
# Ensure virtual environment is activated
pip install -r requirements.txt --upgrade
```

**Upload Directory Errors**
```bash
# Create uploads directory
mkdir uploads
# Check permissions
chmod 755 uploads
```

### Frontend Issues

**Port Already in Use**
Edit `vite.config.ts` and change the port:
```typescript
export default defineConfig({
  server: {
    port: 3001  // Change to any available port
  }
})
```

**API Connection Errors**
- Verify backend is running on port 8000
- Check browser console for CORS errors
- Ensure proxy configuration in `vite.config.ts` is correct

**Build Errors**
```bash
# Clear cache and reinstall
rm -rf node_modules package-lock.json
npm install
```

## Optional: AI Features Setup

### OpenAI Setup
1. Sign up at https://platform.openai.com/
2. Create API key
3. Add to `.env`: `OPENAI_API_KEY=sk-...`
4. Restart backend server

### Anthropic Claude Setup
1. Sign up at https://console.anthropic.com/
2. Create API key
3. Add to `.env`: `ANTHROPIC_API_KEY=sk-ant-...`
4. Restart backend server

### Verify AI Features
```bash
# Check available providers
curl http://localhost:8000/api/ai/providers
```

## Production Deployment

### Backend (Using Gunicorn)
```bash
pip install gunicorn
gunicorn main:app -w 4 -k uvicorn.workers.UvicornWorker --bind 0.0.0.0:8000
```

### Frontend (Static Build)
```bash
npm run build
# Serve the 'dist' folder with nginx or any static server
```

### Docker Deployment (Advanced)
Create `Dockerfile` for backend and frontend, then use docker-compose to run both services.

## Security Considerations

1. **API Keys**: Never commit `.env` files to version control
2. **File Uploads**: The system validates file types and sizes
3. **CORS**: Configure allowed origins in production
4. **Rate Limiting**: Consider adding rate limiting for production
5. **Authentication**: Add user authentication for production use

## Next Steps

- Upload your first codebase
- Explore analysis results and visualizations
- Try AI-powered code explanations
- Export results as JSON
- Customize patterns for business rule extraction

## Support

For issues or questions:
- Check the main README.md
- Review API documentation at `/docs`
- Check backend logs for errors
- Review browser console for frontend issues
