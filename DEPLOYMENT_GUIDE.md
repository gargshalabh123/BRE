## Deployment Guide - Business Rules Extraction Framework

## Complete Stack Overview

### Backend
- **Framework**: FastAPI (Python)
- **Features**:
  - Code analysis (COBOL, SQL, AS400/RPG)
  - ZIP file exploration
  - AI-powered insights
  - Business rules extraction

### Frontend
- **Framework**: React + TypeScript + Vite
- **Features**:
  - File upload
  - Interactive analysis dashboards
  - ZIP explorer with tree/list views
  - File preview with syntax highlighting

---

## Prerequisites

### System Requirements
- **Python**: 3.8 or higher
- **Node.js**: 16 or higher
- **npm**: 8 or higher
- **OS**: Windows, Linux, or macOS

### Optional (for AI features)
- OpenAI API key
- Anthropic API key

---

## Installation

### 1. Clone/Download Repository
```bash
cd c:\code\BRE
```

### 2. Backend Setup

#### Install Python Dependencies
```bash
cd backend

# Create virtual environment (recommended)
python -m venv venv

# Activate virtual environment
# Windows:
venv\Scripts\activate
# Linux/Mac:
source venv/bin/activate

# Install dependencies
pip install fastapi uvicorn python-dotenv python-multipart anthropic openai
```

#### Create Environment File
Create `backend/.env`:
```env
# Server Configuration
HOST=0.0.0.0
PORT=8000

# Upload Configuration
UPLOAD_DIR=../uploads
MAX_UPLOAD_SIZE_MB=100

# AI Configuration (Optional)
ANTHROPIC_API_KEY=your_anthropic_key_here
OPENAI_API_KEY=your_openai_key_here
```

### 3. Frontend Setup

```bash
cd frontend

# Install dependencies
npm install

# Should install:
# - react, react-dom, react-router-dom
# - axios
# - recharts
# - react-syntax-highlighter
# - lucide-react
# - vite, typescript
```

---

## Running in Development Mode

### Option 1: Run Separately (Recommended for Development)

#### Terminal 1 - Backend
```bash
cd backend
python main.py
```
Backend will run on: `http://localhost:8000`

#### Terminal 2 - Frontend
```bash
cd frontend
npm run dev
```
Frontend will run on: `http://localhost:5173`

**Configure CORS**: The backend already allows `http://localhost:5173` in CORS settings.

### Option 2: Use Vite Proxy (Alternative)

Update `frontend/vite.config.ts`:
```typescript
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8000',
        changeOrigin: true
      }
    }
  }
})
```

Then run both servers normally.

---

## Running in Production Mode

### 1. Build Frontend
```bash
cd frontend
npm run build
```

This creates `frontend/dist/` with optimized static files.

### 2. Serve Frontend with Backend

#### Option A: Update FastAPI to Serve Static Files

Update `backend/main.py`:
```python
from fastapi.staticfiles import StaticFiles

# Add before routes
app.mount("/", StaticFiles(directory="../frontend/dist", html=True), name="static")
```

Then run:
```bash
cd backend
python main.py
```

Access at: `http://localhost:8000`

#### Option B: Use a Reverse Proxy (NGINX)

NGINX configuration:
```nginx
server {
    listen 80;
    server_name your-domain.com;

    # Frontend
    location / {
        root /path/to/BRE/frontend/dist;
        try_files $uri $uri/ /index.html;
    }

    # Backend API
    location /api {
        proxy_pass http://localhost:8000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}
```

#### Option C: Docker Deployment

Create `Dockerfile` (backend):
```dockerfile
FROM python:3.9-slim

WORKDIR /app

COPY backend/requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY backend/ .

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

Create `Dockerfile` (frontend):
```dockerfile
FROM node:16-alpine AS build

WORKDIR /app
COPY frontend/package*.json ./
RUN npm install

COPY frontend/ .
RUN npm run build

FROM nginx:alpine
COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
```

Create `docker-compose.yml`:
```yaml
version: '3.8'

services:
  backend:
    build:
      context: .
      dockerfile: Dockerfile.backend
    ports:
      - "8000:8000"
    environment:
      - UPLOAD_DIR=/uploads
    volumes:
      - ./uploads:/uploads

  frontend:
    build:
      context: .
      dockerfile: Dockerfile.frontend
    ports:
      - "80:80"
    depends_on:
      - backend
```

Run:
```bash
docker-compose up -d
```

---

## Testing the Application

### 1. Basic Health Check

```bash
# Backend health
curl http://localhost:8000/health

# Expected: {"status": "healthy"}
```

### 2. Test File Upload

```bash
# Upload a ZIP file
curl -X POST http://localhost:8000/api/zip/upload \
  -F "file=@test_codebase.zip"

# Response will include upload_id
```

### 3. Test ZIP Explorer

```bash
# Replace {upload_id} with actual ID from upload
curl http://localhost:8000/api/zip/{upload_id}/summary

# Should return summary with total files, LOC, etc.
```

### 4. Test Frontend

1. Open browser: `http://localhost:5173` (dev) or `http://localhost:8000` (prod)
2. Drag and drop a ZIP file
3. Wait for upload and analysis
4. Click "ZIP Explorer" tab
5. Verify:
   - Summary cards display correctly
   - Language charts render
   - File tree is interactive
   - File list filters work
   - File preview opens

---

## Supported File Types

### COBOL
- `.cbl` - COBOL source
- `.cob` - COBOL source
- `.cobol` - COBOL source
- `.cpy` - COBOL copybook

### SQL
- `.sql` - SQL script
- `.ddl` - Data Definition Language
- `.dml` - Data Manipulation Language

### AS400/RPG
- `.rpg` - RPG III
- `.rpgle` - RPG IV/ILE
- `.rpglec` - RPG ILE C
- `.sqlrpgle` - SQL RPG
- `.rpg4` - RPG IV
- `.rpgiv` - RPG IV
- `.dspf` - Display file
- `.prtf` - Printer file
- `.lf` - Logical file
- `.pf` - Physical file

---

## API Endpoints Reference

### Upload
- `POST /api/upload` - Upload file/ZIP (legacy)
- `POST /api/zip/upload` - Upload ZIP (new)

### Analysis
- `POST /api/analysis/{id}/full` - Full analysis
- `POST /api/analysis/{id}/metrics` - Metrics only
- `POST /api/analysis/{id}/dependencies` - Dependencies
- `POST /api/analysis/{id}/database` - Database operations
- `POST /api/analysis/{id}/business-rules` - Business rules

### ZIP Explorer
- `GET /api/zip/{id}/summary` - Quick summary
- `GET /api/zip/{id}/explore` - Full exploration
- `GET /api/zip/{id}/files` - List files (with filters)
- `GET /api/zip/{id}/file/{path}` - Get file content
- `POST /api/zip/{id}/extract` - Extract ZIP
- `GET /api/zip/{id}/statistics` - Detailed statistics
- `DELETE /api/zip/{id}` - Delete upload

### AI
- `POST /api/ai/explain-code` - Code explanation
- `POST /api/ai/summarize/{id}` - Codebase summary
- `GET /api/ai/providers` - Available AI providers

---

## Configuration

### Backend Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `HOST` | `0.0.0.0` | Server host |
| `PORT` | `8000` | Server port |
| `UPLOAD_DIR` | `../uploads` | Upload directory |
| `MAX_UPLOAD_SIZE_MB` | `100` | Max upload size |
| `ANTHROPIC_API_KEY` | - | Anthropic API key |
| `OPENAI_API_KEY` | - | OpenAI API key |

### Frontend API Configuration

Update `frontend/src/services/api.ts` if needed:
```typescript
const API_BASE_URL = '/api'  // For production with proxy
// OR
const API_BASE_URL = 'http://localhost:8000/api'  // For dev
```

---

## Troubleshooting

### Backend Issues

**Port already in use:**
```bash
# Change port in .env
PORT=8001

# Or specify in command
python main.py --port 8001
```

**Module not found:**
```bash
# Reinstall dependencies
pip install -r requirements.txt
```

**CORS errors:**
- Check `main.py` CORS configuration
- Ensure frontend URL is in `allow_origins`

### Frontend Issues

**npm install fails:**
```bash
# Clear cache and retry
npm cache clean --force
rm -rf node_modules package-lock.json
npm install
```

**Vite not starting:**
```bash
# Check Node version
node --version  # Should be 16+

# Try different port
npm run dev -- --port 3000
```

**API calls fail:**
- Check backend is running
- Verify `API_BASE_URL` in `api.ts`
- Check browser console for CORS errors

### Upload Issues

**File too large:**
- Increase `MAX_UPLOAD_SIZE_MB` in backend `.env`
- Check disk space

**Analysis fails:**
- Check backend logs
- Verify file is valid ZIP
- Ensure supported file types inside ZIP

---

## Performance Tuning

### Backend
```python
# In main.py, configure workers
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        workers=4,  # Add multiple workers
        reload=False  # Disable in production
    )
```

### Frontend
```bash
# Optimize build
npm run build

# Analyze bundle size
npm run build -- --analyze
```

### Database (if added later)
- Use PostgreSQL or MySQL for persistence
- Add caching layer (Redis)
- Implement database indexing

---

## Security Considerations

### Production Checklist
- [ ] Change default ports
- [ ] Use HTTPS (SSL/TLS)
- [ ] Add authentication
- [ ] Rate limiting
- [ ] Input validation
- [ ] File upload restrictions
- [ ] Sanitize file names
- [ ] Regular security updates
- [ ] Environment variable protection
- [ ] Firewall rules

### Recommended
```bash
# Add authentication middleware
# Add rate limiting
# Use environment-specific configs
# Regular backups
# Monitor logs
# Update dependencies regularly
```

---

## Monitoring

### Logs
```bash
# Backend logs
tail -f backend.log

# Access logs
tail -f access.log
```

### Metrics to Monitor
- Upload success/failure rate
- Analysis time
- API response times
- Disk usage (uploads directory)
- Memory usage
- Error rates

---

## Backup and Maintenance

### Backup
```bash
# Backup uploads directory
tar -czf uploads-backup-$(date +%Y%m%d).tar.gz uploads/

# Backup database (if using)
pg_dump dbname > backup.sql
```

### Cleanup
```bash
# Clean old uploads (older than 7 days)
find uploads/ -type f -mtime +7 -delete

# Clean temp files
rm -rf /tmp/zip_analysis_*
```

---

## Scaling

### Horizontal Scaling
- Use load balancer (NGINX, HAProxy)
- Multiple backend instances
- Shared storage for uploads (S3, NFS)
- Redis for session management

### Vertical Scaling
- Increase server resources
- Optimize database queries
- Use caching
- CDN for static files

---

## Support and Documentation

### Documentation Files
- [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md) - Backend implementation
- [FRONTEND_IMPLEMENTATION.md](FRONTEND_IMPLEMENTATION.md) - Frontend implementation
- [FRONTEND_CAPABILITIES.md](FRONTEND_CAPABILITIES.md) - Frontend features
- [ZIP_EXPLORER_USAGE.md](backend/docs/ZIP_EXPLORER_USAGE.md) - ZIP Explorer API
- [ARCHITECTURE.md](ARCHITECTURE.md) - System architecture
- [QUICK_START.md](QUICK_START.md) - Quick start guide

### API Documentation
- Swagger UI: `http://localhost:8000/docs`
- ReDoc: `http://localhost:8000/redoc`

---

## Quick Reference

### Development Commands
```bash
# Backend
cd backend && python main.py

# Frontend
cd frontend && npm run dev

# Both
# Terminal 1: cd backend && python main.py
# Terminal 2: cd frontend && npm run dev
```

### Production Commands
```bash
# Build frontend
cd frontend && npm run build

# Run production backend
cd backend && python main.py

# Docker
docker-compose up -d
```

### Test Commands
```bash
# Health check
curl http://localhost:8000/health

# Upload ZIP
curl -X POST http://localhost:8000/api/zip/upload -F "file=@test.zip"

# Get summary
curl http://localhost:8000/api/zip/{id}/summary
```

---

**Ready to deploy!** 🚀

For issues or questions, refer to the documentation files or check the API documentation at `/docs`.
