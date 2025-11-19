# Quick Start Guide - ZIP Explorer

## 🚀 Getting Started in 5 Minutes

### 1. Start the Server
```bash
cd backend
python main.py
```
Server will start at `http://localhost:8000`

### 2. Upload Your ZIP File

**Option A: Using curl**
```bash
curl -X POST http://localhost:8000/api/zip/upload \
  -F "file=@your_codebase.zip"
```

**Option B: Using Python requests**
```python
import requests

with open('your_codebase.zip', 'rb') as f:
    response = requests.post(
        'http://localhost:8000/api/zip/upload',
        files={'file': f}
    )
    upload_id = response.json()['upload_id']
    print(f"Upload ID: {upload_id}")
```

**Option C: Using the API docs**
1. Go to `http://localhost:8000/docs`
2. Navigate to `/api/zip/upload`
3. Click "Try it out"
4. Upload your file

### 3. Get Quick Summary
```bash
# Replace {upload_id} with your actual upload ID
curl http://localhost:8000/api/zip/{upload_id}/summary
```

**Example Response:**
```json
{
  "filename": "codebase.zip",
  "total_files": 150,
  "total_size_mb": 45.6,
  "total_loc": 25000,
  "languages": [
    {
      "language": "cobol",
      "file_count": 50,
      "total_loc": 15000,
      "percentage": 60.0
    }
  ]
}
```

### 4. Explore Complete Structure
```bash
curl http://localhost:8000/api/zip/{upload_id}/explore
```

This returns:
- ✅ File tree structure
- ✅ All files with metadata
- ✅ Statistics and breakdowns
- ✅ Language distribution

### 5. Run Demo Script (Optional)
```bash
python backend/examples/zip_explorer_demo.py your_codebase.zip
```

For detailed analysis (slower):
```bash
python backend/examples/zip_explorer_demo.py your_codebase.zip --detailed
```

## 📊 Common Use Cases

### Get Overview of Codebase
```bash
# 1. Upload
UPLOAD_ID=$(curl -X POST http://localhost:8000/api/zip/upload \
  -F "file=@codebase.zip" | jq -r '.upload_id')

# 2. Get summary
curl http://localhost:8000/api/zip/$UPLOAD_ID/summary | jq
```

### List All COBOL Files
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/files?language=cobol" | jq
```

### List All SQL Files
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/files?language=sql" | jq
```

### List All AS400/RPG Files
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/files?language=as400" | jq
```

### Get Content of Specific File
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/file/src/main.cbl" | jq
```

### Get Detailed Statistics
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/statistics" | jq
```

## 🔍 What You Get

### For Each File
- Full path and location
- File size (original + compressed)
- Compression ratio
- Lines of code (LOC)
- Programming language
- File type (text/binary)
- Date/time information

### Aggregate Metrics
- Total files and LOC
- Distribution by language
- Distribution by directory
- Distribution by extension
- Largest files
- Text vs. binary breakdown

### Supported Languages
- **COBOL**: `.cbl`, `.cob`, `.cobol`, `.cpy`
- **SQL**: `.sql`, `.ddl`, `.dml`
- **AS400/RPG**: `.rpg`, `.rpgle`, `.rpglec`, `.sqlrpgle`, `.rpg4`, `.rpgiv`, `.dspf`, `.prtf`, `.lf`, `.pf`

## 💡 Pro Tips

### 1. Start with Summary
Always start with `/summary` endpoint - it's fast and gives you the overview.

### 2. Filter Before Analyzing
Use query parameters to filter files:
```bash
# Only .cbl files
curl "http://localhost:8000/api/zip/$UPLOAD_ID/files?extension=.cbl"

# Only COBOL language
curl "http://localhost:8000/api/zip/$UPLOAD_ID/files?language=cobol"
```

### 3. Use Detailed Analysis Sparingly
Only use `?detailed=true` when you need deep code analysis - it's slower:
```bash
curl "http://localhost:8000/api/zip/$UPLOAD_ID/explore?detailed=true"
```

### 4. Clean Up After Analysis
```bash
curl -X DELETE "http://localhost:8000/api/zip/$UPLOAD_ID"
```

### 5. Use the Interactive Docs
Visit `http://localhost:8000/docs` for interactive API documentation with "Try it out" buttons.

## 📝 Python Script Example

```python
import requests
import json

# Upload ZIP
with open('codebase.zip', 'rb') as f:
    response = requests.post(
        'http://localhost:8000/api/zip/upload',
        files={'file': f}
    )
    upload_id = response.json()['upload_id']

# Get summary
summary = requests.get(
    f'http://localhost:8000/api/zip/{upload_id}/summary'
).json()

print(f"Total files: {summary['total_files']}")
print(f"Total LOC: {summary['total_loc']}")
print("\nLanguages:")
for lang in summary['languages']:
    print(f"  {lang['language']}: {lang['file_count']} files, {lang['total_loc']} LOC")

# List COBOL files
cobol_files = requests.get(
    f'http://localhost:8000/api/zip/{upload_id}/files',
    params={'language': 'cobol'}
).json()

print(f"\nFound {cobol_files['total_files']} COBOL files")

# Clean up
requests.delete(f'http://localhost:8000/api/zip/{upload_id}')
```

## 🐛 Troubleshooting

### "Upload not found"
- Check that you're using the correct upload_id
- Upload might have been deleted

### "Invalid ZIP file"
- Ensure file is a valid ZIP archive
- Try re-creating the ZIP file

### "File too large"
- Default limit is 100MB
- Set `MAX_UPLOAD_SIZE_MB` environment variable to increase

### Server not starting
```bash
# Check if port 8000 is in use
netstat -ano | findstr :8000  # Windows
lsof -i :8000                 # Linux/Mac

# Use different port
export PORT=8080
python main.py
```

## 📚 Full Documentation

- **Detailed Usage**: See [backend/docs/ZIP_EXPLORER_USAGE.md](backend/docs/ZIP_EXPLORER_USAGE.md)
- **Implementation Details**: See [IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)
- **API Docs**: Visit `http://localhost:8000/docs` when server is running

## ⚡ Performance Guide

| Operation | Speed | Use When |
|-----------|-------|----------|
| `/summary` | ⚡ Fast | Quick overview needed |
| `/explore` | ⚡⚡ Medium | Full structure needed |
| `/explore?detailed=true` | ⚡⚡⚡ Slow | Deep analysis needed |

## 🎯 Next Steps

1. ✅ Upload your codebase ZIP
2. ✅ Get summary to understand composition
3. ✅ Explore structure and files
4. ✅ Filter by language to focus on specific files
5. ✅ Extract specific file contents as needed
6. ✅ Use detailed analysis for comprehensive insights
7. ✅ Clean up when done

---

**Need Help?** Check the full documentation or run the demo script!
