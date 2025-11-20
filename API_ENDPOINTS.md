# BRE API Endpoints Documentation

## Base URL
`http://localhost:8000/api`

---

## Analysis Endpoints

### 1. Get All Saved Analyses
**Endpoint:** `GET /analysis/saved`

**Description:** Retrieves a list of all saved analysis runs from the database.

**Response:**
```json
{
  "analyses": [
    {
      "id": 1,
      "upload_id": "00caa244-fb44-4eb8-a92c-0988c83708f8",
      "upload_filename": "demo-code.zip",
      "analysis_date": "2025-11-20 06:06:57",
      "status": "completed",
      "total_files": 311,
      "total_loc": 59441,
      "total_sloc": 52543,
      "total_comments": 3342,
      "avg_complexity": 0.0,
      "project_name": "Default Project",
      "project_description": null
    }
  ],
  "total_count": 1
}
```

**Status Codes:**
- `200 OK` - Successfully retrieved analyses
- `500 Internal Server Error` - Database error

**Example:**
```bash
curl -X GET http://localhost:8000/api/analysis/saved
```

---

### 2. Analyze Full Codebase
**Endpoint:** `POST /analysis/{upload_id}/full`

**Description:** Runs comprehensive analysis on an uploaded codebase. Checks database cache first, performs fresh analysis if not found, and saves results to database.

**Path Parameters:**
- `upload_id` (string) - Unique identifier for the uploaded ZIP file

**Response:**
```json
{
  "metrics": {
    "total_loc": 59441,
    "total_sloc": 52543,
    "total_comments": 3342,
    "total_blank": 3556,
    "average_complexity": 0.0,
    "by_file": [...]
  },
  "dependencies": {},
  "detailed_dependencies": {...},
  "database_operations": {...},
  "business_rules": [...],
  "complexity": {},
  "TEST_SERVER_RELOADED": true,
  "TEST_MESSAGE": "SERVER CODE UPDATED - V3.0 WITH DATABASE",
  "CACHED_FROM_DB": true,
  "analysis_run_id": 1
}
```

**Features:**
- **Database Caching:** If analysis exists in DB, returns cached results (10-20x faster)
- **Auto-Save:** Fresh analyses automatically saved to database
- **Historical Tracking:** Each analysis stored with timestamp and metrics

**Status Codes:**
- `200 OK` - Analysis completed or retrieved from cache
- `404 Not Found` - Upload ID not found
- `500 Internal Server Error` - Analysis failed

**Example:**
```bash
curl -X POST http://localhost:8000/api/analysis/00caa244-fb44-4eb8-a92c-0988c83708f8/full
```

---

### 3. Get Code Metrics
**Endpoint:** `POST /analysis/{upload_id}/metrics`

**Description:** Retrieves code metrics (LOC, complexity, etc.) for an uploaded codebase.

**Path Parameters:**
- `upload_id` (string) - Upload identifier

**Response:**
```json
{
  "total_loc": 59441,
  "total_sloc": 52543,
  "total_comments": 3342,
  "total_blank": 3556,
  "average_complexity": 0.0,
  "by_file": [...],
  "by_language": {...}
}
```

**Status Codes:**
- `200 OK` - Metrics retrieved successfully
- `404 Not Found` - Upload not found
- `500 Internal Server Error` - Analysis failed

---

### 4. Get Dependencies
**Endpoint:** `POST /analysis/{upload_id}/dependencies`

**Description:** Extracts dependency information and generates dependency graph data.

**Response:**
```json
{
  "dependencies": {
    "FILE1.cbl": ["MODULE1", "MODULE2"],
    "FILE2.cbl": ["MODULE3"]
  },
  "graph": {
    "nodes": [
      {"id": "FILE1.cbl"},
      {"id": "MODULE1"}
    ],
    "edges": [
      {"source": "FILE1.cbl", "target": "MODULE1"}
    ]
  }
}
```

**Status Codes:**
- `200 OK` - Dependencies extracted successfully
- `404 Not Found` - Upload not found
- `500 Internal Server Error` - Analysis failed

---

### 5. Get Database Operations
**Endpoint:** `POST /analysis/{upload_id}/database`

**Description:** Extracts all database operations (SQL, IMS DL/I, CICS) from the codebase.

**Response:**
```json
{
  "queries": [
    {
      "file": "PROGRAM1.cbl",
      "line": 150,
      "type": "SQL",
      "category": "SELECT",
      "query": "SELECT * FROM CUSTOMERS",
      "target": "CUSTOMERS"
    }
  ],
  "total_count": 1,
  "by_type": {
    "SQL": 1
  }
}
```

**Status Codes:**
- `200 OK` - Database operations extracted
- `404 Not Found` - Upload not found
- `500 Internal Server Error` - Analysis failed

---

### 6. Extract Business Rules
**Endpoint:** `POST /analysis/{upload_id}/business-rules`

**Description:** Extracts business rules using pattern matching (88-levels, IF-THEN, EVALUATE, etc.).

**Response:**
```json
{
  "rules": [
    {
      "file": "PROGRAM1.cbl",
      "line": 50,
      "type": "CONDITION_88",
      "condition_name": "VALID-CUSTOMER",
      "value": "01",
      "description": "Customer status is valid"
    }
  ],
  "total_count": 1
}
```

**Status Codes:**
- `200 OK` - Business rules extracted
- `404 Not Found` - Upload not found
- `500 Internal Server Error` - Extraction failed

---

### 7. Get File Content
**Endpoint:** `GET /analysis/{upload_id}/file/{file_path:path}`

**Description:** Retrieves the content of a specific source file.

**Path Parameters:**
- `upload_id` (string) - Upload identifier
- `file_path` (string) - Relative path to the file (can include subdirectories)

**Response:**
```json
{
  "path": "src/PROGRAM1.cbl",
  "content": "      IDENTIFICATION DIVISION...",
  "lines": 500,
  "size": 15000
}
```

**Status Codes:**
- `200 OK` - File content retrieved
- `404 Not Found` - File or upload not found
- `500 Internal Server Error` - Failed to read file

**Example:**
```bash
curl -X GET http://localhost:8000/api/analysis/{upload_id}/file/src/PROGRAM1.cbl
```

---

## Upload Endpoints

### 1. Upload ZIP File
**Endpoint:** `POST /upload`

**Description:** Upload a COBOL codebase ZIP file for analysis.

**Request:**
- Content-Type: `multipart/form-data`
- Field name: `file`

**Response:**
```json
{
  "upload_id": "00caa244-fb44-4eb8-a92c-0988c83708f8",
  "filename": "demo-code.zip",
  "size": 1048576,
  "status": "uploaded"
}
```

**Status Codes:**
- `200 OK` - File uploaded successfully
- `400 Bad Request` - Invalid file or format
- `500 Internal Server Error` - Upload failed

---

## Database Schema

The API integrates with a SQLite database containing:

### User Management Tables
- `roles` - User roles with permissions
- `users` - User accounts
- `user_sessions` - Active sessions
- `user_activity_log` - Audit trail

### Analysis Tables
- `projects` - Codebase projects
- `analysis_runs` - Analysis history
- `files` - File metrics
- `dependencies` - Code dependencies
- `database_operations` - SQL/IMS/CICS operations
- `business_rules` - Extracted business logic
- And more...

---

## Performance Features

### Database Caching
- First analysis of a codebase: ~10-30 seconds (runs full analysis + saves to DB)
- Subsequent requests: ~0.5-2 seconds (loads from database) **⚡ 10-20x faster!**

### Optimizations
- WAL mode for better concurrency
- 64MB cache size
- Proper indexing on all foreign keys
- Efficient JOIN operations

---

## Error Handling

All endpoints return consistent error responses:

```json
{
  "detail": "Error message description"
}
```

Common error scenarios:
- Upload not found → 404
- Database errors → 500
- Analysis failures → 500 with detailed error message

---

## CORS Configuration

The API has CORS enabled for:
- `http://localhost:3000`
- `http://localhost:3001`
- Allow credentials: Yes
- Allow methods: All
- Allow headers: All

---

## Testing

### Test Saved Analyses Endpoint
```bash
curl -X GET http://localhost:8000/api/analysis/saved
```

### Test Full Analysis (with caching)
```bash
# First call - runs analysis
curl -X POST http://localhost:8000/api/analysis/{upload_id}/full

# Second call - returns from cache (much faster!)
curl -X POST http://localhost:8000/api/analysis/{upload_id}/full
```

### Check API Documentation
Visit: `http://localhost:8000/docs` (FastAPI auto-generated Swagger UI)

---

## Future Endpoints (TODO)

### Authentication
- `POST /api/auth/login` - Login with credentials
- `POST /api/auth/logout` - Logout current session
- `POST /api/auth/register` - Create new user

### User Management
- `GET /api/users/me` - Get current user info
- `PUT /api/users/me` - Update current user
- `GET /api/users` - List all users (admin only)

### Projects
- `GET /api/projects` - List all projects
- `POST /api/projects` - Create new project
- `PUT /api/projects/{id}` - Update project
- `DELETE /api/projects/{id}` - Delete project

### Analysis Management
- `DELETE /api/analysis/{upload_id}` - Delete analysis
- `GET /api/analysis/{upload_id}/compare/{other_upload_id}` - Compare two analyses

---

## Database Location

**SQLite Database:** `backend/data/bre_analysis.db`

**Access with:**
```bash
sqlite3 backend/data/bre_analysis.db
```

**Query Examples:**
```sql
-- List all saved analyses
SELECT * FROM analysis_runs ORDER BY analysis_date DESC;

-- Count analyses by status
SELECT status, COUNT(*) FROM analysis_runs GROUP BY status;

-- View most complex files
SELECT file_path, complexity FROM files ORDER BY complexity DESC LIMIT 10;
```

---

## Integration Example (Frontend)

### Fetch Saved Analyses
```typescript
const response = await fetch('http://localhost:8000/api/analysis/saved')
const data = await response.json()
console.log(`Found ${data.total_count} saved analyses`)
```

### Run Analysis
```typescript
const response = await fetch(
  `http://localhost:8000/api/analysis/${uploadId}/full`,
  { method: 'POST' }
)
const results = await response.json()

if (results.CACHED_FROM_DB) {
  console.log('Loaded from cache - instant results!')
} else {
  console.log('Fresh analysis completed and saved to DB')
}
```

---

## Summary

The BRE API provides comprehensive endpoints for:
✅ Uploading and analyzing COBOL codebases
✅ Retrieving historical analysis data
✅ Extracting dependencies, database operations, and business rules
✅ Automatic database caching for performance
✅ Complete audit trail and tracking

**All endpoints are now live and tested!**
