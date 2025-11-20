# BRE Database Implementation

## Overview

SQLite database implementation for the Business Rules Extraction (BRE) system with user management and persistent storage.

## Features

✅ **User Authentication & Authorization**
- Role-based access control (Admin, Analyst, Viewer, Developer)
- User session management
- Activity logging

✅ **Persistent Analysis Storage**
- Save all analysis results to database
- Fast retrieval from cache
- Historical tracking

✅ **Complete Data Model**
- Projects and analysis runs
- File metrics and complexity
- Dependencies with parameters
- Database operations (SQL, IMS DL/I, CICS)
- Business rules extraction
- Error messages and AI insights

## Quick Start

### 1. Initialize Database

```bash
cd backend/database
python init_db.py
```

This will:
- Create `backend/data/bre_analysis.db`
- Execute schema SQL
- Insert default roles and admin user

### 2. Database Location

```
BRE/
├── backend/
│   └── data/
│       └── bre_analysis.db  <-- SQLite database file
```

### 3. Default Credentials

**Username:** `admin`
**Password:** `admin123`

⚠️ **IMPORTANT:** Change the default password in production!

## Database Schema

### User Management Tables

1. **roles** - User roles with permissions
2. **users** - User accounts
3. **user_sessions** - Active login sessions
4. **user_activity_log** - Audit trail

### Analysis Tables

5. **projects** - Codebase projects
6. **analysis_runs** - Each upload/analysis session
7. **files** - Individual source files
8. **dependencies** - Code dependencies
9. **dependency_parameters** - Dependency params
10. **database_operations** - SQL/IMS/CICS operations
11. **db_operation_parameters** - DB operation params
12. **business_rules** - Extracted business logic
13. **complexity_details** - Function-level complexity
14. **ai_insights** - AI-generated suggestions
15. **error_messages** - Error handling tracking
16. **code_snippets** - Cached code snippets
17. **analysis_metadata** - Flexible metadata

## Usage

### Using DatabaseManager

```python
from database.db_manager import DatabaseManager

# Initialize
db = DatabaseManager()

# Create user
user_id = db.create_user(
    username="john",
    email="john@example.com",
    password_hash="<bcrypt_hash>",
    role_id=2,
    full_name="John Doe"
)

# Get user
user = db.get_user_by_username("john")

# Create project
project_id = db.create_project(
    name="Banking System",
    description="Legacy banking COBOL system",
    created_by=user_id
)

# Log activity
db.log_user_activity(
    user_id=user_id,
    activity_type="upload",
    description="Uploaded BANKING.zip"
)

db.close()
```

### Using PersistenceService

```python
from database.db_manager import DatabaseManager
from database.persistence_service import PersistenceService

# Initialize
db = DatabaseManager()
service = PersistenceService(db)

# Save analysis results
analysis_run_id = service.save_analysis_results(
    upload_id="tmpXYZ123",
    analysis_results=analyzer_results,
    project_name="Banking System",
    upload_filename="BANKING.zip",
    user_id=1
)

# Load cached results
cached_results = service.load_analysis_results("tmpXYZ123")

db.close()
```

### API Integration

The database is automatically integrated into the analysis API:

```python
# GET /api/analysis/{upload_id}/full

# Flow:
# 1. Check database for cached results
# 2. If found, return immediately (fast!)
# 3. If not found, run analysis
# 4. Save results to database
# 5. Return results
```

## Querying Data

### Example Queries

**Find files with high complexity:**
```sql
SELECT file_path, complexity
FROM files
WHERE complexity > 50
ORDER BY complexity DESC;
```

**Most frequently called programs:**
```sql
SELECT target, COUNT(*) as call_count
FROM dependencies
WHERE dependency_type = 'PROGRAM_CALL'
GROUP BY target
ORDER BY call_count DESC
LIMIT 10;
```

**Compare two analysis runs:**
```sql
SELECT
    f1.file_path,
    f1.complexity as old_complexity,
    f2.complexity as new_complexity,
    (f2.complexity - f1.complexity) as complexity_change
FROM files f1
JOIN files f2 ON f1.file_path = f2.file_path
WHERE f1.analysis_run_id = 1 AND f2.analysis_run_id = 2
AND f1.complexity != f2.complexity
ORDER BY ABS(f2.complexity - f1.complexity) DESC;
```

**Find all IMS DL/I operations:**
```sql
SELECT f.file_path, dbo.category, dbo.line_number, dbo.query_text
FROM database_operations dbo
JOIN files f ON dbo.file_id = f.id
WHERE dbo.operation_type = 'IMS DL/I'
ORDER BY f.file_path, dbo.line_number;
```

**User activity report:**
```sql
SELECT
    u.username,
    ual.activity_type,
    ual.activity_description,
    ual.created_at
FROM user_activity_log ual
JOIN users u ON ual.user_id = u.id
WHERE ual.created_at >= datetime('now', '-7 days')
ORDER BY ual.created_at DESC;
```

## Performance

### Optimization Settings

The database is configured for optimal performance:

```sql
PRAGMA journal_mode = WAL;        -- Write-Ahead Logging
PRAGMA synchronous = NORMAL;      -- Balance safety and speed
PRAGMA cache_size = -64000;       -- 64MB cache
PRAGMA temp_store = MEMORY;       -- Use memory for temp tables
```

### Indexing

All tables have appropriate indexes on:
- Foreign keys
- Search columns (file_path, dependency_type, etc.)
- Sort columns (created_at, complexity, etc.)

## Backup & Maintenance

### Backup Database

```bash
# Simple copy
cp backend/data/bre_analysis.db backend/data/bre_analysis_backup.db

# Or use SQLite backup command
sqlite3 backend/data/bre_analysis.db ".backup 'backend/data/bre_backup_$(date +%Y%m%d).db'"
```

### Vacuum Database

```bash
sqlite3 backend/data/bre_analysis.db "VACUUM;"
```

### Export to JSON

```python
from database.db_manager import DatabaseManager
import json

db = DatabaseManager()
results = db.get_full_analysis_results("upload_id")

with open('export.json', 'w') as f:
    json.dump(results, f, indent=2, default=str)

db.close()
```

## Future Enhancements

- [ ] Add user authentication API endpoints
- [ ] Implement password reset functionality
- [ ] Add analysis comparison views in frontend
- [ ] Create reporting dashboard
- [ ] Add export to Excel/PDF
- [ ] Implement data retention policies
- [ ] Add full-text search on code snippets
- [ ] Create migration scripts for schema updates

## Troubleshooting

### Database Locked Error

If you get "database is locked" error:
```python
# Increase timeout
db.connection.execute("PRAGMA busy_timeout = 5000")
```

### Schema Version Mismatch

To upgrade schema:
```bash
# Backup first!
cp backend/data/bre_analysis.db backend/data/bre_analysis_backup.db

# Then run migration script (to be created)
python backend/database/migrate.py
```

## Security Notes

1. **Password Hashing**: Always use bcrypt with proper salt
2. **SQL Injection**: All queries use parameterized statements
3. **Session Management**: Sessions expire after inactivity
4. **Access Control**: Check user permissions before operations
5. **Audit Trail**: All user actions are logged

## License

Same as parent BRE project.
