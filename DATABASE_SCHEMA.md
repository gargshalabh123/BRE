# Business Rules Extraction - SQLite Database Schema

## Overview

This document proposes a comprehensive SQLite database schema to persist all analysis data from the BRE system, enabling:
- Historical tracking of code analysis over time
- Querying and reporting across multiple analysis runs
- Comparison between different versions of codebases
- Faster retrieval of previously analyzed data
- Export capabilities for external tools

---

## Database Schema

### **1. projects**
Tracks different projects/codebases being analyzed.

```sql
CREATE TABLE projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(name)
);

CREATE INDEX idx_projects_name ON projects(name);
```

**Purpose:** Central registry of all projects being analyzed.

---

### **2. analysis_runs**
Each upload/analysis session.

```sql
CREATE TABLE analysis_runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL,
    upload_id TEXT NOT NULL UNIQUE,  -- Current temp directory ID
    upload_filename TEXT,
    analysis_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status TEXT DEFAULT 'pending',  -- pending, in_progress, completed, failed
    total_files INTEGER DEFAULT 0,
    total_loc INTEGER DEFAULT 0,
    total_sloc INTEGER DEFAULT 0,
    total_comments INTEGER DEFAULT 0,
    total_blank INTEGER DEFAULT 0,
    avg_complexity REAL DEFAULT 0.0,
    error_message TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP,
    FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
);

CREATE INDEX idx_analysis_runs_project ON analysis_runs(project_id);
CREATE INDEX idx_analysis_runs_upload_id ON analysis_runs(upload_id);
CREATE INDEX idx_analysis_runs_date ON analysis_runs(analysis_date);
```

**Purpose:** Track each analysis session with summary statistics.

---

### **3. files**
Individual source files analyzed.

```sql
CREATE TABLE files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    file_path TEXT NOT NULL,
    file_name TEXT NOT NULL,
    file_type TEXT,  -- .cbl, .py, .java, etc.
    file_size INTEGER,
    loc INTEGER DEFAULT 0,
    sloc INTEGER DEFAULT 0,
    comments INTEGER DEFAULT 0,
    blank INTEGER DEFAULT 0,
    complexity INTEGER DEFAULT 0,
    functions INTEGER DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_files_analysis_run ON files(analysis_run_id);
CREATE INDEX idx_files_path ON files(file_path);
CREATE INDEX idx_files_type ON files(file_type);
CREATE INDEX idx_files_complexity ON files(complexity);
```

**Purpose:** Store file-level metrics and metadata.

---

### **4. dependencies**
Detailed dependency tracking.

```sql
CREATE TABLE dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    dependency_type TEXT NOT NULL,  -- PROGRAM_CALL, COPYBOOK, CICS_XCTL, etc.
    target TEXT NOT NULL,
    line_number INTEGER,
    signature TEXT,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_dependencies_file ON dependencies(file_id);
CREATE INDEX idx_dependencies_type ON dependencies(dependency_type);
CREATE INDEX idx_dependencies_target ON dependencies(target);
CREATE INDEX idx_dependencies_analysis_run ON dependencies(analysis_run_id);
```

**Purpose:** Store all code dependencies with context.

---

### **5. dependency_parameters**
Parameters for each dependency (e.g., COMMAREA, FROM, INTO).

```sql
CREATE TABLE dependency_parameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    dependency_id INTEGER NOT NULL,
    parameter_key TEXT,  -- e.g., 'COMMAREA', 'FROM', 'PCB'
    parameter_value TEXT,
    parameter_order INTEGER DEFAULT 0,
    FOREIGN KEY (dependency_id) REFERENCES dependencies(id) ON DELETE CASCADE
);

CREATE INDEX idx_dep_params_dependency ON dependency_parameters(dependency_id);
```

**Purpose:** Store structured parameter data for dependencies.

---

### **6. database_operations**
All database operations (SQL, IMS DL/I, CICS operations).

```sql
CREATE TABLE database_operations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    operation_type TEXT NOT NULL,  -- SQL, IMS DL/I, CICS
    category TEXT,  -- SELECT, INSERT, UPDATE, DELETE, GU, GNP, SYNCPOINT, etc.
    line_number INTEGER,
    query_text TEXT,
    target_table TEXT,  -- For SQL
    target_segment TEXT,  -- For IMS
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_db_ops_file ON database_operations(file_id);
CREATE INDEX idx_db_ops_type ON database_operations(operation_type);
CREATE INDEX idx_db_ops_category ON database_operations(category);
CREATE INDEX idx_db_ops_table ON database_operations(target_table);
CREATE INDEX idx_db_ops_analysis_run ON database_operations(analysis_run_id);
```

**Purpose:** Track all data access patterns.

---

### **7. db_operation_parameters**
Parameters for database operations (PCB, SEGMENT, LAYOUT, WHERE, etc.).

```sql
CREATE TABLE db_operation_parameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    db_operation_id INTEGER NOT NULL,
    parameter_key TEXT,  -- e.g., 'PCB', 'SEG', 'LAYOUT', 'WHERE'
    parameter_value TEXT,
    parameter_order INTEGER DEFAULT 0,
    FOREIGN KEY (db_operation_id) REFERENCES database_operations(id) ON DELETE CASCADE
);

CREATE INDEX idx_db_op_params_operation ON db_operation_parameters(db_operation_id);
```

**Purpose:** Store structured parameters for DB operations.

---

### **8. business_rules**
Extracted business rules.

```sql
CREATE TABLE business_rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    rule_type TEXT NOT NULL,  -- IF-THEN, EVALUATE, 88-Level Condition, etc.
    line_number INTEGER,
    condition_name TEXT,  -- For 88-levels
    condition_value TEXT,  -- For 88-levels
    description TEXT,
    code_snippet TEXT,
    confidence_score REAL DEFAULT 1.0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_business_rules_file ON business_rules(file_id);
CREATE INDEX idx_business_rules_type ON business_rules(rule_type);
CREATE INDEX idx_business_rules_analysis_run ON business_rules(analysis_run_id);
```

**Purpose:** Store all extracted business logic.

---

### **9. complexity_details**
Detailed complexity breakdown per file.

```sql
CREATE TABLE complexity_details (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    function_name TEXT,
    start_line INTEGER,
    end_line INTEGER,
    cyclomatic_complexity INTEGER DEFAULT 0,
    cognitive_complexity INTEGER DEFAULT 0,
    decision_points INTEGER DEFAULT 0,
    nesting_depth INTEGER DEFAULT 0,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE
);

CREATE INDEX idx_complexity_file ON complexity_details(file_id);
CREATE INDEX idx_complexity_function ON complexity_details(function_name);
```

**Purpose:** Track complexity at function/paragraph level.

---

### **10. ai_insights**
AI-generated insights and suggestions.

```sql
CREATE TABLE ai_insights (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    file_id INTEGER,  -- NULL for project-level insights
    insight_type TEXT NOT NULL,  -- modernization_suggestion, code_smell, refactoring_opportunity
    title TEXT NOT NULL,
    description TEXT,
    severity TEXT,  -- low, medium, high, critical
    line_number INTEGER,
    code_snippet TEXT,
    suggestion TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE
);

CREATE INDEX idx_ai_insights_analysis_run ON ai_insights(analysis_run_id);
CREATE INDEX idx_ai_insights_file ON ai_insights(file_id);
CREATE INDEX idx_ai_insights_type ON ai_insights(insight_type);
CREATE INDEX idx_ai_insights_severity ON ai_insights(severity);
```

**Purpose:** Store AI-generated analysis and recommendations.

---

### **11. error_messages**
Tracked error/diagnostic messages from code.

```sql
CREATE TABLE error_messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    message_type TEXT,  -- ERROR, WARNING, INFO
    line_number INTEGER,
    message_text TEXT,
    variables_referenced TEXT,  -- JSON array of variable names
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_error_messages_file ON error_messages(file_id);
CREATE INDEX idx_error_messages_type ON error_messages(message_type);
CREATE INDEX idx_error_messages_analysis_run ON error_messages(analysis_run_id);
```

**Purpose:** Track error handling patterns.

---

### **12. code_snippets**
Reusable code snippets cache (for quick retrieval).

```sql
CREATE TABLE code_snippets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    start_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    snippet_text TEXT NOT NULL,
    snippet_hash TEXT,  -- MD5 hash for deduplication
    snippet_type TEXT,  -- function, paragraph, section
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE
);

CREATE INDEX idx_snippets_file ON code_snippets(file_id);
CREATE INDEX idx_snippets_hash ON code_snippets(snippet_hash);
```

**Purpose:** Cache code snippets for faster retrieval.

---

### **13. analysis_metadata**
Additional metadata for analysis runs.

```sql
CREATE TABLE analysis_metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    metadata_key TEXT NOT NULL,
    metadata_value TEXT,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_metadata_analysis_run ON analysis_metadata(analysis_run_id);
CREATE INDEX idx_metadata_key ON analysis_metadata(metadata_key);
```

**Purpose:** Store flexible key-value metadata for analysis runs.

---

## Relationships

```
projects (1) ──────── (N) analysis_runs
                            │
                            ├── (N) files
                            │      ├── (N) dependencies
                            │      │      └── (N) dependency_parameters
                            │      ├── (N) database_operations
                            │      │      └── (N) db_operation_parameters
                            │      ├── (N) business_rules
                            │      ├── (N) complexity_details
                            │      ├── (N) error_messages
                            │      └── (N) code_snippets
                            │
                            ├── (N) ai_insights
                            └── (N) analysis_metadata
```

---

## Key Benefits

### **1. Historical Tracking**
- Compare analysis results across different versions
- Track code quality trends over time
- Identify when specific issues were introduced

### **2. Advanced Querying**
```sql
-- Find all files with high complexity
SELECT * FROM files WHERE complexity > 50 ORDER BY complexity DESC;

-- Find most frequently called programs
SELECT target, COUNT(*) as call_count
FROM dependencies
WHERE dependency_type = 'PROGRAM_CALL'
GROUP BY target
ORDER BY call_count DESC;

-- Compare two analysis runs
SELECT
    f1.file_path,
    f1.complexity as old_complexity,
    f2.complexity as new_complexity,
    (f2.complexity - f1.complexity) as complexity_change
FROM files f1
JOIN files f2 ON f1.file_path = f2.file_path
WHERE f1.analysis_run_id = 1 AND f2.analysis_run_id = 2;
```

### **3. Performance**
- Cached results instead of re-analyzing
- Fast lookups with proper indexing
- Efficient data retrieval for dashboards

### **4. Reporting**
- Generate compliance reports
- Export data to Excel/CSV
- Create custom analytics

### **5. Integration**
- Connect to BI tools (Tableau, Power BI)
- API endpoints can query DB directly
- Support for batch processing

---

## Migration Strategy

### **Phase 1: Database Setup**
1. Create SQLite database file
2. Execute schema creation SQL
3. Add database connection to backend

### **Phase 2: Data Persistence Layer**
1. Create `database/db_manager.py` - Database operations
2. Create `database/models.py` - ORM models (using SQLAlchemy)
3. Update analyzers to save results to DB

### **Phase 3: API Integration**
1. Modify API endpoints to check DB first
2. Fallback to in-memory analysis if not cached
3. Add endpoints for historical queries

### **Phase 4: Frontend Updates**
1. Add project selection dropdown
2. Show analysis history
3. Comparison views between runs

---

## Additional Considerations

### **Database File Location**
```
BRE/
├── backend/
│   └── data/
│       └── bre_analysis.db  <-- SQLite database file
```

### **Backup Strategy**
- Automatic daily backups
- Export to JSON for portability
- Retention policy (keep last 30 days by default)

### **Performance Optimizations**
- Use PRAGMA settings for better performance
- Regular VACUUM to optimize file size
- Connection pooling for concurrent access

### **Security**
- No sensitive data stored (code snippets only)
- Read-only mode for reports
- Transaction support for data integrity

---

## SQL Initialization Script

```sql
-- Enable foreign keys
PRAGMA foreign_keys = ON;

-- Performance optimizations
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA cache_size = -64000;  -- 64MB cache
PRAGMA temp_store = MEMORY;

-- Create all tables in order
-- (All CREATE TABLE statements from above)

-- Create version tracking
CREATE TABLE schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO schema_version (version) VALUES (1);
```

---

## Next Steps

1. **Review & Approve** - Review this schema and suggest changes
2. **Implementation** - Create database layer and ORM models
3. **Testing** - Test with sample data
4. **Migration** - Migrate existing in-memory data to DB
5. **Enhancement** - Add new features (history, comparison, etc.)

---

**Estimated Implementation Time:**
- Database setup: 1-2 hours
- Data layer implementation: 4-6 hours
- API integration: 3-4 hours
- Frontend updates: 4-6 hours
- Testing: 2-3 hours

**Total: ~15-20 hours**
