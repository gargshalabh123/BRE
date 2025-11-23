-- ============================================
-- Migration 001: Enhanced Dependency Tracking
-- Created: 2025-01-22
-- Description: Adds first-class entities for programs, DB objects,
--              copybooks, and comprehensive relationship tracking
-- ============================================

-- Enable foreign keys
PRAGMA foreign_keys = ON;

-- ============================================
-- 1. PROGRAM/MODULE REGISTRY
-- ============================================

CREATE TABLE IF NOT EXISTS programs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    program_name TEXT NOT NULL,
    program_type TEXT NOT NULL,  -- 'COBOL_PROGRAM', 'PYTHON_MODULE', 'JS_MODULE', etc.
    file_id INTEGER,  -- Link to files table
    file_path TEXT,
    language TEXT,
    entry_point BOOLEAN DEFAULT 0,  -- Is this a main entry point?
    cics_transaction_id TEXT,  -- For CICS programs
    jcl_job_name TEXT,  -- For batch programs
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id, program_name)
);

CREATE INDEX IF NOT EXISTS idx_programs_name ON programs(program_name);
CREATE INDEX IF NOT EXISTS idx_programs_type ON programs(program_type);
CREATE INDEX IF NOT EXISTS idx_programs_entry ON programs(entry_point);
CREATE INDEX IF NOT EXISTS idx_programs_cics_trans ON programs(cics_transaction_id);
CREATE INDEX IF NOT EXISTS idx_programs_run ON programs(analysis_run_id);

-- ============================================
-- 2. DATABASE OBJECTS REGISTRY
-- ============================================

CREATE TABLE IF NOT EXISTS database_objects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    object_name TEXT NOT NULL,
    object_type TEXT NOT NULL,  -- 'TABLE', 'VIEW', 'PROCEDURE', 'FUNCTION', 'VSAM_FILE', 'SEGMENT'
    schema_name TEXT,
    database_type TEXT,  -- 'DB2', 'IMS', 'VSAM', 'IDMS', 'SQL_SERVER', etc.
    definition_file_id INTEGER,  -- Where it's defined (DDL file)
    columns_json TEXT,  -- JSON array of column names and types
    primary_key TEXT,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (definition_file_id) REFERENCES files(id) ON DELETE SET NULL,
    UNIQUE(analysis_run_id, database_type, object_name)
);

CREATE INDEX IF NOT EXISTS idx_db_objects_name ON database_objects(object_name);
CREATE INDEX IF NOT EXISTS idx_db_objects_type ON database_objects(object_type);
CREATE INDEX IF NOT EXISTS idx_db_objects_db_type ON database_objects(database_type);
CREATE INDEX IF NOT EXISTS idx_db_objects_run ON database_objects(analysis_run_id);

-- ============================================
-- 3. COPYBOOK REGISTRY
-- ============================================

CREATE TABLE IF NOT EXISTS copybooks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    copybook_name TEXT NOT NULL,
    file_id INTEGER,
    file_path TEXT,
    copybook_type TEXT,  -- 'DATA_STRUCTURE', 'CONSTANTS', 'SQL', 'WORKING_STORAGE'
    data_structures_json TEXT,  -- JSON of data structures defined
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id, copybook_name)
);

CREATE INDEX IF NOT EXISTS idx_copybooks_name ON copybooks(copybook_name);
CREATE INDEX IF NOT EXISTS idx_copybooks_type ON copybooks(copybook_type);
CREATE INDEX IF NOT EXISTS idx_copybooks_run ON copybooks(analysis_run_id);

-- ============================================
-- 4. PROGRAM-TO-PROGRAM RELATIONSHIPS
-- ============================================

CREATE TABLE IF NOT EXISTS program_calls (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    caller_program_id INTEGER NOT NULL,  -- Who is calling
    caller_file_id INTEGER NOT NULL,
    caller_line_number INTEGER,
    callee_program_id INTEGER,  -- Who is being called (NULL if external/unknown)
    callee_program_name TEXT NOT NULL,  -- Name even if not found
    call_type TEXT NOT NULL,  -- 'STATIC_CALL', 'DYNAMIC_CALL', 'CICS_XCTL', 'CICS_LINK', 'FUNCTION_CALL', 'METHOD_CALL'
    call_mechanism TEXT,  -- 'CALL', 'PERFORM', 'EXEC CICS XCTL', 'import', 'require'
    parameters_json TEXT,  -- JSON array of parameters passed
    commarea_structure TEXT,  -- For CICS calls
    return_type TEXT,  -- For functions
    call_signature TEXT,  -- Full call statement
    conditional BOOLEAN DEFAULT 0,  -- Is this call conditional (in IF block)?
    frequency_hint TEXT,  -- 'ONCE', 'LOOP', 'RECURSIVE'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (caller_program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (callee_program_id) REFERENCES programs(id) ON DELETE SET NULL,
    FOREIGN KEY (caller_file_id) REFERENCES files(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_prog_calls_caller ON program_calls(caller_program_id);
CREATE INDEX IF NOT EXISTS idx_prog_calls_callee ON program_calls(callee_program_id);
CREATE INDEX IF NOT EXISTS idx_prog_calls_callee_name ON program_calls(callee_program_name);
CREATE INDEX IF NOT EXISTS idx_prog_calls_type ON program_calls(call_type);
CREATE INDEX IF NOT EXISTS idx_prog_calls_run ON program_calls(analysis_run_id);

-- ============================================
-- 5. PROGRAM-TO-DATABASE RELATIONSHIPS
-- ============================================

CREATE TABLE IF NOT EXISTS program_db_access (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    program_id INTEGER NOT NULL,
    file_id INTEGER NOT NULL,
    line_number INTEGER,
    db_object_id INTEGER,  -- Link to database_objects (NULL if not found/external)
    db_object_name TEXT NOT NULL,  -- Name even if not in registry
    db_object_type TEXT,  -- 'TABLE', 'VIEW', 'PROCEDURE', etc.
    access_type TEXT NOT NULL,  -- 'SELECT', 'INSERT', 'UPDATE', 'DELETE', 'CALL', 'EXEC'
    access_mode TEXT,  -- 'READ', 'WRITE', 'READ_WRITE'
    columns_accessed TEXT,  -- Comma-separated column names
    where_clause TEXT,  -- Simplified WHERE condition
    join_tables TEXT,  -- Other tables in JOIN
    sql_statement TEXT,  -- Full SQL (truncated to 500 chars)
    transaction_context TEXT,  -- 'COMMIT', 'ROLLBACK', 'SYNCPOINT'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (db_object_id) REFERENCES database_objects(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_prog_db_program ON program_db_access(program_id);
CREATE INDEX IF NOT EXISTS idx_prog_db_object ON program_db_access(db_object_id);
CREATE INDEX IF NOT EXISTS idx_prog_db_object_name ON program_db_access(db_object_name);
CREATE INDEX IF NOT EXISTS idx_prog_db_access_type ON program_db_access(access_type);
CREATE INDEX IF NOT EXISTS idx_prog_db_run ON program_db_access(analysis_run_id);

-- ============================================
-- 6. PROGRAM-TO-COPYBOOK RELATIONSHIPS
-- ============================================

CREATE TABLE IF NOT EXISTS program_copybook_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    program_id INTEGER NOT NULL,
    file_id INTEGER NOT NULL,
    line_number INTEGER,
    copybook_id INTEGER,  -- Link to copybooks table (NULL if not found)
    copybook_name TEXT NOT NULL,  -- Name even if not found
    usage_context TEXT,  -- 'WORKING_STORAGE', 'LINKAGE_SECTION', 'FILE_SECTION', 'PROCEDURE'
    replacing_clause TEXT,  -- REPLACING content if any
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_prog_copy_program ON program_copybook_usage(program_id);
CREATE INDEX IF NOT EXISTS idx_prog_copy_copybook ON program_copybook_usage(copybook_id);
CREATE INDEX IF NOT EXISTS idx_prog_copy_name ON program_copybook_usage(copybook_name);
CREATE INDEX IF NOT EXISTS idx_prog_copy_run ON program_copybook_usage(analysis_run_id);

-- ============================================
-- 7. BMS MAP REGISTRY & USAGE
-- ============================================

CREATE TABLE IF NOT EXISTS bms_maps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    map_name TEXT NOT NULL,
    mapset_name TEXT NOT NULL,
    file_id INTEGER,  -- BMS source file
    map_type TEXT,  -- 'INPUT', 'OUTPUT', 'INOUT'
    fields_json TEXT,  -- JSON of field definitions
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE SET NULL,
    UNIQUE(analysis_run_id, mapset_name, map_name)
);

CREATE INDEX IF NOT EXISTS idx_bms_maps_mapset ON bms_maps(mapset_name);
CREATE INDEX IF NOT EXISTS idx_bms_maps_name ON bms_maps(map_name);
CREATE INDEX IF NOT EXISTS idx_bms_maps_run ON bms_maps(analysis_run_id);

CREATE TABLE IF NOT EXISTS program_bms_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    program_id INTEGER NOT NULL,
    file_id INTEGER NOT NULL,
    line_number INTEGER,
    bms_map_id INTEGER,
    map_name TEXT NOT NULL,
    mapset_name TEXT NOT NULL,
    operation TEXT,  -- 'SEND', 'RECEIVE'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (bms_map_id) REFERENCES bms_maps(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_prog_bms_program ON program_bms_usage(program_id);
CREATE INDEX IF NOT EXISTS idx_prog_bms_map ON program_bms_usage(bms_map_id);
CREATE INDEX IF NOT EXISTS idx_prog_bms_run ON program_bms_usage(analysis_run_id);

-- ============================================
-- 8. CALL GRAPH MATERIALIZED PATHS
-- ============================================

CREATE TABLE IF NOT EXISTS call_graph_paths (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    source_program_id INTEGER NOT NULL,
    target_program_id INTEGER NOT NULL,
    path_length INTEGER NOT NULL,  -- Number of hops
    path_json TEXT NOT NULL,  -- JSON array of program IDs in path
    path_description TEXT,  -- Human readable path
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (source_program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (target_program_id) REFERENCES programs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_call_paths_source ON call_graph_paths(source_program_id);
CREATE INDEX IF NOT EXISTS idx_call_paths_target ON call_graph_paths(target_program_id);
CREATE INDEX IF NOT EXISTS idx_call_paths_length ON call_graph_paths(path_length);
CREATE INDEX IF NOT EXISTS idx_call_paths_run ON call_graph_paths(analysis_run_id);

-- ============================================
-- 9. ENHANCE EXISTING BUSINESS RULES TABLE
-- ============================================

-- Add columns to existing business_rules table
ALTER TABLE business_rules ADD COLUMN program_id INTEGER REFERENCES programs(id);
ALTER TABLE business_rules ADD COLUMN db_object_id INTEGER REFERENCES database_objects(id);
ALTER TABLE business_rules ADD COLUMN copybook_id INTEGER REFERENCES copybooks(id);
ALTER TABLE business_rules ADD COLUMN rule_category TEXT;  -- 'VALIDATION', 'CALCULATION', 'AUTHORIZATION', 'WORKFLOW'
ALTER TABLE business_rules ADD COLUMN affected_fields TEXT;  -- Fields/variables involved
ALTER TABLE business_rules ADD COLUMN rule_expression TEXT;  -- Normalized rule expression

CREATE INDEX IF NOT EXISTS idx_business_rules_program ON business_rules(program_id);
CREATE INDEX IF NOT EXISTS idx_business_rules_db_object ON business_rules(db_object_id);
CREATE INDEX IF NOT EXISTS idx_business_rules_category ON business_rules(rule_category);

-- ============================================
-- 10. ADD SOURCE CODE STORAGE TO FILES TABLE
-- ============================================

-- Add source code column to files table (for future RAG support)
ALTER TABLE files ADD COLUMN source_code TEXT;
ALTER TABLE files ADD COLUMN source_code_hash TEXT;

CREATE INDEX IF NOT EXISTS idx_files_source_hash ON files(source_code_hash);

-- ============================================
-- 11. USEFUL VIEWS
-- ============================================

-- Program dependency summary view
CREATE VIEW IF NOT EXISTS v_program_dependency_summary AS
SELECT
    p.id as program_id,
    p.program_name,
    p.program_type,
    p.file_path,
    p.language,
    p.entry_point,

    -- Count outgoing calls
    (SELECT COUNT(*) FROM program_calls WHERE caller_program_id = p.id) as calls_made,

    -- Count incoming calls
    (SELECT COUNT(*) FROM program_calls WHERE callee_program_id = p.id) as called_by,

    -- Count DB accesses
    (SELECT COUNT(DISTINCT db_object_name) FROM program_db_access WHERE program_id = p.id) as db_objects_used,

    -- Count copybooks used
    (SELECT COUNT(*) FROM program_copybook_usage WHERE program_id = p.id) as copybooks_used,

    -- Count business rules
    (SELECT COUNT(*) FROM business_rules WHERE program_id = p.id) as business_rules_count,

    p.analysis_run_id
FROM programs p;

-- Database object usage view
CREATE VIEW IF NOT EXISTS v_db_object_usage AS
SELECT
    dbo.id as db_object_id,
    dbo.object_name,
    dbo.object_type,
    dbo.database_type,

    -- Count programs using this object
    (SELECT COUNT(DISTINCT program_id) FROM program_db_access WHERE db_object_id = dbo.id) as used_by_programs,

    -- Count read operations
    (SELECT COUNT(*) FROM program_db_access WHERE db_object_id = dbo.id AND access_mode = 'READ') as read_count,

    -- Count write operations
    (SELECT COUNT(*) FROM program_db_access WHERE db_object_id = dbo.id AND access_mode = 'WRITE') as write_count,

    dbo.analysis_run_id
FROM database_objects dbo;

-- Copybook usage view
CREATE VIEW IF NOT EXISTS v_copybook_usage AS
SELECT
    c.id as copybook_id,
    c.copybook_name,
    c.copybook_type,
    c.file_path,

    -- Count programs using this copybook
    (SELECT COUNT(DISTINCT program_id) FROM program_copybook_usage WHERE copybook_id = c.id) as used_by_programs,

    c.analysis_run_id
FROM copybooks c;

-- ============================================
-- 12. UPDATE SCHEMA VERSION
-- ============================================

INSERT INTO schema_version (version, description)
VALUES (2, 'Enhanced dependency tracking with first-class entities');

-- Migration complete
