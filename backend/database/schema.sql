-- Business Rules Extraction - SQLite Database Schema
-- Version 1.0

-- Enable foreign keys and performance optimizations
PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA cache_size = -64000;  -- 64MB cache
PRAGMA temp_store = MEMORY;

-- ============================================
-- USER MANAGEMENT TABLES
-- ============================================

-- Roles for access control
CREATE TABLE roles (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    role_name TEXT NOT NULL UNIQUE,
    description TEXT,
    can_upload BOOLEAN DEFAULT 0,
    can_analyze BOOLEAN DEFAULT 0,
    can_delete BOOLEAN DEFAULT 0,
    can_export BOOLEAN DEFAULT 1,
    can_manage_users BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_roles_name ON roles(role_name);

-- Users
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    email TEXT UNIQUE,
    password_hash TEXT NOT NULL,
    role_id INTEGER NOT NULL,
    full_name TEXT,
    is_active BOOLEAN DEFAULT 1,
    last_login TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (role_id) REFERENCES roles(id)
);

CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_role ON users(role_id);
CREATE INDEX idx_users_active ON users(is_active);

-- User sessions for tracking login activity
CREATE TABLE user_sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    session_token TEXT NOT NULL UNIQUE,
    ip_address TEXT,
    user_agent TEXT,
    expires_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE INDEX idx_sessions_user ON user_sessions(user_id);
CREATE INDEX idx_sessions_token ON user_sessions(session_token);
CREATE INDEX idx_sessions_expires ON user_sessions(expires_at);

-- User activity audit log
CREATE TABLE user_activity_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    activity_type TEXT NOT NULL,  -- login, logout, upload, analyze, delete, export
    activity_description TEXT,
    ip_address TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

CREATE INDEX idx_activity_user ON user_activity_log(user_id);
CREATE INDEX idx_activity_type ON user_activity_log(activity_type);
CREATE INDEX idx_activity_date ON user_activity_log(created_at);

-- ============================================
-- PROJECT AND ANALYSIS TABLES
-- ============================================

-- Projects
CREATE TABLE projects (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    description TEXT,
    created_by INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    UNIQUE(name)
);

CREATE INDEX idx_projects_name ON projects(name);
CREATE INDEX idx_projects_created_by ON projects(created_by);

-- Analysis runs
CREATE TABLE analysis_runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL,
    upload_id TEXT NOT NULL UNIQUE,
    upload_filename TEXT,
    uploaded_by INTEGER,
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
    FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
    FOREIGN KEY (uploaded_by) REFERENCES users(id) ON DELETE SET NULL
);

CREATE INDEX idx_analysis_runs_project ON analysis_runs(project_id);
CREATE INDEX idx_analysis_runs_upload_id ON analysis_runs(upload_id);
CREATE INDEX idx_analysis_runs_date ON analysis_runs(analysis_date);
CREATE INDEX idx_analysis_runs_status ON analysis_runs(status);
CREATE INDEX idx_analysis_runs_uploaded_by ON analysis_runs(uploaded_by);

-- Files
CREATE TABLE files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    file_path TEXT NOT NULL,
    file_name TEXT NOT NULL,
    file_type TEXT,
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

-- Dependencies
CREATE TABLE dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    dependency_type TEXT NOT NULL,
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

-- Dependency parameters
CREATE TABLE dependency_parameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    dependency_id INTEGER NOT NULL,
    parameter_key TEXT,
    parameter_value TEXT,
    parameter_order INTEGER DEFAULT 0,
    FOREIGN KEY (dependency_id) REFERENCES dependencies(id) ON DELETE CASCADE
);

CREATE INDEX idx_dep_params_dependency ON dependency_parameters(dependency_id);

-- Database operations
CREATE TABLE database_operations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    operation_type TEXT NOT NULL,
    category TEXT,
    line_number INTEGER,
    query_text TEXT,
    target_table TEXT,
    target_segment TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_db_ops_file ON database_operations(file_id);
CREATE INDEX idx_db_ops_type ON database_operations(operation_type);
CREATE INDEX idx_db_ops_category ON database_operations(category);
CREATE INDEX idx_db_ops_table ON database_operations(target_table);
CREATE INDEX idx_db_ops_analysis_run ON database_operations(analysis_run_id);

-- DB operation parameters
CREATE TABLE db_operation_parameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    db_operation_id INTEGER NOT NULL,
    parameter_key TEXT,
    parameter_value TEXT,
    parameter_order INTEGER DEFAULT 0,
    FOREIGN KEY (db_operation_id) REFERENCES database_operations(id) ON DELETE CASCADE
);

CREATE INDEX idx_db_op_params_operation ON db_operation_parameters(db_operation_id);

-- Business rules
CREATE TABLE business_rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    rule_type TEXT NOT NULL,
    line_number INTEGER,
    condition_name TEXT,
    condition_value TEXT,
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

-- Complexity details
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

-- AI insights
CREATE TABLE ai_insights (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    file_id INTEGER,
    insight_type TEXT NOT NULL,
    title TEXT NOT NULL,
    description TEXT,
    severity TEXT,
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

-- Error messages
CREATE TABLE error_messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    analysis_run_id INTEGER NOT NULL,
    message_type TEXT,
    line_number INTEGER,
    message_text TEXT,
    variables_referenced TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_error_messages_file ON error_messages(file_id);
CREATE INDEX idx_error_messages_type ON error_messages(message_type);
CREATE INDEX idx_error_messages_analysis_run ON error_messages(analysis_run_id);

-- Code snippets cache
CREATE TABLE code_snippets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id INTEGER NOT NULL,
    start_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    snippet_text TEXT NOT NULL,
    snippet_hash TEXT,
    snippet_type TEXT,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE
);

CREATE INDEX idx_snippets_file ON code_snippets(file_id);
CREATE INDEX idx_snippets_hash ON code_snippets(snippet_hash);

-- Analysis metadata
CREATE TABLE analysis_metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    metadata_key TEXT NOT NULL,
    metadata_value TEXT,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_metadata_analysis_run ON analysis_metadata(analysis_run_id);
CREATE INDEX idx_metadata_key ON analysis_metadata(metadata_key);

-- Schema version tracking
CREATE TABLE schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    description TEXT
);

INSERT INTO schema_version (version, description) VALUES (1, 'Initial schema with user management');

-- ============================================
-- DEFAULT DATA
-- ============================================

-- Insert default roles
INSERT INTO roles (role_name, description, can_upload, can_analyze, can_delete, can_export, can_manage_users) VALUES
    ('admin', 'Administrator with full access', 1, 1, 1, 1, 1),
    ('analyst', 'Code analyst with upload and analyze permissions', 1, 1, 0, 1, 0),
    ('viewer', 'Read-only viewer with export permissions', 0, 0, 0, 1, 0),
    ('developer', 'Developer with upload and analyze permissions', 1, 1, 0, 1, 0);

-- Insert default admin user (password: admin123 - CHANGE THIS IN PRODUCTION!)
-- Password hash for 'admin123' using bcrypt
INSERT INTO users (username, email, password_hash, role_id, full_name, is_active) VALUES
    ('admin', 'admin@bre.local', '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LewY5GyYIx.Y7P0JO', 1, 'System Administrator', 1);
