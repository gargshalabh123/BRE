-- Add JCL (Job Control Language) analysis tables
-- Created: 2025-01-22
-- Description: Tables for storing JCL job definitions, steps, and dataset usage

-- JCL Jobs (top-level job definitions)
CREATE TABLE IF NOT EXISTS jcl_jobs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    job_name TEXT NOT NULL,
    file_path TEXT,
    class TEXT,  -- Job class
    msgclass TEXT,  -- Message class
    msglevel TEXT,  -- Message level
    notify TEXT,  -- Notification user
    region TEXT,  -- Region size
    time TEXT,  -- Time limit
    cond TEXT,  -- Condition code
    user TEXT,  -- User ID
    password TEXT,  -- Password (if present, should be masked)
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id, job_name)
);

CREATE INDEX IF NOT EXISTS idx_jcl_jobs_analysis
ON jcl_jobs(analysis_run_id);

CREATE INDEX IF NOT EXISTS idx_jcl_jobs_name
ON jcl_jobs(job_name);


-- JCL Steps (execution steps within a job)
CREATE TABLE IF NOT EXISTS jcl_steps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    job_id INTEGER NOT NULL,
    step_name TEXT NOT NULL,
    program_name TEXT,  -- Program executed (from EXEC PGM=)
    proc_name TEXT,  -- Procedure called (from EXEC)
    parm TEXT,  -- Parameters passed
    cond TEXT,  -- Condition code for step
    region TEXT,  -- Region for step
    time TEXT,  -- Time limit for step
    step_order INTEGER,  -- Execution order (1, 2, 3...)
    line_number INTEGER,  -- Line in JCL file
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (job_id) REFERENCES jcl_jobs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_steps_job
ON jcl_steps(job_id);

CREATE INDEX IF NOT EXISTS idx_jcl_steps_program
ON jcl_steps(program_name);

CREATE INDEX IF NOT EXISTS idx_jcl_steps_order
ON jcl_steps(step_order);


-- JCL Datasets (DD statements - data definitions)
CREATE TABLE IF NOT EXISTS jcl_datasets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    step_id INTEGER NOT NULL,
    dd_name TEXT NOT NULL,  -- DD name (SYSIN, SYSOUT, etc.)
    dataset_name TEXT,  -- DSN= value
    disposition TEXT,  -- DISP= value (NEW, OLD, SHR, etc.)
    disp_normal TEXT,  -- Normal disposition
    disp_abnormal TEXT,  -- Abnormal disposition
    space TEXT,  -- SPACE= parameters
    unit TEXT,  -- UNIT= value
    vol TEXT,  -- VOL= value
    dcb TEXT,  -- DCB= parameters
    recfm TEXT,  -- Record format
    lrecl INTEGER,  -- Logical record length
    blksize INTEGER,  -- Block size
    is_temp BOOLEAN DEFAULT 0,  -- Temporary dataset (&&name)
    is_generation BOOLEAN DEFAULT 0,  -- Generation data group (+1, 0, -1)
    line_number INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (step_id) REFERENCES jcl_steps(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_datasets_step
ON jcl_datasets(step_id);

CREATE INDEX IF NOT EXISTS idx_jcl_datasets_dsn
ON jcl_datasets(dataset_name);

CREATE INDEX IF NOT EXISTS idx_jcl_datasets_dd
ON jcl_datasets(dd_name);


-- JCL Procedures (cataloged procedures)
CREATE TABLE IF NOT EXISTS jcl_procs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    proc_name TEXT NOT NULL,
    file_path TEXT,
    proc_type TEXT,  -- INLINE, CATALOGED
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id, proc_name)
);

CREATE INDEX IF NOT EXISTS idx_jcl_procs_analysis
ON jcl_procs(analysis_run_id);

CREATE INDEX IF NOT EXISTS idx_jcl_procs_name
ON jcl_procs(proc_name);


-- JCL Proc Steps (steps within a procedure)
CREATE TABLE IF NOT EXISTS jcl_proc_steps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    proc_id INTEGER NOT NULL,
    step_name TEXT NOT NULL,
    program_name TEXT,
    parm TEXT,
    step_order INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (proc_id) REFERENCES jcl_procs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_proc_steps_proc
ON jcl_proc_steps(proc_id);


-- JCL Symbols/Variables (SET statements)
CREATE TABLE IF NOT EXISTS jcl_symbols (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    job_id INTEGER NOT NULL,
    symbol_name TEXT NOT NULL,
    symbol_value TEXT,
    line_number INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (job_id) REFERENCES jcl_jobs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_symbols_job
ON jcl_symbols(job_id);


-- JCL Conditions (IF/THEN/ELSE/ENDIF)
CREATE TABLE IF NOT EXISTS jcl_conditions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    job_id INTEGER NOT NULL,
    condition_type TEXT,  -- IF, THEN, ELSE, ENDIF
    condition_expr TEXT,  -- Condition expression
    target_step TEXT,  -- Step being checked
    line_number INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (job_id) REFERENCES jcl_jobs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_conditions_job
ON jcl_conditions(job_id);


-- JCL Dependencies (job-to-job dependencies via datasets)
CREATE TABLE IF NOT EXISTS jcl_dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_job_id INTEGER NOT NULL,  -- Job that creates dataset
    target_job_id INTEGER NOT NULL,  -- Job that uses dataset
    dataset_name TEXT NOT NULL,
    dependency_type TEXT,  -- TRIGGER, INPUT, OUTPUT
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (source_job_id) REFERENCES jcl_jobs(id) ON DELETE CASCADE,
    FOREIGN KEY (target_job_id) REFERENCES jcl_jobs(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_jcl_deps_source
ON jcl_dependencies(source_job_id);

CREATE INDEX IF NOT EXISTS idx_jcl_deps_target
ON jcl_dependencies(target_job_id);


-- JCL Statistics Summary
CREATE TABLE IF NOT EXISTS jcl_statistics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    total_jobs INTEGER DEFAULT 0,
    total_steps INTEGER DEFAULT 0,
    total_datasets INTEGER DEFAULT 0,
    total_procs INTEGER DEFAULT 0,
    avg_steps_per_job REAL DEFAULT 0,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id)
);

CREATE INDEX IF NOT EXISTS idx_jcl_stats_analysis
ON jcl_statistics(analysis_run_id);
