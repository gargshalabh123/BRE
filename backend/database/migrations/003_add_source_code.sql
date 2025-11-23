-- Migration: Add source code storage
-- Stores full source code for each file to enable agentic AI analysis

-- Add source_code column to files table
ALTER TABLE files ADD COLUMN source_code TEXT;

-- Create index for faster file content access
CREATE INDEX idx_files_has_source ON files(analysis_run_id) WHERE source_code IS NOT NULL;

-- Update schema version
INSERT INTO schema_version (version, description) VALUES (3, 'Added source code storage for agentic AI analysis');
