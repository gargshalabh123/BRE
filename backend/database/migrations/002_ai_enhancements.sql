-- Migration: Enhanced AI Analysis Support
-- Adds columns for scoped analysis and caching

-- Add new columns to ai_insights table
ALTER TABLE ai_insights ADD COLUMN scope_type TEXT;  -- 'file', 'rule', 'dependency', 'query', 'codebase'
ALTER TABLE ai_insights ADD COLUMN scope_id TEXT;    -- ID of the specific item being analyzed
ALTER TABLE ai_insights ADD COLUMN provider TEXT;    -- 'openai', 'anthropic', 'gemini'
ALTER TABLE ai_insights ADD COLUMN model TEXT;       -- Model name used
ALTER TABLE ai_insights ADD COLUMN cached BOOLEAN DEFAULT 0;

-- Create AI analysis cache table
CREATE TABLE IF NOT EXISTS ai_analysis_cache (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    item_type TEXT NOT NULL,  -- 'file', 'rule', 'dependency', 'query'
    item_id TEXT NOT NULL,     -- Unique identifier for the item
    item_hash TEXT,            -- Hash of item content for cache invalidation
    analysis_text TEXT NOT NULL,
    provider TEXT NOT NULL,
    model TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    accessed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    access_count INTEGER DEFAULT 1,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE
);

CREATE INDEX idx_ai_cache_run ON ai_analysis_cache(analysis_run_id);
CREATE INDEX idx_ai_cache_item ON ai_analysis_cache(item_type, item_id);
CREATE INDEX idx_ai_cache_hash ON ai_analysis_cache(item_hash);
CREATE INDEX idx_ai_cache_created ON ai_analysis_cache(created_at);

-- Create conversation history table for chat feature
CREATE TABLE IF NOT EXISTS ai_conversations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    user_id INTEGER,
    session_id TEXT NOT NULL,
    message_role TEXT NOT NULL,  -- 'user' or 'assistant'
    message_content TEXT NOT NULL,
    context_used TEXT,  -- JSON array of file paths used as context
    provider TEXT,
    model TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

CREATE INDEX idx_conversations_run ON ai_conversations(analysis_run_id);
CREATE INDEX idx_conversations_session ON ai_conversations(session_id);
CREATE INDEX idx_conversations_created ON ai_conversations(created_at);

-- Update schema version
INSERT INTO schema_version (version, description) VALUES (2, 'Enhanced AI analysis support with caching and conversations');
