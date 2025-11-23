-- Add BMS (Basic Mapping Support) analysis tables
-- Created: 2025-01-22
-- Description: Tables for storing BMS mapset, map, and field definitions

-- BMS Mapsets (top-level container)
CREATE TABLE IF NOT EXISTS bms_mapsets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    mapset_name TEXT NOT NULL,
    file_path TEXT,
    language TEXT DEFAULT 'BMS',
    mode TEXT,  -- DSECT, MAP, etc.
    storage TEXT,  -- BASE, TIOAPFX
    ctrl TEXT,  -- FREEKB, FRSET, etc.
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id, mapset_name)
);

CREATE INDEX IF NOT EXISTS idx_bms_mapsets_analysis
ON bms_mapsets(analysis_run_id);

CREATE INDEX IF NOT EXISTS idx_bms_mapsets_name
ON bms_mapsets(mapset_name);


-- BMS Maps (screen definitions within a mapset)
CREATE TABLE IF NOT EXISTS bms_maps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    mapset_id INTEGER NOT NULL,
    map_name TEXT NOT NULL,
    size_line INTEGER,  -- Screen size (lines)
    size_column INTEGER,  -- Screen size (columns)
    line INTEGER,  -- Starting line in file
    map_type TEXT,  -- IN, OUT, INOUT
    justify TEXT,  -- LEFT, RIGHT
    lang TEXT,  -- COBOL, PLI, ASM
    tioapfx TEXT,  -- YES, NO
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (mapset_id) REFERENCES bms_mapsets(id) ON DELETE CASCADE,
    UNIQUE(mapset_id, map_name)
);

CREATE INDEX IF NOT EXISTS idx_bms_maps_mapset
ON bms_maps(mapset_id);

CREATE INDEX IF NOT EXISTS idx_bms_maps_name
ON bms_maps(map_name);


-- BMS Fields (individual screen fields)
CREATE TABLE IF NOT EXISTS bms_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    map_id INTEGER NOT NULL,
    field_name TEXT NOT NULL,
    position_line INTEGER,  -- Line position on screen
    position_column INTEGER,  -- Column position on screen
    length INTEGER,  -- Field length
    pic_clause TEXT,  -- PIC clause for data definition
    initial_value TEXT,  -- Initial value
    attr TEXT,  -- Attributes (BRT, DRK, NORM, etc.)
    attrb TEXT,  -- Extended attributes
    color TEXT,  -- Field color
    hilight TEXT,  -- Highlight (BLINK, REVERSE, UNDERLINE)
    justify TEXT,  -- LEFT, RIGHT
    occurs INTEGER,  -- Number of occurrences (for arrays)
    field_order INTEGER,  -- Order in map
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (map_id) REFERENCES bms_maps(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_bms_fields_map
ON bms_fields(map_id);

CREATE INDEX IF NOT EXISTS idx_bms_fields_name
ON bms_fields(field_name);

CREATE INDEX IF NOT EXISTS idx_bms_fields_position
ON bms_fields(position_line, position_column);


-- BMS Field Attributes (detailed attribute information)
CREATE TABLE IF NOT EXISTS bms_field_attributes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    field_id INTEGER NOT NULL,
    attribute_type TEXT NOT NULL,  -- ASKIP, NUM, PROT, UNPROT, etc.
    attribute_value TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (field_id) REFERENCES bms_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_bms_field_attrs_field
ON bms_field_attributes(field_id);


-- BMS Program Usage (which programs use which mapsets)
-- Enhanced version of program_bms_usage table
CREATE TABLE IF NOT EXISTS bms_program_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    program_id INTEGER NOT NULL,
    mapset_id INTEGER NOT NULL,
    map_id INTEGER,  -- Optional: specific map used
    usage_type TEXT,  -- SEND, RECEIVE, BOTH
    line_number INTEGER,
    code_snippet TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (program_id) REFERENCES programs(id) ON DELETE CASCADE,
    FOREIGN KEY (mapset_id) REFERENCES bms_mapsets(id) ON DELETE CASCADE,
    FOREIGN KEY (map_id) REFERENCES bms_maps(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_bms_prog_usage_program
ON bms_program_usage(program_id);

CREATE INDEX IF NOT EXISTS idx_bms_prog_usage_mapset
ON bms_program_usage(mapset_id);


-- BMS Statistics Summary
CREATE TABLE IF NOT EXISTS bms_statistics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_run_id INTEGER NOT NULL,
    total_mapsets INTEGER DEFAULT 0,
    total_maps INTEGER DEFAULT 0,
    total_fields INTEGER DEFAULT 0,
    avg_fields_per_map REAL DEFAULT 0,
    max_fields_in_map INTEGER DEFAULT 0,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (analysis_run_id) REFERENCES analysis_runs(id) ON DELETE CASCADE,
    UNIQUE(analysis_run_id)
);

CREATE INDEX IF NOT EXISTS idx_bms_stats_analysis
ON bms_statistics(analysis_run_id);
