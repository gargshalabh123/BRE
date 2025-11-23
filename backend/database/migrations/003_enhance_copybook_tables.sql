-- Enhance copybook tables with detailed structure information
-- Created: 2025-01-22
-- Description: Add detailed field structure, REDEFINES, and hierarchy information

-- Copybook Fields (detailed structure hierarchy)
CREATE TABLE IF NOT EXISTS copybook_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    parent_field_id INTEGER,  -- For hierarchical structure (01-49 levels)
    level_number INTEGER NOT NULL,  -- 01, 05, 10, 15, etc.
    field_name TEXT,
    pic_clause TEXT,  -- PIC X(10), PIC 9(5)V99, etc.
    usage_clause TEXT,  -- COMP, COMP-3, DISPLAY, BINARY
    value_clause TEXT,  -- VALUE 'ABC', VALUE ZEROS
    occurs_count INTEGER,  -- OCCURS clause
    occurs_depending TEXT,  -- DEPENDING ON field
    redefines_field TEXT,  -- REDEFINES clause
    justified TEXT,  -- JUSTIFIED RIGHT
    blank_when_zero BOOLEAN DEFAULT 0,
    sign_clause TEXT,  -- SIGN LEADING SEPARATE
    synchronized BOOLEAN DEFAULT 0,
    indexed_by TEXT,  -- INDEXED BY clause
    field_order INTEGER,  -- Order in copybook
    line_number INTEGER,
    computed_length INTEGER,  -- Calculated field length
    field_path TEXT,  -- Full hierarchical path (e.g., "CUSTOMER-RECORD.ADDRESS.STREET")
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (parent_field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_copybook_fields_copybook
ON copybook_fields(copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_fields_parent
ON copybook_fields(parent_field_id);

CREATE INDEX IF NOT EXISTS idx_copybook_fields_level
ON copybook_fields(level_number);

CREATE INDEX IF NOT EXISTS idx_copybook_fields_name
ON copybook_fields(field_name);


-- Copybook REDEFINES Relationships
CREATE TABLE IF NOT EXISTS copybook_redefines (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    redefines_field_id INTEGER NOT NULL,  -- Field that redefines
    original_field_id INTEGER NOT NULL,  -- Original field being redefined
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (redefines_field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE,
    FOREIGN KEY (original_field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_copybook_redefines_copybook
ON copybook_redefines(copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_redefines_field
ON copybook_redefines(redefines_field_id);


-- Copybook Constants (VALUE clauses at 01/77/88 level)
CREATE TABLE IF NOT EXISTS copybook_constants (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    field_id INTEGER NOT NULL,
    constant_name TEXT NOT NULL,
    constant_value TEXT,
    constant_type TEXT,  -- 88_LEVEL, VALUE_CLAUSE
    line_number INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_copybook_constants_copybook
ON copybook_constants(copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_constants_field
ON copybook_constants(field_id);


-- Copybook Arrays (OCCURS clauses)
CREATE TABLE IF NOT EXISTS copybook_arrays (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    field_id INTEGER NOT NULL,
    array_name TEXT NOT NULL,
    occurs_min INTEGER,  -- Minimum occurrences
    occurs_max INTEGER,  -- Maximum occurrences
    depending_on_field TEXT,  -- DEPENDING ON clause
    indexed_by TEXT,  -- INDEXED BY clause
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_copybook_arrays_copybook
ON copybook_arrays(copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_arrays_field
ON copybook_arrays(field_id);


-- Copybook Cross References (COPY statements within copybooks)
CREATE TABLE IF NOT EXISTS copybook_includes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    parent_copybook_id INTEGER NOT NULL,
    included_copybook_id INTEGER,
    included_copybook_name TEXT NOT NULL,
    line_number INTEGER,
    replacing_clause TEXT,  -- REPLACING clause if present
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (parent_copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (included_copybook_id) REFERENCES copybooks(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS idx_copybook_includes_parent
ON copybook_includes(parent_copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_includes_child
ON copybook_includes(included_copybook_id);


-- Copybook Computational Fields (COMP, COMP-3, etc.)
CREATE TABLE IF NOT EXISTS copybook_comp_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    field_id INTEGER NOT NULL,
    comp_type TEXT NOT NULL,  -- COMP, COMP-1, COMP-2, COMP-3, COMP-4, COMP-5
    pic_clause TEXT,
    bytes_used INTEGER,  -- Actual bytes consumed
    description TEXT,  -- Binary, Floating point, Packed decimal, etc.
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    FOREIGN KEY (field_id) REFERENCES copybook_fields(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_copybook_comp_fields_copybook
ON copybook_comp_fields(copybook_id);

CREATE INDEX IF NOT EXISTS idx_copybook_comp_fields_type
ON copybook_comp_fields(comp_type);


-- Copybook Statistics (enhanced)
CREATE TABLE IF NOT EXISTS copybook_field_statistics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    copybook_id INTEGER NOT NULL,
    total_fields INTEGER DEFAULT 0,
    max_level INTEGER DEFAULT 0,
    total_redefines INTEGER DEFAULT 0,
    total_occurs INTEGER DEFAULT 0,
    total_comp_fields INTEGER DEFAULT 0,
    total_88_levels INTEGER DEFAULT 0,
    estimated_record_length INTEGER DEFAULT 0,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (copybook_id) REFERENCES copybooks(id) ON DELETE CASCADE,
    UNIQUE(copybook_id)
);

CREATE INDEX IF NOT EXISTS idx_copybook_field_stats_copybook
ON copybook_field_statistics(copybook_id);
