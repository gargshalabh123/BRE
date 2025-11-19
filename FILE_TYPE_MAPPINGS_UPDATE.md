# File Type Mappings Update

## Problem
COBOL-related files like `.cpy`, `.jcl`, `.bms` were showing as "Unknown" type instead of being properly identified as COBOL ecosystem files.

## Solution
Updated file type mappings across all analyzers to properly recognize mainframe and COBOL-related file types.

## Files Modified

### 1. `backend/analyzers/code_analyzer.py`

**Added comprehensive file type mappings:**

#### COBOL and Mainframe Files:
- `.cbl`, `.cob`, `.cobol` → `COBOL Program`
- `.cpy` → `COBOL Copybook`
- `.jcl` → `JCL (Job Control Language)`
- `.bms` → `BMS (Basic Mapping Support)`
- `.prc` → `COBOL Procedure`
- `.dclgen` → `DB2 DCLGEN`
- `.mfs` → `IMS MFS`

#### AS/400 and RPG Files:
- `.rpg`, `.rpgle`, `.rpglec` → `RPG` / `RPG ILE`
- `.sqlrpgle` → `RPG with SQL`
- `.dspf` → `Display File`
- `.prtf` → `Print File`
- `.lf` → `Logical File`
- `.pf` → `Physical File`
- `.cmd` → `CL Command`
- `.clle` → `CL ILE`

#### SQL and Database:
- `.sql` → `SQL`
- `.ddl` → `DDL (Data Definition)`
- `.dml` → `DML (Data Manipulation)`
- `.pls` → `PL/SQL`
- `.pkb` → `PL/SQL Package Body`
- `.pks` → `PL/SQL Package Spec`

#### Modern Languages:
- `.java`, `.py`, `.js`, `.jsx`, `.ts`, `.tsx`, `.c`, `.cpp`, `.cs`, `.go`, `.rs`, etc.

#### Scripts:
- `.sh`, `.bash`, `.bat`, `.ps1`

#### Config Files:
- `.xml`, `.json`, `.yaml`, `.yml`, `.properties`, `.conf`, `.cfg`, `.ini`

### 2. `backend/analyzers/language_router.py`

**Updated LANGUAGE_MAP to route mainframe files correctly:**

```python
LANGUAGE_MAP = {
    # COBOL and mainframe files
    '.cbl': 'cobol',
    '.cob': 'cobol',
    '.cobol': 'cobol',
    '.cpy': 'cobol',      # COBOL Copybook
    '.jcl': 'jcl',        # Job Control Language
    '.bms': 'bms',        # Basic Mapping Support (CICS)
    '.prc': 'cobol',      # COBOL Procedure
    '.dclgen': 'cobol',   # DB2 DCLGEN
    '.mfs': 'mfs',        # IMS MFS

    # SQL and Database
    '.sql': 'sql',
    '.ddl': 'sql',
    '.dml': 'sql',
    '.pls': 'plsql',
    '.pkb': 'plsql',
    '.pks': 'plsql',

    # AS400/RPG
    '.rpg': 'as400',
    '.rpgle': 'as400',
    # ... (full list in code)
}
```

### 3. `backend/utils/zip_explorer.py`

**Updated `_is_text_file()` text_extensions to include:**
- All COBOL/mainframe extensions: `.cpy`, `.jcl`, `.bms`, `.prc`, `.dclgen`, `.mfs`
- All SQL/database extensions: `.pls`, `.pkb`, `.pks`
- All AS/400 extensions: `.clle`, `.cmd`, and others

### 4. `frontend/src/components/OverviewTab.tsx`

**Added two new tables:**

1. **"Files by Extension"** (lines 69-87)
   - Shows Extension, Type, Count, Total Size
   - Data from main analysis

2. **"Detailed Breakdown by Extension"** (lines 209-241)
   - Shows Extension, Language, Files, Total LOC, Avg LOC/File, Total Size
   - Data from ZIP explorer with LOC metrics

## Expected Results

After restarting the backend server, the UI will now show:

### Files by Extension Table:
| Extension | Type | Count | Total Size (KB) |
|-----------|------|-------|-----------------|
| .cbl | COBOL Program | 27 | 1234.56 |
| .cpy | COBOL Copybook | 62 | 345.67 |
| .jcl | JCL (Job Control Language) | 15 | 89.12 |
| .bms | BMS (Basic Mapping Support) | 8 | 45.23 |

### Detailed Breakdown by Extension Table:
| Extension | Language | Files | Total LOC | Avg LOC/File | Total Size (KB) |
|-----------|----------|-------|-----------|--------------|-----------------|
| .cbl | COBOL | 27 | 150,000 | 5,555 | 1234.56 |
| .cpy | COBOL | 62 | 23,000 | 371 | 345.67 |
| .jcl | JCL | 15 | 5,400 | 360 | 89.12 |
| .bms | BMS | 8 | 1,200 | 150 | 45.23 |

## Benefits

1. **Proper Classification**: All mainframe and COBOL ecosystem files now properly identified
2. **Better Visibility**: Two detailed tables show exactly what files are being analyzed
3. **Accurate Metrics**: LOC counts per extension help verify analysis accuracy
4. **Complete Context**: Users can see file type, language, count, LOC, and size all in one view

## Next Steps

**You must restart the backend server** for these changes to take effect:

```bash
# Stop the running server (Ctrl+C)

# Restart
cd c:\code\BRE\backend
python main.py
```

Then refresh your browser to see the updated file type labels and new detailed tables.
