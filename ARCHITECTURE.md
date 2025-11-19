# Business Rules Extraction Framework - Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         USER INTERFACE                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │ Web Frontend │  │  REST Client │  │ Python Script│         │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘         │
└─────────┼──────────────────┼──────────────────┼─────────────────┘
          │                  │                  │
          └──────────────────┼──────────────────┘
                             │
                    ┌────────▼────────┐
                    │   FastAPI App   │
                    │  (main.py)      │
                    └────────┬────────┘
                             │
          ┌──────────────────┼──────────────────┐
          │                  │                  │
    ┌─────▼──────┐  ┌───────▼────────┐  ┌─────▼──────┐
    │ Analysis   │  │  ZIP Analysis  │  │  AI/ML     │
    │ API        │  │  API           │  │  API       │
    └─────┬──────┘  └───────┬────────┘  └─────┬──────┘
          │                  │                  │
          │         ┌────────▼────────┐         │
          │         │  ZIP Explorer   │         │
          │         │  (utils/)       │         │
          │         └────────┬────────┘         │
          │                  │                  │
          └──────────────────┼──────────────────┘
                             │
                    ┌────────▼────────┐
                    │ Language Router │
                    └────────┬────────┘
                             │
          ┌──────────────────┼──────────────────┐
          │                  │                  │
    ┌─────▼──────┐  ┌───────▼────────┐  ┌─────▼──────┐
    │   COBOL    │  │      SQL       │  │   AS400    │
    │  Analyzer  │  │   Analyzer     │  │  Analyzer  │
    └────────────┘  └────────────────┘  └────────────┘
```

## Component Details

### 1. API Layer

#### FastAPI Application ([main.py](backend/main.py))
- Entry point for all HTTP requests
- CORS configuration
- Route management
- Error handling

#### API Routers
- **Analysis API** ([api/analysis.py](backend/api/analysis.py))
  - Full codebase analysis
  - Metrics extraction
  - Dependency analysis
  - Database operations analysis

- **ZIP Analysis API** ([api/zip_analysis.py](backend/api/zip_analysis.py))
  - ZIP file upload
  - Structure exploration
  - Metadata extraction
  - LOC counting
  - File content access

- **AI API** ([api/ai.py](backend/api/ai.py))
  - AI-powered analysis
  - Business rules extraction
  - Code insights

### 2. Core Components

#### ZIP Explorer ([utils/zip_explorer.py](backend/utils/zip_explorer.py))
```python
┌─────────────────────────────────┐
│      ZipExplorer Class          │
├─────────────────────────────────┤
│ + explore()                     │
│ + extract_to()                  │
│ + get_file_content()            │
│                                 │
│ Private Methods:                │
│ - _get_zip_info()              │
│ - _build_file_tree()           │
│ - _get_files_metadata()        │
│ - _calculate_statistics()      │
│ - _get_language_distribution() │
│ - _perform_detailed_analysis() │
└─────────────────────────────────┘
```

**Key Features:**
- No extraction required for basic analysis
- Streaming file content reading
- Memory-efficient processing
- Comprehensive metadata extraction

#### Language Router ([analyzers/language_router.py](backend/analyzers/language_router.py))
```python
┌─────────────────────────────────┐
│   LanguageRouter Class          │
├─────────────────────────────────┤
│ LANGUAGE_MAP:                   │
│  .cbl → cobol                   │
│  .sql → sql                     │
│  .rpg → as400                   │
│                                 │
│ Methods:                        │
│ + analyze_file()                │
│ + batch_analyze()               │
│ + get_supported_languages()     │
└─────────────────────────────────┘
```

**Routing Logic:**
1. Detect file extension
2. Map to language
3. Route to appropriate analyzer
4. Return structured results

### 3. Language Analyzers

#### COBOL Analyzer ([analyzers/cobol_analyzer.py](backend/analyzers/cobol_analyzer.py))
```
Input: .cbl, .cob, .cobol, .cpy
       ↓
┌──────────────────────┐
│  COBOLAnalyzer       │
├──────────────────────┤
│ - Divisions          │
│ - Copybooks          │
│ - Paragraphs         │
│ - Sections           │
│ - File I/O           │
│ - Database ops       │
│ - Business rules     │
│ - Data items         │
│ - Metrics            │
└──────────────────────┘
       ↓
Output: Structured analysis
```

#### SQL Analyzer ([analyzers/sql_analyzer.py](backend/analyzers/sql_analyzer.py))
```
Input: .sql, .ddl, .dml
       ↓
┌──────────────────────┐
│  SQLAnalyzer         │
├──────────────────────┤
│ - Query types        │
│ - Tables             │
│ - Operations         │
│ - DDL statements     │
│ - DML statements     │
│ - Business logic     │
│ - Complexity         │
└──────────────────────┘
       ↓
Output: Structured analysis
```

#### AS400 Analyzer ([analyzers/as400_analyzer.py](backend/analyzers/as400_analyzer.py))
```
Input: .rpg, .rpgle, .rpglec, etc.
       ↓
┌──────────────────────┐
│  AS400Analyzer       │
├──────────────────────┤
│ - Format detection   │
│ - Procedures         │
│ - Subroutines        │
│ - File definitions   │
│ - Database ops       │
│ - Business rules     │
│ - Data structures    │
│ - Indicators         │
└──────────────────────┘
       ↓
Output: Structured analysis
```

## Data Flow

### Upload and Basic Analysis Flow
```
1. User uploads ZIP
   │
   ├─→ POST /api/zip/upload
   │   └─→ Save to disk
   │       └─→ Generate upload_id
   │
2. User requests summary
   │
   ├─→ GET /api/zip/{id}/summary
   │   └─→ ZipExplorer.explore(extract=False)
   │       ├─→ Read ZIP directory
   │       ├─→ Count files
   │       ├─→ Detect languages
   │       ├─→ Estimate LOC
   │       └─→ Return summary
   │
3. User explores structure
   │
   └─→ GET /api/zip/{id}/explore
       └─→ ZipExplorer.explore(extract=False)
           ├─→ Build file tree
           ├─→ Extract metadata
           ├─→ Calculate statistics
           └─→ Return full analysis
```

### Detailed Analysis Flow
```
1. User requests detailed analysis
   │
   ├─→ GET /api/zip/{id}/explore?detailed=true
   │   └─→ ZipExplorer.explore(extract=True)
   │       ├─→ Extract ZIP to temp dir
   │       ├─→ Scan for supported files
   │       ├─→ For each file:
   │       │   ├─→ Detect language
   │       │   ├─→ Route to analyzer
   │       │   │   ├─→ COBOLAnalyzer
   │       │   │   ├─→ SQLAnalyzer
   │       │   │   └─→ AS400Analyzer
   │       │   └─→ Collect results
   │       ├─→ Cleanup temp dir
   │       └─→ Return detailed analysis
```

## File Storage Structure

```
uploads/
├── {upload_id_1}/
│   ├── codebase.zip         # Original ZIP
│   └── extracted/           # (Optional) Extracted files
│       ├── src/
│       │   ├── main.cbl
│       │   └── utils.cbl
│       └── sql/
│           └── schema.sql
│
├── {upload_id_2}/
│   └── another.zip
│
└── {upload_id_3}/
    └── legacy.zip
```

## Processing Modes

### Mode 1: Quick Summary (⚡ Fastest)
- No extraction
- ZIP directory reading only
- Basic metadata
- Language detection
- LOC estimation

**Use Case:** Initial codebase overview

### Mode 2: Full Exploration (⚡⚡ Fast)
- No extraction
- Read file contents for LOC
- Complete metadata
- File tree structure
- Statistics

**Use Case:** Detailed codebase understanding

### Mode 3: Detailed Analysis (⚡⚡⚡ Comprehensive)
- Full extraction
- Language-specific analysis
- Business rules extraction
- Deep code insights
- Modernization hints

**Use Case:** In-depth code analysis

## Supported File Types

### Primary Languages (Full Analysis)
```
COBOL
├── .cbl    (COBOL source)
├── .cob    (COBOL source)
├── .cobol  (COBOL source)
└── .cpy    (Copybook)

SQL
├── .sql    (SQL script)
├── .ddl    (Data Definition)
└── .dml    (Data Manipulation)

AS400/RPG
├── .rpg       (RPG III)
├── .rpgle     (RPG IV/ILE)
├── .rpglec    (RPG ILE C)
├── .sqlrpgle  (SQL + RPG)
├── .rpg4      (RPG IV)
├── .rpgiv     (RPG IV)
├── .dspf      (Display file)
├── .prtf      (Printer file)
├── .lf        (Logical file)
└── .pf        (Physical file)
```

### Text Files (LOC Counting)
All text-based files get LOC counting but not deep analysis

### Binary Files
Detected and counted but not analyzed

## API Response Formats

### Summary Response
```json
{
  "filename": "string",
  "total_files": 0,
  "total_size_mb": 0.0,
  "total_loc": 0,
  "languages": [
    {
      "language": "string",
      "file_count": 0,
      "total_loc": 0,
      "percentage": 0.0
    }
  ],
  "top_extensions": []
}
```

### Full Exploration Response
```json
{
  "zip_info": { ... },
  "file_tree": { ... },
  "files": [ ... ],
  "statistics": { ... },
  "language_distribution": { ... },
  "detailed_analysis": null | { ... }
}
```

## Extension Points

### Adding New Languages
1. Create analyzer in `backend/analyzers/`
2. Update `language_router.py` LANGUAGE_MAP
3. Add analyzer to router's `__init__`
4. Update documentation

### Custom Analysis
1. Extend base analyzer class
2. Implement `analyze_file()` method
3. Register in language router
4. Add to API endpoints

## Performance Characteristics

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| ZIP Upload | O(n) | O(n) |
| Summary | O(m) | O(1) |
| Exploration | O(n*m) | O(n) |
| Detailed Analysis | O(n*m*k) | O(n) |

Where:
- n = number of files
- m = average file size
- k = analysis complexity factor

## Security Considerations

1. **File Validation**: Only ZIP files accepted
2. **Size Limits**: Configurable max upload size
3. **Path Traversal**: Prevented during extraction
4. **Cleanup**: Automatic temp file removal
5. **Sandboxing**: Isolated upload directories

## Scalability

### Current Design
- Single server
- Synchronous processing
- Local file storage

### Future Improvements
- Async file processing
- Distributed storage
- Caching layer
- Queue-based analysis
- Horizontal scaling

---

**Last Updated:** 2025-11-18
