# ZIP Explorer - Usage Guide

## Overview

The ZIP Explorer provides comprehensive analysis of ZIP file contents including file structure, metadata, Lines of Code (LOC), and detailed code analysis for COBOL, SQL, and AS400/RPG files.

## Features

### 1. **File Structure Analysis**
- Hierarchical tree view of all files and folders
- Complete directory structure mapping
- File path and organization visibility

### 2. **Comprehensive Metadata**
- File size (original and compressed)
- Compression ratios
- File types and extensions
- MIME types
- Creation and modification dates
- Language detection (COBOL, SQL, AS400/RPG)

### 3. **Lines of Code (LOC) Metrics**
- Automatic LOC counting for all text files
- Language-specific LOC breakdown
- Total codebase size estimation
- Code vs. comments vs. blank lines

### 4. **Statistical Analysis**
- Total files and size metrics
- Distribution by file type/extension
- Distribution by directory
- Language distribution
- Largest files identification
- Text vs. binary file counts

### 5. **Detailed Code Analysis** (Optional)
- Full COBOL analysis (divisions, copybooks, paragraphs, etc.)
- SQL analysis (queries, DDL, DML operations)
- AS400/RPG analysis (procedures, subroutines, file operations)

## API Endpoints

### 1. Upload ZIP File
```http
POST /api/zip/upload
Content-Type: multipart/form-data

file: <your-zip-file>
```

**Response:**
```json
{
  "upload_id": "uuid-string",
  "filename": "codebase.zip",
  "size_bytes": 12345678,
  "status": "uploaded"
}
```

### 2. Quick Summary
```http
GET /api/zip/{upload_id}/summary
```

**Response:**
```json
{
  "filename": "codebase.zip",
  "total_files": 150,
  "total_size_mb": 45.6,
  "total_loc": 25000,
  "languages": [
    {
      "language": "cobol",
      "file_count": 50,
      "total_loc": 15000,
      "percentage": 60.0
    },
    {
      "language": "sql",
      "file_count": 30,
      "total_loc": 5000,
      "percentage": 20.0
    }
  ],
  "top_extensions": [
    [".cbl", {"count": 50, "size": 3000000, "loc": 15000}],
    [".sql", {"count": 30, "size": 500000, "loc": 5000}]
  ]
}
```

### 3. Full Exploration
```http
GET /api/zip/{upload_id}/explore?detailed=false
```

**Query Parameters:**
- `detailed` (boolean): If true, extracts files and performs deep code analysis

**Response:**
```json
{
  "zip_info": {
    "filename": "codebase.zip",
    "path": "/path/to/file.zip",
    "size_bytes": 12345678,
    "size_mb": 11.77,
    "created": "2025-11-18T10:00:00",
    "modified": "2025-11-18T12:00:00",
    "total_files": 150,
    "compression": "ZIP_DEFLATED"
  },
  "file_tree": {
    "name": "root",
    "type": "folder",
    "children": [...]
  },
  "files": [
    {
      "path": "src/main.cbl",
      "name": "main.cbl",
      "directory": "src",
      "extension": ".cbl",
      "language": "cobol",
      "size_bytes": 50000,
      "size_kb": 48.83,
      "compressed_size": 8000,
      "compression_ratio": 84.0,
      "date_time": "2025-11-18T10:00:00",
      "mime_type": "text/plain",
      "estimated_loc": 1200,
      "is_text": true
    }
  ],
  "statistics": {
    "total_files": 150,
    "total_size_bytes": 12345678,
    "total_size_mb": 11.77,
    "total_compressed_bytes": 5000000,
    "total_compressed_mb": 4.77,
    "overall_compression_ratio": 59.5,
    "total_lines_of_code": 25000,
    "text_files": 120,
    "binary_files": 30,
    "by_extension": {
      ".cbl": {
        "count": 50,
        "size": 3000000,
        "loc": 15000
      }
    },
    "by_directory": {
      "src": {
        "count": 80,
        "size": 8000000,
        "loc": 20000
      }
    },
    "largest_files": [
      {
        "path": "src/large.cbl",
        "size_kb": 500.0,
        "loc": 5000
      }
    ],
    "unique_directories": 15,
    "unique_extensions": 8
  },
  "language_distribution": {
    "languages": {
      "cobol": {
        "file_count": 50,
        "total_size": 3000000,
        "total_loc": 15000,
        "files": ["src/file1.cbl", "src/file2.cbl"]
      }
    },
    "languages_sorted": [
      {
        "language": "cobol",
        "file_count": 50,
        "total_size_kb": 2929.69,
        "total_loc": 15000,
        "percentage": 60.0
      }
    ]
  },
  "detailed_analysis": null
}
```

### 4. List Files with Filtering
```http
GET /api/zip/{upload_id}/files?language=cobol&extension=.cbl
```

**Query Parameters:**
- `language`: Filter by language (cobol, sql, as400)
- `extension`: Filter by extension (.cbl, .sql, .rpgle)

**Response:**
```json
{
  "total_files": 50,
  "files": [...]
}
```

### 5. Get File Content
```http
GET /api/zip/{upload_id}/file/{file_path}
```

**Response:**
```json
{
  "path": "src/main.cbl",
  "content": "IDENTIFICATION DIVISION...",
  "lines": 1200,
  "size": 50000
}
```

### 6. Extract ZIP
```http
POST /api/zip/{upload_id}/extract
```

Extracts the ZIP file to server for further analysis.

**Response:**
```json
{
  "status": "extracted",
  "path": "/path/to/extracted",
  "upload_id": "uuid-string"
}
```

### 7. Get Statistics
```http
GET /api/zip/{upload_id}/statistics
```

Returns detailed statistics including language distribution and breakdowns.

### 8. Delete Upload
```http
DELETE /api/zip/{upload_id}
```

Deletes the uploaded ZIP and any extracted files.

## Python Usage Examples

### Basic Usage
```python
from utils.zip_explorer import ZipExplorer, explore_zip, get_zip_summary

# Quick summary
summary = get_zip_summary('path/to/codebase.zip')
print(f"Total files: {summary['total_files']}")
print(f"Total LOC: {summary['total_loc']}")

# Full exploration (without extraction)
results = explore_zip('path/to/codebase.zip', extract=False)
print(f"Languages: {results['language_distribution']}")

# Detailed analysis (with extraction and code analysis)
detailed_results = explore_zip('path/to/codebase.zip', extract=True)
print(f"Analysis: {detailed_results['detailed_analysis']}")
```

### Advanced Usage
```python
from utils.zip_explorer import ZipExplorer

# Create explorer instance
explorer = ZipExplorer('path/to/codebase.zip')

# Explore without extraction
results = explorer.explore(extract=False)

# Filter files by language
cobol_files = [f for f in results['files'] if f['language'] == 'cobol']
print(f"Found {len(cobol_files)} COBOL files")

# Get content of specific file
content = explorer.get_file_content('src/main.cbl')
print(content)

# Extract to specific location
extracted_path = explorer.extract_to('/path/to/destination')
print(f"Extracted to: {extracted_path}")
```

## Supported File Types

### Fully Analyzed Languages
- **COBOL**: `.cbl`, `.cob`, `.cobol`, `.cpy`
- **SQL**: `.sql`, `.ddl`, `.dml`
- **AS400/RPG**: `.rpg`, `.rpgle`, `.rpglec`, `.sqlrpgle`, `.rpg4`, `.rpgiv`, `.dspf`, `.prtf`, `.lf`, `.pf`

### Text Files (LOC counted)
All text-based files including configuration, documentation, and source code files.

### Binary Files
Binary files are detected but not analyzed for LOC.

## Metrics Provided

### File-Level Metrics
- File size (bytes, KB)
- Compressed size
- Compression ratio
- Lines of code
- Language/type
- Directory path

### Aggregate Metrics
- Total files
- Total size (original and compressed)
- Total LOC
- Files by language
- Files by directory
- Files by extension
- Largest files
- Text vs. binary breakdown

### Code Analysis Metrics (when detailed=true)
- **COBOL**: Divisions, copybooks, paragraphs, file I/O, database operations, business rules
- **SQL**: Query types, table operations, data manipulation
- **AS400/RPG**: Procedures, subroutines, file definitions, database operations, indicators

## Performance Considerations

1. **Quick Summary** (`/summary`): Fast, reads ZIP directory only
2. **Exploration** (`/explore?detailed=false`): Medium, reads file contents for LOC
3. **Detailed Analysis** (`/explore?detailed=true`): Slow, extracts and analyzes all supported files

## Example Workflow

```bash
# 1. Upload ZIP
curl -X POST http://localhost:8000/api/zip/upload \
  -F "file=@codebase.zip"

# Response: {"upload_id": "abc123", ...}

# 2. Get quick summary
curl http://localhost:8000/api/zip/abc123/summary

# 3. Explore full structure
curl http://localhost:8000/api/zip/abc123/explore

# 4. List COBOL files only
curl http://localhost:8000/api/zip/abc123/files?language=cobol

# 5. Get specific file content
curl http://localhost:8000/api/zip/abc123/file/src/main.cbl

# 6. Detailed analysis (if needed)
curl http://localhost:8000/api/zip/abc123/explore?detailed=true

# 7. Cleanup
curl -X DELETE http://localhost:8000/api/zip/abc123
```

## Error Handling

All endpoints return appropriate HTTP status codes:
- `200`: Success
- `400`: Bad request (invalid ZIP file)
- `404`: Upload not found
- `413`: File too large
- `500`: Server error

Error responses include details:
```json
{
  "detail": "Error message here"
}
```
