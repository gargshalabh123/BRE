## Language-Specific Code Analyzers

The BRE framework includes specialized analyzers for major legacy and modern programming languages. Each analyzer provides deep insights specific to that language's syntax, patterns, and best practices.

## Supported Languages

### Fully Supported (Deep Analysis)

#### 1. COBOL Analyzer
**File Extensions:** `.cbl`, `.cob`, `.cobol`, `.cpy`

**Features:**
- ✅ Division detection (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- ✅ Copybook extraction (COPY statements)
- ✅ Paragraph and Section identification
- ✅ File I/O operations (OPEN, CLOSE, READ, WRITE)
- ✅ Embedded SQL detection (EXEC SQL)
- ✅ CICS command detection (EXEC CICS)
- ✅ 88-level condition names
- ✅ Data item analysis (01, 77 levels, PIC clauses)
- ✅ Business rule extraction
- ✅ Modernization hints (GOTO, ALTER detection)

**Business Rules Detected:**
- Financial calculations (COMPUTE with INTEREST, TAX, etc.)
- Date/time rules
- EVALUATE statements (decision logic)
- Validation PERFORM statements
- Arithmetic operations

**Example Output:**
```json
{
  "language": "COBOL",
  "divisions": {
    "IDENTIFICATION DIVISION": 1,
    "PROCEDURE DIVISION": 1
  },
  "copybooks": [
    {"name": "CUSTOMER-RECORD", "line": 45}
  ],
  "database": [
    {"type": "SELECT", "category": "EMBEDDED SQL", "line": 234}
  ],
  "modernization_hints": [
    {
      "type": "Code Smell",
      "issue": "GOTO statements found",
      "recommendation": "Replace with structured programming"
    }
  ]
}
```

#### 2. Java Analyzer
**File Extensions:** `.java`

**Features:**
- ✅ Package and import analysis
- ✅ Class, interface, and method extraction
- ✅ Annotation detection (@Service, @RestController, etc.)
- ✅ Framework detection (Spring, Hibernate, JPA, etc.)
- ✅ Design pattern identification
- ✅ JDBC/JPA database operations
- ✅ Code smell detection
- ✅ Method visibility analysis

**Frameworks Detected:**
- Spring (Boot, MVC, Data)
- Hibernate
- JPA
- JAX-RS
- Servlets
- JSF
- Struts
- EJB

**Design Patterns:**
- Singleton
- Factory
- Builder
- Observer
- Strategy
- DAO/Repository
- DTO
- Service/Controller

**Code Smells:**
- God classes (>1000 lines)
- Too many methods (>30)
- Empty catch blocks
- Debug code (System.out.println)
- Magic numbers

**Example Output:**
```json
{
  "language": "Java",
  "package": "com.example.service",
  "classes": [
    {
      "name": "CustomerService",
      "modifier": "public",
      "implements": ["CustomerInterface"]
    }
  ],
  "frameworks": ["Spring", "JPA"],
  "design_patterns": ["Service", "DAO"],
  "code_smells": [
    {
      "type": "Debug Code",
      "severity": "LOW",
      "description": "Found System.out.println - use proper logging"
    }
  ]
}
```

#### 3. Python Analyzer
**File Extensions:** `.py`, `.pyw`

**Features:**
- ✅ AST-based parsing
- ✅ Function and class extraction with decorators
- ✅ Import analysis (stdlib vs third-party)
- ✅ Python 2 vs 3 detection
- ✅ Framework detection (Django, Flask, FastAPI, etc.)
- ✅ Async/await detection
- ✅ ORM query extraction (Django ORM, SQLAlchemy)
- ✅ Code quality assessment
- ✅ Cyclomatic complexity

**Frameworks Detected:**
- Django
- Flask
- FastAPI
- SQLAlchemy
- Pandas
- NumPy
- Pytest
- Celery

**Code Quality Issues:**
- Long functions (>50 lines)
- Too many globals (>10)
- Bare except clauses
- TODO/FIXME comments

**Example Output:**
```json
{
  "language": "Python",
  "python_version": "Python 3.x",
  "functions": [
    {
      "name": "calculate_discount",
      "args": ["price", "customer_age"],
      "decorators": ["@validate"],
      "is_async": false
    }
  ],
  "frameworks": ["FastAPI", "SQLAlchemy"],
  "async_code": {
    "has_async": true,
    "async_functions": 5,
    "await_calls": 12
  },
  "dependencies": {
    "stdlib": ["os", "json", "datetime"],
    "third_party": ["fastapi", "sqlalchemy"],
    "local": []
  }
}
```

#### 4. SQL Analyzer
**File Extensions:** `.sql`, `.ddl`, `.dml`

**Features:**
- ✅ SQL dialect detection (Oracle, SQL Server, MySQL, PostgreSQL, DB2)
- ✅ Table definition extraction
- ✅ Stored procedure analysis
- ✅ Function extraction
- ✅ Trigger detection
- ✅ Index analysis
- ✅ Constraint extraction (PK, FK, CHECK, UNIQUE)
- ✅ Query pattern analysis
- ✅ Optimization hints

**Dialects Detected:**
- Oracle (VARCHAR2, ROWNUM, EXEC)
- SQL Server (IDENTITY, GO, GETDATE)
- MySQL (AUTO_INCREMENT, ENGINE)
- PostgreSQL (SERIAL, RETURNING)
- DB2 (FETCH FIRST, SYSIBM)

**Optimization Hints:**
- SELECT * detection
- Missing WHERE clauses
- Implicit JOINs
- Excessive OR conditions
- Subquery optimization

**Example Output:**
```json
{
  "language": "SQL",
  "dialect": "Oracle",
  "tables": [
    {"name": "CUSTOMERS", "columns": ["id", "name", "balance"]}
  ],
  "procedures": [
    {"name": "UPDATE_BALANCE", "parameters": ["cust_id", "amount"]}
  ],
  "optimization_hints": [
    {
      "type": "Performance",
      "issue": "SELECT * found",
      "recommendation": "Specify explicit column names"
    }
  ]
}
```

### Basic Support

These languages have basic file scanning and metrics but no specialized analysis yet:

- **C/C++** (`.c`, `.cpp`, `.h`, `.hpp`)
- **JavaScript** (`.js`, `.jsx`)
- **TypeScript** (`.ts`, `.tsx`)
- **C#** (`.cs`)
- **Go** (`.go`)
- **Rust** (`.rs`)
- **Ruby** (`.rb`)
- **PHP** (`.php`)
- **Perl** (`.pl`)
- **Visual Basic** (`.vb`)

## API Endpoints

### 1. Get Supported Languages
```bash
GET /api/languages/supported-languages
```

Returns list of all supported languages and their file extensions.

### 2. Analyze Specific File
```bash
POST /api/languages/{upload_id}/analyze-file/{file_path}
```

Analyzes a single file using the appropriate language analyzer.

**Example:**
```bash
curl -X POST "http://localhost:8000/api/languages/abc123/analyze-file/src/main/CustomerService.java"
```

### 3. Analyze by Language
```bash
POST /api/languages/{upload_id}/analyze-by-language/{language}
```

Analyzes all files of a specific language.

**Supported Languages:** cobol, java, python, sql, c, cpp, javascript, typescript, csharp

**Example:**
```bash
curl -X POST "http://localhost:8000/api/languages/abc123/analyze-by-language/java"
```

### 4. Deep Analysis
```bash
POST /api/languages/{upload_id}/deep-analysis
```

Performs comprehensive analysis on entire codebase, groups by language.

**Example:**
```bash
curl -X POST "http://localhost:8000/api/languages/abc123/deep-analysis"
```

**Response:**
```json
{
  "total_files_analyzed": 150,
  "languages": {
    "java": {
      "total_files": 75,
      "total_lines": 15000,
      "business_rules_count": 234,
      "database_operations_count": 89
    },
    "sql": {
      "total_files": 25,
      "total_lines": 5000,
      "business_rules_count": 45
    }
  }
}
```

### 5. Language Summary
```bash
GET /api/languages/{upload_id}/language-summary
```

Quick summary of languages in the codebase.

## Usage Examples

### Python API Usage

```python
from analyzers import analyze_code_file, analyze_codebase

# Analyze single file
result = analyze_code_file('path/to/CustomerService.java')
print(result['language'])  # 'java'
print(result['classes'])   # List of classes

# Analyze entire codebase
results = analyze_codebase('path/to/project', extensions=['.py', '.java'])
print(results['summary'])
```

### Via Frontend

1. Upload your codebase
2. Navigate to the "Language Analysis" tab (if implemented)
3. Select a language to analyze
4. View language-specific insights

### Via cURL

```bash
# Get supported languages
curl http://localhost:8000/api/languages/supported-languages

# Upload file
UPLOAD_ID=$(curl -X POST -F "file=@code.zip" http://localhost:8000/api/upload | jq -r .upload_id)

# Deep analysis
curl -X POST "http://localhost:8000/api/languages/$UPLOAD_ID/deep-analysis"

# Analyze just Java files
curl -X POST "http://localhost:8000/api/languages/$UPLOAD_ID/analyze-by-language/java"
```

## Business Rules by Language

### COBOL Business Rules
- Financial calculations (COMPUTE)
- Date validations
- Status checks (EVALUATE)
- Condition names (88-level)
- Validation paragraphs

### Java Business Rules
- Financial rules (if statements with amount/price)
- Validation annotations (@NotNull, @Min, @Max)
- State machines (switch statements)
- BigDecimal calculations
- Business method patterns (calculate*, validate*)

### Python Business Rules
- Financial rules (if with amount/balance)
- Validation decorators (@validate)
- Calculation functions (def calculate_*)
- Constraints (assert, raise)
- Decimal quantization

### SQL Business Rules
- CHECK constraints
- DEFAULT values
- CASE WHEN logic
- Foreign keys (referential integrity)
- UNIQUE constraints
- NOT NULL requirements

## Extending the Analyzers

### Adding a New Language

1. Create analyzer file: `backend/analyzers/your_language_analyzer.py`

```python
class YourLanguageAnalyzer:
    def analyze_file(self, file_path: Path) -> Dict[str, Any]:
        # Your analysis logic
        return {
            'file': str(file_path),
            'language': 'YourLanguage',
            # ... more results
        }
```

2. Register in `language_router.py`:

```python
LANGUAGE_MAP = {
    '.yourlang': 'yourlanguage',
    # ... existing mappings
}

self.analyzers = {
    'yourlanguage': YourLanguageAnalyzer(),
    # ... existing analyzers
}
```

3. Add to `__init__.py`:

```python
from .your_language_analyzer import YourLanguageAnalyzer

__all__ = [..., 'YourLanguageAnalyzer']
```

### Customizing Business Rule Patterns

Edit the `_extract_business_rules` method in each analyzer:

```python
patterns = [
    (r'your_regex_pattern', 'Your Rule Type'),
    # Add more patterns
]
```

## Performance Considerations

- **Large Files:** Analyzers may be slow on files >10,000 lines
- **AST Parsing:** Python analyzer uses AST which is more accurate but slower
- **Batch Analysis:** Use deep analysis for entire codebases
- **Caching:** Results are not cached - re-analysis re-processes files

## Limitations

- **COBOL:** Column 7-72 format assumed; some dialects may not be fully supported
- **Java:** Complex generics may not be fully parsed
- **Python:** Syntax errors will cause AST parsing to fall back to regex
- **SQL:** Dialect detection is heuristic-based; may not be 100% accurate

## Future Enhancements

- [ ] C/C++ specialized analyzer
- [ ] JavaScript/TypeScript analyzer
- [ ] Call graph generation
- [ ] Cross-language dependency tracking
- [ ] Performance profiling hints
- [ ] Security vulnerability detection
- [ ] Code duplication detection
- [ ] Test coverage analysis integration
