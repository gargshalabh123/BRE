# Language-Specific Analyzers - Summary

## What I've Created

I've built **4 specialized code analyzers** for the BRE framework, each tailored to extract business rules and insights from specific programming languages used in legacy systems.

## The Analyzers

### 1. **COBOL Analyzer** (`cobol_analyzer.py`)
**Purpose:** Analyze COBOL/COBOL II legacy mainframe code

**What it extracts:**
- ✅ **Divisions:** IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- ✅ **Copybooks:** All COPY statements (like #include)
- ✅ **Paragraphs & Sections:** COBOL subroutines
- ✅ **File I/O:** OPEN, CLOSE, READ, WRITE operations
- ✅ **Database:** EXEC SQL and EXEC CICS commands
- ✅ **Data Items:** 01/77/88 level items, PIC clauses
- ✅ **Business Rules:** IF statements, COMPUTE, EVALUATE
- ✅ **Modernization Hints:** GOTO, ALTER warnings

**Example Business Rules Detected:**
```cobol
IF CUST-AGE >= WS-SENIOR-AGE          ← Detected as "Date/Age Rule"
    MOVE 20 TO WS-DISCOUNT-RATE       ← Detected as "Financial Calculation"
```

---

### 2. **Java Analyzer** (`java_analyzer.py`)
**Purpose:** Analyze Java/J2EE enterprise applications

**What it extracts:**
- ✅ **Structure:** Packages, classes, interfaces, methods
- ✅ **Annotations:** @Service, @RestController, @Entity, etc.
- ✅ **Frameworks:** Spring, Hibernate, JPA, JAX-RS detection
- ✅ **Design Patterns:** Singleton, Factory, Builder, DAO
- ✅ **Database:** JDBC, JPA queries, PreparedStatements
- ✅ **Business Rules:** if statements, BigDecimal calculations
- ✅ **Code Smells:** God classes, empty catch, System.out.println

**Example Business Rules Detected:**
```java
if (customer.getAge() >= SENIOR_AGE)   ← Detected as "Date/Time Rule"
balance.multiply(discountRate)         ← Detected as "Financial Calculation"
@NotNull @Min(0)                        ← Detected as "Validation Rule"
```

---

### 3. **Python Analyzer** (`python_analyzer.py`)
**Purpose:** Analyze Python 2.x/3.x applications

**What it extracts:**
- ✅ **AST Parsing:** Functions, classes, decorators
- ✅ **Python Version:** Detects Python 2 vs 3
- ✅ **Imports:** Stdlib vs third-party separation
- ✅ **Frameworks:** Django, Flask, FastAPI, SQLAlchemy
- ✅ **Async Code:** async/await detection
- ✅ **Database:** Django ORM, SQLAlchemy queries
- ✅ **Complexity:** Cyclomatic complexity calculation
- ✅ **Code Quality:** Long functions, bare except, TODOs

**Example Business Rules Detected:**
```python
if customer.age >= SENIOR_AGE:         ← Detected as "Date/Time Rule"
balance * discount_rate                ← Detected as "Financial Rule"
@validate                              ← Detected as "Validation Rule"
```

---

### 4. **SQL Analyzer** (`sql_analyzer.py`)
**Purpose:** Analyze SQL scripts, stored procedures, triggers

**What it extracts:**
- ✅ **Dialect Detection:** Oracle, SQL Server, MySQL, PostgreSQL, DB2
- ✅ **DDL:** CREATE TABLE, CREATE INDEX
- ✅ **Procedures & Functions:** Stored procedure analysis
- ✅ **Triggers:** BEFORE/AFTER INSERT/UPDATE triggers
- ✅ **Constraints:** PRIMARY KEY, FOREIGN KEY, CHECK, UNIQUE
- ✅ **Queries:** SELECT, INSERT, UPDATE, DELETE counts
- ✅ **Business Rules:** CHECK constraints, CASE WHEN logic
- ✅ **Optimization Hints:** SELECT *, missing WHERE, implicit JOINs

**Example Business Rules Detected:**
```sql
CHECK (BALANCE >= 1000.00)             ← Detected as "Financial Constraint"
CASE WHEN STATUS = 'A'                 ← Detected as "Status Logic"
IF v_age >= c_senior_age THEN          ← Detected as "Date/Time Rule"
```

---

## Architecture

### Language Router (`language_router.py`)
**Automatically routes files to the correct analyzer based on extension**

```python
.cbl, .cob  → COBOL Analyzer
.java       → Java Analyzer
.py         → Python Analyzer
.sql        → SQL Analyzer
```

For unsupported languages, provides basic metrics (LOC, file size).

---

## New API Endpoints

I've added a complete API layer for language-specific analysis:

### 1. **Get Supported Languages**
```bash
GET /api/languages/supported-languages
```

### 2. **Analyze Specific File**
```bash
POST /api/languages/{upload_id}/analyze-file/path/to/file.java
```

### 3. **Analyze by Language**
```bash
POST /api/languages/{upload_id}/analyze-by-language/java
```
Analyzes ALL Java files in the upload.

### 4. **Deep Analysis**
```bash
POST /api/languages/{upload_id}/deep-analysis
```
Analyzes entire codebase, groups by language, aggregates business rules and DB operations.

### 5. **Language Summary**
```bash
GET /api/languages/{upload_id}/language-summary
```
Quick overview of what languages are present.

---

## Test Samples

I've created realistic test files in `test_samples/`:

- **`sample.cbl`** - COBOL program with divisions, copybooks, embedded SQL, business rules
- **`sample.java`** - Java class with Spring patterns, JDBC, validation annotations
- **`sample.py`** - Python service with Django ORM, decorators, type hints
- **`sample.sql`** - Oracle SQL with procedures, triggers, constraints, business rules

You can upload these individually or as a ZIP to test the analyzers!

---

## How to Use

### Via API (After Starting Backend)

```bash
# 1. Upload test samples
cd test_samples
zip samples.zip *.cbl *.java *.py *.sql
curl -X POST -F "file=@samples.zip" http://localhost:8000/api/upload

# 2. Get upload ID from response, then:
UPLOAD_ID="abc123"

# 3. Deep analysis (all languages)
curl http://localhost:8000/api/languages/$UPLOAD_ID/deep-analysis

# 4. Analyze just Java files
curl http://localhost:8000/api/languages/$UPLOAD_ID/analyze-by-language/java

# 5. Analyze specific file
curl http://localhost:8000/api/languages/$UPLOAD_ID/analyze-file/sample.cbl
```

### Via Python Code

```python
from analyzers import analyze_code_file

# Analyze single file
result = analyze_code_file('sample.java')
print(result['frameworks'])      # ['Spring', 'JDBC']
print(result['design_patterns']) # ['Service', 'DAO']
print(result['business_rules'])  # List of business rules
```

---

## What Business Rules Get Extracted

### COBOL
- Financial calculations (COMPUTE with money fields)
- Date/age validations
- EVALUATE (CASE) logic
- 88-level condition names
- Validation PERFORM paragraphs

### Java
- if statements with amount/price/balance
- BigDecimal operations
- @Validation annotations
- State machines (switch)
- calculate*/validate*/check* methods

### Python
- if statements with financial/date fields
- @validate decorators
- Decimal quantization
- assert/raise statements
- calculate_*/validate_* functions

### SQL
- CHECK constraints
- CASE WHEN logic
- DEFAULT values
- Referential integrity (FK)
- UNIQUE/NOT NULL constraints

---

## Integration with Main Framework

The language analyzers integrate seamlessly with the existing BRE framework:

1. **Same Upload Flow** - Uses the same file upload API
2. **Complementary** - Works alongside the general CodeAnalyzer
3. **Deeper Insights** - Provides language-specific business rules
4. **Aggregation** - Deep analysis combines results from all languages

---

## Files Created

### Analyzers (Backend)
```
backend/analyzers/
├── cobol_analyzer.py          # COBOL-specific analyzer
├── java_analyzer.py           # Java-specific analyzer
├── python_analyzer.py         # Python-specific analyzer
├── sql_analyzer.py            # SQL-specific analyzer
├── language_router.py         # Routes to correct analyzer
└── __init__.py                # Updated exports
```

### API Layer
```
backend/api/
└── language_analysis.py       # New API endpoints
```

### Documentation
```
LANGUAGE_ANALYZERS.md          # Complete usage guide
LANGUAGE_ANALYZERS_SUMMARY.md  # This file
```

### Test Files
```
test_samples/
├── sample.cbl                 # COBOL test
├── sample.java                # Java test
├── sample.py                  # Python test
└── sample.sql                 # SQL test
```

---

## Key Features

### 1. **Language-Aware Business Rules**
Each analyzer knows what business rules look like in that language:
- COBOL: COMPUTE with financial fields
- Java: BigDecimal calculations
- Python: Decimal operations
- SQL: CHECK constraints with business logic

### 2. **Framework Detection**
- Java: Detects Spring, Hibernate, JPA, EJB
- Python: Detects Django, Flask, FastAPI, SQLAlchemy
- SQL: Detects Oracle, SQL Server, MySQL, PostgreSQL

### 3. **Code Quality Insights**
- Java: God classes, code smells
- Python: Long functions, bare except
- SQL: Optimization hints (SELECT *, missing WHERE)
- COBOL: Modernization hints (GOTO, ALTER)

### 4. **Aggregation & Reporting**
Deep analysis combines results:
- Total business rules by language
- Database operations across all files
- Framework usage summary
- Code quality trends

---

## Next Steps to Use

1. **Start the backend:**
   ```bash
   cd backend
   venv\Scripts\activate
   python main.py
   ```

2. **Test with samples:**
   ```bash
   cd test_samples
   # Upload individual file or create ZIP
   ```

3. **Access API docs:**
   ```
   http://localhost:8000/docs
   ```
   Look for the "Language-Specific Analysis" section

4. **Try deep analysis:**
   Upload your legacy codebase and call `/deep-analysis` to see business rules grouped by language!

---

## Benefits for Legacy Code Analysis

✅ **COBOL Legacy Systems** - Extract mainframe business logic
✅ **Java Enterprise Apps** - Understand Spring/J2EE architecture
✅ **Python Services** - Analyze modern microservices
✅ **Database Logic** - Extract business rules from stored procedures
✅ **Cross-Language** - Analyze polyglot codebases
✅ **Migration Planning** - Identify patterns for modernization
✅ **Documentation** - Auto-generate business rule catalogs

---

## Example Output

### Deep Analysis Response:
```json
{
  "total_files_analyzed": 4,
  "languages": {
    "cobol": {
      "total_files": 1,
      "total_lines": 95,
      "business_rules_count": 8,
      "database_operations_count": 1,
      "modernization_hints": [...]
    },
    "java": {
      "total_files": 1,
      "total_lines": 137,
      "business_rules_count": 12,
      "database_operations_count": 5,
      "frameworks": ["JDBC"],
      "code_smells": [...]
    },
    "python": {
      "total_files": 1,
      "total_lines": 145,
      "business_rules_count": 10,
      "database_operations_count": 3,
      "frameworks": ["SQLite"]
    },
    "sql": {
      "total_files": 1,
      "total_lines": 195,
      "business_rules_count": 15,
      "optimization_hints": [...]
    }
  }
}
```

This gives you a complete picture of your legacy codebase's business logic!
