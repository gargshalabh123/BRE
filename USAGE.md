# Usage Guide - Business Rules Extraction Framework

## Quick Start

1. **Start Backend**: `cd backend && python main.py`
2. **Start Frontend**: `cd frontend && npm run dev`
3. **Open Browser**: Navigate to `http://localhost:3000`
4. **Upload Code**: Drag and drop your codebase or select files
5. **Analyze**: View results across different tabs

## Features Overview

### 1. Code Upload
- **Supported Formats**: Individual files or ZIP archives
- **Supported Languages**: COBOL, Java, Python, JavaScript, C/C++, SQL, and more
- **Max Size**: 100MB (configurable)

### 2. Overview Tab
**What it shows:**
- Total files and codebase size
- Lines of code metrics
- File type distribution (pie chart)
- Code composition (bar chart)
- Database operations summary

**Use Cases:**
- Quick assessment of codebase size and composition
- Identify primary technologies used
- Understand code vs comments ratio

### 3. Metrics Tab
**What it shows:**
- Detailed LOC (Lines of Code) analysis per file
- SLOC (Source Lines of Code - excluding blanks/comments)
- Comment lines
- Blank lines
- Cyclomatic complexity (for Python files)
- Function counts

**Use Cases:**
- Identify largest/most complex files
- Find files with low comment ratios
- Prioritize refactoring efforts
- Estimate migration effort

**How to use:**
- Click column headers to sort by different metrics
- Look for files with high complexity scores
- Compare LOC vs SLOC to find heavily commented code

### 4. Dependencies Tab
**What it shows:**
- Import/dependency analysis per file
- Most frequently imported modules
- Dependency counts and relationships

**Use Cases:**
- Understand module dependencies
- Identify tightly coupled code
- Find external library usage
- Plan migration strategy

**Insights:**
- High import counts may indicate tight coupling
- Shared dependencies show architectural patterns
- External modules need migration assessment

### 5. Database Tab
**What it shows:**
- All SQL queries found in code
- Query types (SELECT, INSERT, UPDATE, DELETE, etc.)
- File locations and line numbers
- Query type distribution

**Use Cases:**
- Inventory all database operations
- Identify data access patterns
- Find hardcoded SQL queries
- Plan database migration

**Filtering:**
- Use the dropdown to filter by query type
- Click on queries to see full text
- Note file locations for detailed review

### 6. Business Rules Tab
**What it shows:**
- Detected business logic patterns
- Rule types (Financial, Date/Age, Status, Validation, etc.)
- Code snippets containing rules
- File locations

**Use Cases:**
- Extract business logic from legacy code
- Document business rules
- Identify validation logic
- Support knowledge transfer

**Pattern Types:**
- **Financial Rules**: Amount, price, discount calculations
- **Date/Age Rules**: Date validations and age checks
- **Status Rules**: State machine logic
- **Validation Rules**: Input validation
- **Calculation Rules**: Business calculations
- **Threshold Rules**: Limits and boundaries

### 7. AI Insights Tab
**What it shows:**
- AI-generated codebase summary
- Architecture assessment
- Technology stack analysis
- Modernization recommendations
- Technical debt indicators

**Prerequisites:**
- OpenAI or Anthropic API key configured
- Backend .env file properly set up

**How to use:**
1. Select AI provider (OpenAI or Anthropic)
2. Click "Generate Summary"
3. Wait for AI analysis (10-30 seconds)
4. Review comprehensive insights

**What AI analyzes:**
- Overall codebase structure
- Technology patterns
- Business domain identification
- Refactoring opportunities
- Migration recommendations

## API Usage (Advanced)

### Upload File
```bash
curl -X POST "http://localhost:8000/api/upload" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@your-code.zip"
```

Response:
```json
{
  "upload_id": "tmp_abc123",
  "filename": "your-code.zip",
  "size": 1048576,
  "is_archive": true,
  "files_count": 42,
  "files": ["file1.py", "file2.java", "..."]
}
```

### Full Analysis
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/full"
```

### Metrics Only
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/metrics"
```

### Dependencies
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/dependencies"
```

### Database Operations
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/database"
```

### Business Rules
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/business-rules"
```

### AI Code Explanation
```bash
curl -X POST "http://localhost:8000/api/ai/explain-code" \
  -H "Content-Type: application/json" \
  -d '{
    "upload_id": "tmp_abc123",
    "file_path": "src/main.py",
    "provider": "anthropic"
  }'
```

### AI Codebase Summary
```bash
curl -X POST "http://localhost:8000/api/ai/summarize/{upload_id}?provider=anthropic"
```

## Export Results

### Via UI
1. Navigate to analysis results
2. Click "Export JSON" button
3. File downloads as `analysis-{upload_id}.json`

### Via API
The full analysis endpoint returns JSON that can be saved:
```bash
curl -X POST "http://localhost:8000/api/analysis/{upload_id}/full" \
  > analysis_results.json
```

## Customization

### Adding Custom Business Rule Patterns

Edit `backend/analyzers/code_analyzer.py`:

```python
patterns = [
    # Add your custom patterns
    (r'your_regex_pattern', 'Your Rule Type'),
    (r'\bcustom_function\s*\(', 'Custom Business Logic'),
]
```

### Modifying File Type Support

Edit `backend/analyzers/code_analyzer.py`:

```python
FILE_EXTENSIONS = {
    '.your_ext': 'Your Language',
    # Add more extensions
}
```

### Adjusting Upload Limits

Edit `backend/.env`:
```env
MAX_UPLOAD_SIZE_MB=200  # Increase limit
```

## Common Workflows

### 1. Legacy Code Assessment
1. Upload codebase
2. Review Overview tab for size and composition
3. Check Metrics tab for complexity hotspots
4. Review Database tab for data dependencies
5. Generate AI summary for recommendations

### 2. Migration Planning
1. Upload legacy code
2. Analyze dependencies to understand module structure
3. Review database operations for data migration
4. Extract business rules for documentation
5. Export results for migration planning

### 3. Code Documentation
1. Upload undocumented code
2. Use AI insights to generate high-level documentation
3. Extract business rules for rule documentation
4. Export metrics for technical documentation

### 4. Technical Debt Assessment
1. Upload codebase
2. Review complexity metrics
3. Identify files with low comment ratios
4. Check for SQL injection risks (hardcoded queries)
5. Generate AI recommendations

## Best Practices

### For Upload
- ✅ Use ZIP for multiple files
- ✅ Include all source files
- ✅ Remove binaries and build artifacts
- ❌ Don't include node_modules, venv, etc.
- ❌ Don't upload sensitive data without review

### For Analysis
- ✅ Review all tabs for comprehensive understanding
- ✅ Export results for documentation
- ✅ Use AI insights for strategic decisions
- ✅ Sort metrics to find priorities
- ❌ Don't rely solely on pattern matching
- ❌ Don't skip manual code review

### For Business Rules
- ✅ Review extracted rules manually
- ✅ Add custom patterns for your domain
- ✅ Document rules outside the system
- ❌ Don't assume 100% accuracy
- ❌ Don't use as sole documentation source

## Tips and Tricks

1. **Large Codebases**: Upload in chunks if over 100MB
2. **Performance**: Smaller uploads analyze faster
3. **Accuracy**: AI insights work better with complete codebases
4. **Context**: Include configuration files for better analysis
5. **Iteration**: Re-run analysis after adding custom patterns

## Limitations

- Pattern matching may miss complex business rules
- AI analysis requires API keys and internet connection
- Very large files may slow analysis
- Some languages have better support than others
- Complexity metrics currently only for Python

## Future Enhancements

Planned features:
- Call graph visualization
- Custom pattern configuration UI
- PDF report export
- Multi-language complexity analysis
- Incremental analysis
- Git repository integration
- Code smell detection
- Architecture diagram generation

## Support and Feedback

For questions or issues:
- Check logs in backend console
- Review browser console for frontend errors
- Consult SETUP.md for configuration
- Review API docs at `/docs`
