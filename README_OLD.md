# Business Rules Extraction Framework

A comprehensive framework for analyzing legacy codebases and extracting business rules, dependencies, and insights.

## Features

### 1. Code Upload & Processing
- Upload individual files or ZIP archives
- Support for multiple legacy languages (COBOL, Java, C, C++, Python, etc.)
- Automatic extraction and processing

### 2. Pattern & Regex-Based Analysis
- Business rule extraction using configurable patterns
- Code quality metrics (LOC, complexity, etc.)
- File type analysis and categorization
- Dependency mapping and visualization
- Database operation extraction (SQL queries, stored procedures)

### 3. AI-Powered Analysis
- Natural language explanations of code blocks
- Business logic summarization
- Interactive charts and visualizations
- Code-to-documentation generation

## Architecture

```
BRE/
├── backend/              # Python FastAPI backend
│   ├── api/             # API endpoints
│   ├── analyzers/       # Code analysis engines
│   ├── extractors/      # Pattern & rule extractors
│   └── ai/              # GenAI integration
├── frontend/            # React UI
│   ├── src/
│   │   ├── components/  # UI components
│   │   ├── pages/       # Main pages
│   │   └── services/    # API services
└── uploads/             # Temporary file storage
```

## Setup

### Quick Start (Windows - Python 3.13)

If you're using Python 3.13, run the automated setup:
```bash
cd backend
install-py313.bat
```

### Manual Backend Setup
```bash
cd backend
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
python main.py
```

**Note for Python 3.13 users:** Some packages have compatibility issues. See [TROUBLESHOOTING.md](TROUBLESHOOTING.md) for details.

### Frontend Setup
```bash
cd frontend
npm install
npm start
```

## Environment Variables

Create a `.env` file in the backend directory:
```
OPENAI_API_KEY=your_key_here
ANTHROPIC_API_KEY=your_key_here
MAX_UPLOAD_SIZE=100MB
```

## Usage

1. Access the web UI at `http://localhost:3000`
2. Upload a file or ZIP containing legacy code
3. View analysis results including:
   - Code metrics and statistics
   - Dependency graphs
   - Database operations
   - AI-generated explanations
4. Export results as JSON, PDF, or HTML

## Supported Languages

- COBOL
- Java
- C/C++
- Python
- JavaScript
- SQL
- And more...
