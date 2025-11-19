# Business Rules Extraction Framework

A comprehensive full-stack application for analyzing legacy codebases (COBOL, SQL, AS400/RPG) with ZIP file exploration, code metrics, business rules extraction, and AI-powered insights.

![Version](https://img.shields.io/badge/version-1.0.0-blue)
![Python](https://img.shields.io/badge/python-3.8+-green)
![React](https://img.shields.io/badge/react-18.2-blue)
![FastAPI](https://img.shields.io/badge/fastapi-latest-teal)

---

## 🚀 Features

### Code Analysis
- **Multi-Language Support**: COBOL, SQL, AS400/RPG (RPG III, RPG IV, RPGLE)
- **Lines of Code (LOC)**: Automatic counting with breakdown by comments, blank lines, and source
- **Complexity Metrics**: Analyze code complexity and structure
- **Business Rules Extraction**: Pattern-based extraction of business logic
- **Database Operations**: Identify and catalog SQL queries and database interactions

### ZIP File Explorer
- **Complete Structure Analysis**: Hierarchical file tree visualization
- **Comprehensive Metadata**: File sizes, compression ratios, dates, MIME types
- **Language Distribution**: Visual charts and statistics
- **File Browser**: Tree view and list view with filtering
- **File Preview**: Syntax-highlighted code viewer with download capability
- **Smart Filtering**: Search, filter by language/extension, sortable columns

### AI-Powered Insights
- **Code Explanations**: Natural language explanations using Claude or GPT
- **Codebase Summaries**: Automated documentation generation
- **Multiple Providers**: Support for Anthropic Claude and OpenAI GPT

### Interactive Frontend
- **Modern React UI**: Built with TypeScript and Vite
- **7 Analysis Tabs**: Overview, Metrics, Dependencies, Database, Business Rules, AI Insights, ZIP Explorer
- **Data Visualizations**: Interactive charts and graphs using Recharts
- **Responsive Design**: Works on desktop, tablet, and mobile

---

## ⚡ Quick Start

### 1. Setup Backend
```bash
cd backend
python -m venv venv
venv\Scripts\activate  # Windows
pip install fastapi uvicorn python-dotenv python-multipart anthropic openai
```

### 2. Setup Frontend
```bash
cd frontend
npm install
```

### 3. Run Application

**Terminal 1 - Backend:**
```bash
cd backend
python main.py
```

**Terminal 2 - Frontend:**
```bash
cd frontend
npm run dev
```

### 4. Access Application
- **Frontend**: http://localhost:5173
- **API Docs**: http://localhost:8000/docs

---

## 🎯 Supported Languages

| Language | Extensions | Features |
|----------|-----------|----------|
| **COBOL** | `.cbl`, `.cob`, `.cobol`, `.cpy` | Divisions, copybooks, paragraphs, file I/O, database ops |
| **SQL** | `.sql`, `.ddl`, `.dml` | Query types, tables, DDL/DML statements |
| **AS400/RPG** | `.rpg`, `.rpgle`, `.rpglec`, `.sqlrpgle`, `.dspf`, `.prtf`, `.lf`, `.pf` | Procedures, subroutines, file definitions |

---

## 📖 Usage

### Upload and Analyze

1. Open http://localhost:5173
2. Drag and drop a ZIP file with COBOL/SQL/AS400 code
3. Click **"ZIP Explorer"** tab
4. Explore:
   - Summary statistics
   - Language distribution charts
   - File browser (tree/list views)
   - File preview with syntax highlighting
   - Search and filter capabilities

---

## 🔌 API Endpoints

### ZIP Explorer
```
POST   /api/zip/upload              # Upload ZIP
GET    /api/zip/{id}/summary        # Quick summary
GET    /api/zip/{id}/explore        # Full exploration
GET    /api/zip/{id}/files          # List files
GET    /api/zip/{id}/file/{path}    # File content
DELETE /api/zip/{id}                # Cleanup
```

### Analysis
```
POST   /api/analysis/{id}/full             # Full analysis
POST   /api/analysis/{id}/metrics          # Metrics
POST   /api/analysis/{id}/business-rules   # Business rules
```

**Full Documentation**: http://localhost:8000/docs

---

## 📚 Documentation

- **[QUICK_START.md](QUICK_START.md)** - Get started in 5 minutes
- **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)** - Production deployment
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - System architecture
- **[FRONTEND_IMPLEMENTATION.md](FRONTEND_IMPLEMENTATION.md)** - Frontend details
- **[backend/docs/ZIP_EXPLORER_USAGE.md](backend/docs/ZIP_EXPLORER_USAGE.md)** - ZIP Explorer API

---

## 🛠️ Technology Stack

**Backend**: FastAPI (Python), Custom language parsers
**Frontend**: React 18 + TypeScript + Vite
**Visualization**: Recharts
**Syntax Highlighting**: React Syntax Highlighter
**AI**: Anthropic Claude, OpenAI GPT

---

## 📊 Example Output

```json
{
  "total_files": 150,
  "total_loc": 25000,
  "languages": [
    {"language": "cobol", "file_count": 50, "percentage": 60.0},
    {"language": "sql", "file_count": 30, "percentage": 20.0}
  ]
}
```

---

## 🚢 Production Build

```bash
# Build frontend
cd frontend && npm run build

# Run backend
cd backend && python main.py
```

See [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md) for details.

---

## 📝 License

Proprietary. All rights reserved.

---

## 📈 Version

**v1.0.0** (2025-11-18)
- Complete ZIP Explorer (backend + frontend)
- COBOL, SQL, AS400/RPG support
- Interactive visualizations
- AI-powered insights

---

**Made with ❤️ for legacy code analysis**
