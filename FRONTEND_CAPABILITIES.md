# Frontend Capabilities - Business Rules Extraction Framework

## Overview

The frontend is a **React + TypeScript** application built with **Vite** that provides a modern, interactive user interface for analyzing legacy codebases.

## Technology Stack

### Core Framework
- **React 18.2** - Modern React with hooks
- **TypeScript 5.3** - Type-safe development
- **Vite 5.0** - Fast build tool and dev server
- **React Router 6.20** - Client-side routing

### UI Libraries
- **Recharts 2.10** - Charts and data visualizations
- **React Force Graph 2D 1.25** - Interactive dependency graphs
- **React Syntax Highlighter 15.5** - Code syntax highlighting
- **React Dropzone 14.2** - Drag-and-drop file upload
- **React Markdown 9.0** - Markdown rendering
- **Lucide React 0.294** - Modern icon library

### HTTP Client
- **Axios 1.6** - API communication

## Current Features

### 1. **Upload Page** ([UploadPage.tsx](frontend/src/pages/UploadPage.tsx))

#### Features:
- ✅ **Drag-and-drop file upload**
- ✅ **Click to browse file selection**
- ✅ **ZIP file support**
- ✅ **Multiple language support** (currently shows Java, Python, etc.)
- ✅ **Loading states** with spinner
- ✅ **Error handling** and user feedback
- ✅ **Responsive design**

#### Accepted File Types:
Currently configured to accept:
- ZIP archives (`.zip`)
- COBOL files (`.cbl`, `.cob`, `.cobol`)
- Java files (`.java`)
- Python files (`.py`)
- JavaScript/TypeScript (`.js`, `.ts`)
- C/C++ (`.c`, `.cpp`)
- SQL (`.sql`)
- Text files (`.txt`, `.log`)

**Note**: Should be updated to only show COBOL, SQL, and AS400/RPG since backend now only supports these.

### 2. **Analysis Page** ([AnalysisPage.tsx](frontend/src/pages/AnalysisPage.tsx))

#### Tab-Based Interface:
The analysis page features 6 tabs for different analysis views:

1. **Overview Tab** ✅
2. **Metrics Tab** ✅
3. **Dependencies Tab** ✅
4. **Database Tab** ✅
5. **Business Rules Tab** ✅
6. **AI Insights Tab** ✅

#### Features:
- ✅ **Tab navigation** for different analysis views
- ✅ **Export to JSON** functionality
- ✅ **Back to upload** navigation
- ✅ **Loading states** during analysis
- ✅ **Error handling**
- ✅ **Responsive layout**

### 3. **Overview Tab** ([OverviewTab.tsx](frontend/src/components/OverviewTab.tsx))

#### Displays:
- 📊 **Summary Statistics Cards**:
  - Total Files
  - Total Size (MB)
  - Lines of Code
  - Source Lines
  - Database Operations count
  - Business Rules count

- 📈 **Pie Chart**: File types distribution
- 📊 **Bar Chart**: Code composition (source lines, comments, blank lines)

### 4. **Metrics Tab** ([MetricsTab.tsx](frontend/src/components/MetricsTab.tsx))

#### Displays:
- Detailed LOC metrics by file
- Complexity metrics
- Function counts
- File-by-file breakdown
- Sortable tables

### 5. **Dependencies Tab** ([DependenciesTab.tsx](frontend/src/components/DependenciesTab.tsx))

#### Features:
- 🕸️ **Interactive dependency graph** using react-force-graph-2d
- Visual representation of code dependencies
- Node and edge visualization
- Interactive exploration

### 6. **Database Tab** ([DatabaseTab.tsx](frontend/src/components/DatabaseTab.tsx))

#### Displays:
- SQL queries extracted from code
- Query types (SELECT, INSERT, UPDATE, DELETE)
- Database operations by type
- File locations of queries
- Line numbers

### 7. **Business Rules Tab** ([BusinessRulesTab.tsx](frontend/src/components/BusinessRulesTab.tsx))

#### Features:
- Extracted business rules listing
- Rule types categorization
- Source file locations
- Code snippets
- Line numbers

### 8. **AI Insights Tab** ([AIInsightsTab.tsx](frontend/src/components/AIInsightsTab.tsx))

#### Features:
- AI-powered code explanations
- Codebase summarization
- Provider selection (Anthropic, OpenAI, etc.)
- Natural language insights

## API Integration

### Current API Service ([api.ts](frontend/src/services/api.ts))

#### Upload Endpoints:
```typescript
uploadFile(file: File): Promise<UploadResponse>
deleteUpload(uploadId: string): Promise<void>
```

#### Analysis Endpoints:
```typescript
analyzeFullCodebase(uploadId: string): Promise<AnalysisResults>
analyzeMetrics(uploadId: string): Promise<any>
analyzeDependencies(uploadId: string): Promise<any>
analyzeDatabase(uploadId: string): Promise<any>
extractBusinessRules(uploadId: string): Promise<any>
getFileContent(uploadId: string, filePath: string): Promise<any>
```

#### AI Endpoints:
```typescript
explainCode(request: ExplanationRequest): Promise<ExplanationResponse>
summarizeCodebase(uploadId: string, provider: string): Promise<SummaryResponse>
getAvailableProviders(): Promise<any>
```

## What's Missing for ZIP Explorer

The frontend **does NOT currently have** UI for the new ZIP Explorer API endpoints:

### Missing Features:

1. **No ZIP Explorer Page/Tab**
   - No UI to explore ZIP structure
   - No file tree visualization
   - No LOC summary display
   - No language distribution charts

2. **No ZIP-Specific API Calls**
   - Missing calls to `/api/zip/*` endpoints
   - Not using the new ZIP analysis features

3. **No Metadata Display**
   - Can't show file metadata (size, compression, dates)
   - Can't show directory structure
   - Can't filter by language/extension

## Recommended Frontend Enhancements

### 1. **Create ZIP Explorer Page**

Add a new page/tab to display ZIP analysis:

```typescript
// frontend/src/pages/ZipExplorerPage.tsx
interface ZipExplorerPageProps {
  uploadId: string
}

Features needed:
- File tree component (hierarchical view)
- Summary cards (total files, LOC, size)
- Language distribution pie chart
- Directory breakdown
- File listing with filters
- File preview modal
```

### 2. **Update API Service**

Add ZIP explorer endpoints:

```typescript
// frontend/src/services/api.ts

// Add new endpoints
uploadZip: async (file: File) => {...}
getZipSummary: async (uploadId: string) => {...}
exploreZip: async (uploadId: string, detailed: boolean) => {...}
listZipFiles: async (uploadId: string, filters: {
  language?: string
  extension?: string
}) => {...}
getZipFileContent: async (uploadId: string, filePath: string) => {...}
getZipStatistics: async (uploadId: string) => {...}
```

### 3. **Create Reusable Components**

#### File Tree Component
```typescript
// components/FileTree.tsx
- Hierarchical folder/file display
- Expandable/collapsible folders
- File icons by type
- Click to preview
```

#### LOC Metrics Card
```typescript
// components/LOCMetrics.tsx
- Total LOC display
- By language breakdown
- By directory breakdown
- Progress bars
```

#### Language Distribution Chart
```typescript
// components/LanguageChart.tsx
- Pie chart of languages
- Percentage breakdowns
- Interactive legend
```

#### File Metadata Table
```typescript
// components/FileMetadataTable.tsx
- Sortable columns
- Filtering by language/extension
- Size, LOC, compression info
- Pagination
```

### 4. **Update Upload Page**

Modify [UploadPage.tsx](frontend/src/pages/UploadPage.tsx):

```typescript
// Update supported languages display
const supportedLanguages = ['COBOL', 'SQL', 'AS400/RPG']

// Remove Java, Python, etc. from the badges

// Update accepted file types to only:
accept: {
  'application/zip': ['.zip'],
  'text/x-cobol': ['.cbl', '.cob', '.cobol', '.cpy'],
  'text/x-sql': ['.sql', '.ddl', '.dml'],
  'text/x-rpg': ['.rpg', '.rpgle', '.rpglec', '.sqlrpgle']
}
```

### 5. **Add ZIP Analysis Tab**

In [AnalysisPage.tsx](frontend/src/pages/AnalysisPage.tsx), add a new tab:

```typescript
<button
  className={`tab ${activeTab === 'zip' ? 'active' : ''}`}
  onClick={() => setActiveTab('zip')}
>
  ZIP Explorer
</button>

{activeTab === 'zip' && <ZipExplorerTab uploadId={uploadId!} />}
```

## Proposed New Components

### 1. ZipExplorerTab Component
```typescript
// components/ZipExplorerTab.tsx

import React, { useState, useEffect } from 'react'
import api from '../services/api'

const ZipExplorerTab: React.FC<{ uploadId: string }> = ({ uploadId }) => {
  const [summary, setSummary] = useState(null)
  const [explorerData, setExplorerData] = useState(null)
  const [loading, setLoading] = useState(true)

  // Features:
  // - Quick summary cards
  // - File tree visualization
  // - Language distribution chart
  // - Statistics tables
  // - File filtering

  return (...)
}
```

### 2. FileTreeViewer Component
```typescript
// components/FileTreeViewer.tsx

interface TreeNode {
  name: string
  type: 'file' | 'folder'
  children?: TreeNode[]
  size?: number
  path?: string
}

const FileTreeViewer: React.FC<{ tree: TreeNode }> = ({ tree }) => {
  // Recursive tree rendering
  // Folder expand/collapse
  // File click to preview
  return (...)
}
```

### 3. ZipSummaryCards Component
```typescript
// components/ZipSummaryCards.tsx

const ZipSummaryCards: React.FC<{ summary: ZipSummary }> = ({ summary }) => {
  return (
    <div className="grid">
      <StatCard title="Total Files" value={summary.total_files} />
      <StatCard title="Total Size" value={`${summary.total_size_mb} MB`} />
      <StatCard title="Total LOC" value={summary.total_loc.toLocaleString()} />
      <StatCard title="COBOL Files" value={cobolCount} />
      <StatCard title="SQL Files" value={sqlCount} />
      <StatCard title="AS400 Files" value={as400Count} />
    </div>
  )
}
```

### 4. LanguageDistributionChart Component
```typescript
// components/LanguageDistributionChart.tsx

import { PieChart, Pie } from 'recharts'

const LanguageDistributionChart: React.FC = ({ languages }) => {
  // Pie chart showing language distribution
  // Color-coded by language
  // Percentage labels
  return (...)
}
```

### 5. FileListTable Component
```typescript
// components/FileListTable.tsx

const FileListTable: React.FC<{ files: FileMetadata[] }> = ({ files }) => {
  const [filter, setFilter] = useState({ language: '', extension: '' })
  const [sortBy, setSortBy] = useState('name')

  // Features:
  // - Sortable columns (name, size, LOC)
  // - Filter by language dropdown
  // - Filter by extension dropdown
  // - Search by filename
  // - Click row to preview file

  return (...)
}
```

## Directory Structure for New Features

```
frontend/src/
├── pages/
│   ├── UploadPage.tsx          # Update to show only COBOL/SQL/AS400
│   ├── AnalysisPage.tsx        # Add ZIP Explorer tab
│   └── ZipExplorerPage.tsx     # NEW - Dedicated ZIP explorer page (optional)
│
├── components/
│   ├── OverviewTab.tsx         # Existing
│   ├── MetricsTab.tsx          # Existing
│   ├── DependenciesTab.tsx     # Existing
│   ├── DatabaseTab.tsx         # Existing
│   ├── BusinessRulesTab.tsx    # Existing
│   ├── AIInsightsTab.tsx       # Existing
│   ├── ZipExplorerTab.tsx      # NEW - ZIP analysis tab
│   ├── FileTreeViewer.tsx      # NEW - Tree visualization
│   ├── ZipSummaryCards.tsx     # NEW - Summary metrics
│   ├── LanguageDistributionChart.tsx  # NEW - Language pie chart
│   ├── FileListTable.tsx       # NEW - Filterable file list
│   └── FilePreviewModal.tsx    # NEW - Modal to show file content
│
└── services/
    └── api.ts                  # Update with ZIP endpoints
```

## Sample UI Mockup

```
┌─────────────────────────────────────────────────────────────┐
│  Business Rules Extraction Framework                        │
│                                                              │
│  [Overview] [Metrics] [Dependencies] [Database] [Rules]     │
│  [AI Insights] [ZIP Explorer] ← NEW TAB                     │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  ZIP Explorer                                                │
│                                                              │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐      │
│  │ 150      │ │ 45.6 MB  │ │ 25,000   │ │ 50       │      │
│  │ Files    │ │ Size     │ │ LOC      │ │ COBOL    │      │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘      │
│                                                              │
│  ┌────────────────────┐  ┌──────────────────────────────┐  │
│  │ File Tree          │  │ Language Distribution        │  │
│  │                    │  │                              │  │
│  │ 📁 src/            │  │  [Pie Chart]                │  │
│  │   📁 cobol/        │  │  60% COBOL                  │  │
│  │     📄 main.cbl    │  │  25% SQL                    │  │
│  │     📄 utils.cbl   │  │  15% AS400                  │  │
│  │   📁 sql/          │  │                              │  │
│  │     📄 schema.sql  │  │                              │  │
│  │ 📁 docs/           │  │                              │  │
│  └────────────────────┘  └──────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Files List                                           │  │
│  │ Filter: [Language ▼] [Extension ▼] [Search...]     │  │
│  │                                                       │  │
│  │ Name          │ Size   │ LOC   │ Language │ Type    │  │
│  │ main.cbl      │ 48 KB  │ 1200  │ COBOL    │ Source  │  │
│  │ utils.cbl     │ 35 KB  │ 890   │ COBOL    │ Source  │  │
│  │ schema.sql    │ 12 KB  │ 340   │ SQL      │ DDL     │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Next Steps to Implement

### Priority 1: Update Existing UI
1. ✅ Update [UploadPage.tsx](frontend/src/pages/UploadPage.tsx) to show only COBOL, SQL, AS400
2. ✅ Remove references to Java, Python, etc.
3. ✅ Update accepted file types

### Priority 2: Add ZIP API Integration
1. ✅ Update [api.ts](frontend/src/services/api.ts) with ZIP endpoints
2. ✅ Add TypeScript interfaces for ZIP responses
3. ✅ Add error handling

### Priority 3: Create ZIP Explorer Components
1. ✅ Create `ZipExplorerTab.tsx`
2. ✅ Create `FileTreeViewer.tsx`
3. ✅ Create `ZipSummaryCards.tsx`
4. ✅ Create `LanguageDistributionChart.tsx`
5. ✅ Create `FileListTable.tsx`
6. ✅ Create `FilePreviewModal.tsx`

### Priority 4: Integration
1. ✅ Add ZIP Explorer tab to [AnalysisPage.tsx](frontend/src/pages/AnalysisPage.tsx)
2. ✅ Wire up all components
3. ✅ Test end-to-end flow
4. ✅ Add loading states
5. ✅ Add error handling

## Running the Frontend

```bash
# Install dependencies
cd frontend
npm install

# Start dev server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

Frontend will run on `http://localhost:5173` (Vite default) or configured port.

## Current State Summary

### ✅ **What Works**
- File upload (drag-and-drop or click)
- Full codebase analysis
- Multiple analysis tabs
- Charts and visualizations
- AI-powered insights
- Export to JSON
- Responsive design

### ❌ **What's Missing for ZIP Explorer**
- No ZIP-specific UI
- No file tree visualization
- No LOC summary display
- No language distribution for ZIP
- No ZIP metadata display
- Not using new `/api/zip/*` endpoints

### 🔧 **What Needs Updating**
- Remove Java, Python from supported languages
- Update file type acceptance
- Add ZIP explorer tab/page
- Connect to new backend ZIP APIs

---

**Recommendation**: Implement the ZIP Explorer UI to match the powerful backend capabilities you just built!
