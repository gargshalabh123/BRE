# Mainframe Dependencies Enhancement

## Overview
Enhanced the dependency tracking system to capture critical COBOL/mainframe-specific dependencies that were previously missing, including CICS program control, transaction management, BMS screen dependencies, and IMS database access patterns.

## What Was Added

### New Dependency Types

#### 1. CICS Program Control
- **`CICS_XCTL`** - Transfer control to another program (no return)
  - Pattern: `EXEC CICS XCTL PROGRAM('PROGNAME')`
  - Color: Pink (#fce4ec / #c2185b)
  - Use case: Program flow tracking, modernization planning

- **`CICS_LINK`** - Link to another program (returns control)
  - Pattern: `EXEC CICS LINK PROGRAM('PROGNAME')`
  - Color: Purple (#f3e5f5 / #7b1fa2)
  - Use case: Subroutine call patterns, API conversion candidates

#### 2. CICS Transaction Management
- **`CICS_RETURN_TRANSID`** - Return to CICS with next transaction ID
  - Pattern: `EXEC CICS RETURN TRANSID('ABCD')`
  - Color: Yellow (#fff9c4 / #f57f17)
  - Use case: Transaction flow mapping, pseudo-conversational patterns

- **`CICS_START_TRANSID`** - Start a new transaction
  - Pattern: `EXEC CICS START TRANSID('ABCD')`
  - Color: Orange (#fff3e0 / #e65100)
  - Use case: Asynchronous processing, batch initiation

#### 3. BMS Screen Dependencies
- **`BMS_SEND_MAP`** - Send screen/map to terminal
  - Pattern: `EXEC CICS SEND MAP('MAPNAME') MAPSET('MAPSET1')`
  - Color: Teal (#e0f2f1 / #00695c)
  - Parameters: [mapset_name, map_name]
  - Use case: UI modernization, screen flow documentation

- **`BMS_RECEIVE_MAP`** - Receive input from screen/map
  - Pattern: `EXEC CICS RECEIVE MAP('MAPNAME') MAPSET('MAPSET1')`
  - Color: Green (#e8f5e9 / #2e7d32)
  - Parameters: [mapset_name, map_name]
  - Use case: Input handling, form processing

#### 4. IMS Database Dependencies
- **`IMS_DLI_CALL`** - IMS DL/I database operation
  - Pattern: `CALL 'CBLTDLI' USING GU-FUNCTION ...`
  - Color: Deep Purple (#ede7f6 / #4527a0)
  - Use case: Database access patterns, segment usage

- **`IMS_PSB`** - Program Specification Block reference
  - Pattern: `PSB: PSBNAME` (in comments or metadata)
  - Color: Purple (#f3e5f5 / #6a1b9a)
  - Use case: Database schema dependencies

## Files Modified

### Backend

#### `backend/analyzers/dependency_analyzer.py`
**Enhanced `analyze_cobol_calls()` method** with:
- Comment line filtering to prevent false positives
- 8 new regex patterns for mainframe-specific dependencies

**Comment Detection Logic:**
```python
# COBOL comment detection - comments have * or / in column 7 (index 6)
if len(line) >= 7:
    indicator_col = line[6] if len(line) > 6 else ''
    if indicator_col in ['*', '/', '$']:
        continue  # Skip comment lines
    clean_line = line[6:72].strip()
else:
    # Short line or free-format, check if starts with comment
    if line.strip().startswith('*'):
        continue
    clean_line = line.strip()
```

This prevents false positives like:
```cobol
* You may obtain a copy of the License at
```
from being detected as a COPY statement.

**New Dependency Pattern Examples:**

```python
# EXEC CICS XCTL
xctl_match = re.search(
    r'EXEC\s+CICS\s+XCTL\s+PROGRAM\([\'"]?([A-Z0-9\-]+)[\'"]?\)',
    clean_line, re.IGNORECASE
)

# BMS Map usage
send_map_match = re.search(
    r'EXEC\s+CICS\s+SEND\s+MAP\([\'"]?([A-Z0-9]+)[\'"]?\)\s+MAPSET\([\'"]?([A-Z0-9]+)[\'"]?\)',
    clean_line, re.IGNORECASE
)

# IMS DL/I calls
ims_call_match = re.search(
    r'\bCALL\s+[\'"]?CBLTDLI[\'"]?\s+USING\s+([A-Z0-9\-]+)',
    clean_line, re.IGNORECASE
)
```

Each pattern extracts:
- `target` - Program/transaction/map being referenced
- `type` - Dependency classification
- `line` - Line number in source
- `signature` - Full statement (truncated to 200 chars)
- `parameters` - Extracted parameter names (especially for BMS maps)
- `description` - Human-readable explanation

### Frontend

#### `frontend/src/utils/dependencyColors.ts` (NEW FILE)
**Centralized styling utility** to eliminate code duplication and ensure consistency:

```typescript
export interface DependencyTypeStyle {
  backgroundColor: string
  color: string
}

export const getDependencyTypeStyle = (type: string): DependencyTypeStyle => {
  switch (type) {
    case 'CICS_XCTL':
      return { backgroundColor: '#fce4ec', color: '#c2185b' }
    case 'CICS_LINK':
      return { backgroundColor: '#f3e5f5', color: '#7b1fa2' }
    case 'CICS_RETURN_TRANSID':
      return { backgroundColor: '#fff9c4', color: '#f57f17' }
    case 'CICS_START_TRANSID':
      return { backgroundColor: '#fff3e0', color: '#e65100' }
    case 'BMS_SEND_MAP':
      return { backgroundColor: '#e0f2f1', color: '#00695c' }
    case 'BMS_RECEIVE_MAP':
      return { backgroundColor: '#e8f5e9', color: '#2e7d32' }
    case 'IMS_DLI_CALL':
      return { backgroundColor: '#ede7f6', color: '#4527a0' }
    case 'IMS_PSB':
      return { backgroundColor: '#f3e5f5', color: '#6a1b9a' }
    // ... existing types ...
    default:
      return { backgroundColor: '#f5f5f5', color: '#424242' }
  }
}

export const getDependencyTypeLabel = (type: string): string => {
  return type.replace(/_/g, ' ')
}

export const getDependencyTypeCategory = (type: string): string => {
  if (['PROGRAM_CALL', 'CICS_XCTL', 'CICS_LINK'].includes(type)) {
    return 'Program Control'
  }
  if (['CICS_RETURN_TRANSID', 'CICS_START_TRANSID'].includes(type)) {
    return 'CICS Transactions'
  }
  if (['BMS_SEND_MAP', 'BMS_RECEIVE_MAP'].includes(type)) {
    return 'Screen/BMS'
  }
  if (['IMS_DLI_CALL', 'IMS_PSB'].includes(type)) {
    return 'IMS Database'
  }
  // ... more categories ...
}
```

#### `frontend/src/pages/FileDetailPage.tsx`
**Updated to use utility functions:**

```typescript
import { getDependencyTypeStyle, getDependencyTypeLabel } from '../utils/dependencyColors'

// In dependency type badge rendering:
<span style={{
  padding: '3px 8px',
  borderRadius: '4px',
  fontSize: '0.85em',
  fontWeight: 'bold',
  ...getDependencyTypeStyle(dep.type)  // Spread operator for styles
}}>
  {getDependencyTypeLabel(dep.type)}   // Formatted label
</span>
```

#### `frontend/src/components/DependenciesTab.tsx` & `frontend/src/pages/FileDetailPage.tsx`
**Updated type breakdown badges and table with dependency grouping on BOTH main Dependencies tab and File Detail page:**

**Dependency Grouping Logic:**
```typescript
// Group dependencies by file, type, and target to consolidate duplicate entries
const groupedDependencies = hasDetailedDeps ? (() => {
  const grouped: Record<string, any[]> = {}

  Object.entries(filteredDependencies).forEach(([file, deps]) => {
    (deps as any[])
      .filter(dep => filterByType === 'ALL' || dep.type === filterByType)
      .forEach(dep => {
        // Create a unique key for file + type + target
        const key = `${file}|||${dep.type}|||${dep.target}`
        if (!grouped[key]) {
          grouped[key] = []
        }
        grouped[key].push(dep)
      })
  })

  // Convert grouped dependencies to consolidated format
  return Object.entries(grouped).map(([key, deps]) => {
    const [file, type, target] = key.split('|||')
    const lines = deps.map(d => d.line).sort((a, b) => a - b)
    const firstDep = deps[0]

    return {
      file,
      type,
      target,
      lines, // Array of all line numbers
      count: deps.length, // How many times this dependency appears
      signature: firstDep.signature,
      parameters: firstDep.parameters,
      description: firstDep.description
    }
  })
})() : []
```

**Benefits:**
- **Reduces table verbosity** - Instead of showing 10 rows for a paragraph performed 10 times, shows 1 row with count badge
- **Aggregates line numbers** - Shows all occurrence line numbers: `125, 148, 201, 245, 289`
- **Smart truncation** - For many occurrences, shows: `125, 148, 201, ... 445, 489`
- **Count badge** - Yellow badge highlights dependencies called multiple times
- **Maintains all information** - Same signature, parameters, and description

**Implementation Differences:**
- **Main Dependencies Tab** (`DependenciesTab.tsx`): Groups by `file + type + target` to consolidate across all files
- **File Detail Page** (`FileDetailPage.tsx`): Groups by `type + target` (file already filtered to current file)

**Enhanced Table Columns:**
- **Count Column**: Badge showing how many times this dependency appears (yellow if > 1)
- **Lines Column**: Comma-separated list of all line numbers where dependency occurs

```typescript
// Type breakdown badges:
<div style={{
  padding: '10px 15px',
  borderRadius: '8px',
  ...getDependencyTypeStyle(type),
  fontWeight: 'bold',
  fontSize: '0.9em'
}}>
  {getDependencyTypeLabel(type)}: {count}
</div>

// Count badge with conditional styling:
<span style={{
  padding: '2px 8px',
  backgroundColor: dep.count > 1 ? '#fff3cd' : '#e9ecef',
  borderRadius: '12px',
  fontSize: '0.85em',
  fontWeight: 'bold',
  color: dep.count > 1 ? '#856404' : '#495057'
}}>
  {dep.count}
</span>

// Lines display with smart truncation:
{dep.lines.length <= 5 ? (
  dep.lines.join(', ')
) : (
  `${dep.lines.slice(0, 3).join(', ')}, ... ${dep.lines.slice(-2).join(', ')}`
)}
```

## Dependency Category System

Dependencies are now organized into logical categories:

### Program Control
- `PROGRAM_CALL` - Standard COBOL CALL
- `CICS_XCTL` - Transfer control (no return)
- `CICS_LINK` - Call and return

**Modernization Impact:** These represent inter-program communication that needs to be converted to API calls or microservices

### CICS Transactions
- `CICS_RETURN_TRANSID` - Pseudo-conversational return
- `CICS_START_TRANSID` - Asynchronous transaction start

**Modernization Impact:** Transaction flow must be preserved in modern architecture (session management, async processing)

### Screen/BMS
- `BMS_SEND_MAP` - Screen output
- `BMS_RECEIVE_MAP` - Screen input

**Modernization Impact:** Direct mapping to UI modernization - each map becomes a web form or API endpoint

### IMS Database
- `IMS_DLI_CALL` - Database operations (GU, GN, ISRT, DLET, REPL)
- `IMS_PSB` - Program Specification Block

**Modernization Impact:** Database access patterns that need SQL/ORM conversion

### Internal Flow
- `PERFORM_PARAGRAPH` - Internal paragraph calls

**Modernization Impact:** Can be refactored into functions/methods

### Code Reuse
- `COPYBOOK` - Shared code/data structures

**Modernization Impact:** Becomes shared libraries or domain objects

### Module Import
- `IMPORT`, `FROM_IMPORT`, `REQUIRE` - Modern language imports

### Function Call
- `METHOD_CALL`, `FUNCTION_CALL` - Modern language function/method calls

## Dependency Grouping Feature

### Before (Verbose Table)
Without grouping, PERFORM PARAGRAPH dependencies would create many duplicate rows:

| File | Type | Target | Line | Signature | Parameters | Description |
|------|------|--------|------|-----------|------------|-------------|
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | 125 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | 148 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | 201 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | 245 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | 289 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 3000-PROCESS-ACCOUNT | 167 | PERFORM 3000-PROCESS-ACCOUNT | - | Performs paragraph 3000-PROCESS-ACCOUNT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 3000-PROCESS-ACCOUNT | 312 | PERFORM 3000-PROCESS-ACCOUNT | - | Performs paragraph 3000-PROCESS-ACCOUNT |

**Problem:** 7 rows for just 2 unique paragraphs - hard to see the big picture!

### After (Grouped Table)
With grouping, dependencies are consolidated:

| File | Type | Target | Count | Lines | Signature | Parameters | Description |
|------|------|--------|-------|-------|-----------|------------|-------------|
| ACCTPGM.cbl | PERFORM PARAGRAPH | 2000-VALIDATE-INPUT | **5** | 125, 148, 201, 245, 289 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| ACCTPGM.cbl | PERFORM PARAGRAPH | 3000-PROCESS-ACCOUNT | **2** | 167, 312 | PERFORM 3000-PROCESS-ACCOUNT | - | Performs paragraph 3000-PROCESS-ACCOUNT |

**Benefits:**
- **70% fewer rows** - Much more readable
- **Count badges** - Instantly see frequently-used dependencies (yellow badge when count > 1)
- **All line numbers visible** - Can still see every occurrence
- **Smart truncation** - For 50+ occurrences, shows first 3 and last 2 with "..." in between

## Example Output

### COBOL CICS Program Analysis

**Source Code:**
```cobol
EXEC CICS XCTL PROGRAM('ACCT02') END-EXEC
EXEC CICS LINK PROGRAM('VALPGM') END-EXEC
EXEC CICS RETURN TRANSID('ACT2') END-EXEC
EXEC CICS START TRANSID('BTCH') END-EXEC
EXEC CICS SEND MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC
EXEC CICS RECEIVE MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC
CALL 'CBLTDLI' USING GU-FUNCTION PCB-MASK SEGMENT-IO
```

**Dependencies Tab Display:**

| Type | Target | Line | Signature | Parameters | Description |
|------|--------|------|-----------|------------|-------------|
| <span style="background:#fce4ec;color:#c2185b">CICS XCTL</span> | ACCT02 | 145 | EXEC CICS XCTL PROGRAM('ACCT02') END-EXEC | - | CICS XCTL to program ACCT02 |
| <span style="background:#f3e5f5;color:#7b1fa2">CICS LINK</span> | VALPGM | 156 | EXEC CICS LINK PROGRAM('VALPGM') END-EXEC | - | CICS LINK to program VALPGM |
| <span style="background:#fff9c4;color:#f57f17">CICS RETURN TRANSID</span> | ACT2 | 178 | EXEC CICS RETURN TRANSID('ACT2') END-EXEC | - | Returns to CICS transaction ACT2 |
| <span style="background:#fff3e0;color:#e65100">CICS START TRANSID</span> | BTCH | 189 | EXEC CICS START TRANSID('BTCH') END-EXEC | - | Starts CICS transaction BTCH |
| <span style="background:#e0f2f1;color:#00695c">BMS SEND MAP</span> | ACCTSET/ACCMAP | 201 | EXEC CICS SEND MAP('ACCMAP') MAPSET('ACCTSET') | ACCTSET, ACCMAP | Sends BMS map ACCMAP from mapset ACCTSET |
| <span style="background:#e8f5e9;color:#2e7d32">BMS RECEIVE MAP</span> | ACCTSET/ACCMAP | 212 | EXEC CICS RECEIVE MAP('ACCMAP') MAPSET('ACCTSET') | ACCTSET, ACCMAP | Receives BMS map ACCMAP from mapset ACCTSET |
| <span style="background:#ede7f6;color:#4527a0">IMS DLI CALL</span> | GU-FUNCTION | 225 | CALL 'CBLTDLI' USING GU-FUNCTION PCB-MASK SEGMENT-IO | - | IMS DL/I call using function GU-FUNCTION |

## Benefits for Modernization

### 1. Program Flow Mapping
- **XCTL chains:** Trace program-to-program transfers
- **LINK patterns:** Identify reusable subroutines
- **Transaction flows:** Map pseudo-conversational patterns

### 2. UI Modernization Planning
- **Screen inventory:** All BMS maps used by each program
- **Input/Output mapping:** Which programs send vs. receive each map
- **Form dependencies:** Which mapsets are critical

### 3. Database Access Patterns
- **IMS operations:** What type of database operations (GET, INSERT, etc.)
- **PSB dependencies:** Which database schemas are used
- **Data flow:** Parameters passed to DL/I calls

### 4. Architecture Documentation
- **Component boundaries:** XCTL vs LINK shows architectural patterns
- **Async processing:** START TRANSID identifies background jobs
- **Session management:** RETURN TRANSID shows conversational state

### 5. API Design
- **LINK calls** → REST API endpoints
- **BMS SEND/RECEIVE** → Web forms or API payloads
- **Transaction IDs** → Microservice names
- **IMS calls** → Database queries or ORM operations

## Testing the Enhancement

### 1. Upload COBOL Codebase
Upload a COBOL codebase containing:
- CICS programs with XCTL, LINK commands
- Programs with EXEC CICS RETURN TRANSID
- BMS map usage (SEND MAP, RECEIVE MAP)
- IMS database calls

### 2. Navigate to Dependencies Tab
On the main analysis page, click the "Dependencies" tab

### 3. Verify Display
Check that:
- ✅ All 8 new dependency types appear in "Dependency Types Breakdown"
- ✅ Each type has correct color coding
- ✅ Type filter dropdown includes all new types
- ✅ Detailed table shows correct information:
  - Target programs/transactions/maps
  - Line numbers
  - Full signatures
  - Parameters (especially for BMS maps)
  - Descriptions

### 4. Check File Detail Page
Click on a COBOL file from the Metrics tab, then Dependencies tab:
- ✅ New dependency types are displayed with correct colors
- ✅ Utility functions work correctly
- ✅ Labels are formatted properly (spaces instead of underscores)

## Color Coding Reference

| Type | Category | Background | Text Color | Hex Codes |
|------|----------|------------|------------|-----------|
| PROGRAM_CALL | Program Control | Light Blue | Dark Blue | #e3f2fd / #1565c0 |
| CICS_XCTL | Program Control | Pink | Dark Pink | #fce4ec / #c2185b |
| CICS_LINK | Program Control | Light Purple | Dark Purple | #f3e5f5 / #7b1fa2 |
| CICS_RETURN_TRANSID | CICS Transactions | Light Yellow | Dark Orange | #fff9c4 / #f57f17 |
| CICS_START_TRANSID | CICS Transactions | Light Orange | Dark Orange | #fff3e0 / #e65100 |
| BMS_SEND_MAP | Screen/BMS | Light Teal | Dark Teal | #e0f2f1 / #00695c |
| BMS_RECEIVE_MAP | Screen/BMS | Light Green | Dark Green | #e8f5e9 / #2e7d32 |
| IMS_DLI_CALL | IMS Database | Light Deep Purple | Deep Purple | #ede7f6 / #4527a0 |
| IMS_PSB | IMS Database | Light Purple | Dark Purple | #f3e5f5 / #6a1b9a |
| PERFORM_PARAGRAPH | Internal Flow | Light Purple | Dark Purple | #f3e5f5 / #6a1b9a |
| COPYBOOK | Code Reuse | Light Green | Dark Green | #e8f5e9 / #2e7d32 |
| IMPORT | Module Import | Light Orange | Dark Orange | #fff3e0 / #e65100 |
| FROM_IMPORT | Module Import | Light Orange | Darker Orange | #ffe0b2 / #ef6c00 |
| REQUIRE | Module Import | Light Yellow-Orange | Dark Orange | #fff8e1 / #f57c00 |
| METHOD_CALL | Function Call | Pink | Dark Pink | #fce4ec / #c2185b |
| FUNCTION_CALL | Function Call | Light Teal | Dark Teal | #e0f2f1 / #00695c |

## Code Maintainability

### Before (Duplicated):
```typescript
// FileDetailPage.tsx
backgroundColor:
  dep.type === 'PROGRAM_CALL' ? '#e3f2fd' :
  dep.type === 'PERFORM_PARAGRAPH' ? '#f3e5f5' :
  // ... 15 more cases ...

// DependenciesTab.tsx (Type breakdown)
backgroundColor:
  type === 'PROGRAM_CALL' ? '#e3f2fd' :
  type === 'PERFORM_PARAGRAPH' ? '#f3e5f5' :
  // ... 15 more cases ...

// DependenciesTab.tsx (Table)
backgroundColor:
  dep.type === 'PROGRAM_CALL' ? '#e3f2fd' :
  dep.type === 'PERFORM_PARAGRAPH' ? '#f3e5f5' :
  // ... 15 more cases ...
```

**Problem:** 3 places to update when adding new dependency types!

### After (Centralized):
```typescript
// dependencyColors.ts (single source of truth)
export const getDependencyTypeStyle = (type: string): DependencyTypeStyle => {
  switch (type) {
    case 'PROGRAM_CALL': return { backgroundColor: '#e3f2fd', color: '#1565c0' }
    case 'CICS_XCTL': return { backgroundColor: '#fce4ec', color: '#c2185b' }
    // ... all types defined once
  }
}

// All components:
<span style={{ ...getDependencyTypeStyle(dep.type) }}>
  {getDependencyTypeLabel(dep.type)}
</span>
```

**Benefit:** Add new dependency types in ONE place, automatically propagates everywhere!

## Summary

This enhancement provides comprehensive mainframe dependency tracking that was previously missing:

✅ **CICS Program Control** - XCTL, LINK patterns for program flow
✅ **CICS Transactions** - RETURN TRANSID, START TRANSID for transaction mapping
✅ **BMS Screen Dependencies** - SEND MAP, RECEIVE MAP for UI modernization
✅ **IMS Database** - DL/I calls and PSB references for data access patterns
✅ **Centralized Styling** - Single utility file for maintainability
✅ **Consistent UI** - Same color coding across all views
✅ **Category System** - Logical grouping for better understanding
✅ **Backward Compatible** - Falls back to simple dependencies if detailed not available

These enhancements transform the BRE tool into a comprehensive mainframe modernization planning platform that can:
- Map complete program flows including CICS control transfers
- Document transaction chains and pseudo-conversational patterns
- Inventory all screen/UI dependencies for web conversion
- Track database access patterns for data migration
- Provide actionable insights for API design and microservices architecture
