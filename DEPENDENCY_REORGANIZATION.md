# Dependency Type Reorganization

## Overview

Reorganized dependency types to display in their most appropriate sections based on their purpose and context.

---

## Changes Made

### 1. CONDITION_88 → Moved to Business Rules

**Previous Location:** Dependencies tab
**New Location:** Business Rules tab

**Reason:** 88-level conditions represent business logic and data-driven rules, not code dependencies. They define condition names and their associated values that control business behavior.

**Display Format:**
```
Business Rules Tab:
| Line | Type              | Condition Name              | Value | Description           | Code Snippet |
|------|-------------------|-----------------------------|----|----------------------|--------------|
| 145  | 88-Level Condition| COULD-NOT-SCHEDULE-PSB      | 'TE' | 88-level condition   | 88 COULD-... |
| 146  | 88-Level Condition| STATUS-OK                   | '  ' | 88-level condition   | 88 STATUS... |
```

**Benefits:**
- ✅ Groups with other business rules
- ✅ Easy value lookup table
- ✅ Clear separation from code dependencies
- ✅ Better business logic documentation

---

### 2. IMS_DLI_OP → Moved to DB Operations

**Previous Location:** Dependencies tab
**New Location:** DB Operations tab

**Reason:** IMS DL/I operations are database operations (read, write, schedule, terminate). They represent data access patterns, not code dependencies.

**Display Format:**
```
DB Operations Tab:
| Line | Type     | Category                 | Target/Operation | Context                                          | Query/Statement |
|------|----------|--------------------------|------------------|--------------------------------------------------|----------------|
| 312  | IMS DL/I | IMS Database Operation   | GU               | GU, PCB:AUTH-PCB, SEG:AUTH-REC, LAYOUT:..., WHERE:... | EXEC DLI... |
| 348  | IMS DL/I | IMS Database Operation   | GNP              | GNP, PCB:AUTH-PCB, SEG:AUTH-REC, LAYOUT:...      | EXEC DLI... |
| 401  | IMS DL/I | IMS Database Operation   | REPL             | REPL, PCB:AUTH-PCB, SEG:AUTH-REC, LAYOUT:...     | EXEC DLI... |
| 512  | IMS DL/I | IMS Database Operation   | SCHD             | SCHD, PSB:CUSTPSB                                 | EXEC DLI... |
```

**Context Column Shows:**
- Operation type (GU, GNP, REPL, SCHD, TERM)
- PCB (Program Communication Block)
- Segment name
- Layout (data structure)
- WHERE clause
- PSB (for SCHD/TERM operations)

**Benefits:**
- ✅ Groups with other DB operations (SQL, etc.)
- ✅ Complete operation context visible
- ✅ Easy to analyze data access patterns
- ✅ Better migration planning (IMS → SQL)

---

### 3. CICS_OP → Moved to DB Operations

**Previous Location:** Dependencies tab
**New Location:** DB Operations tab

**Reason:** CICS operations (SYNCPOINT, SYNCPOINT ROLLBACK, LINK with COMMAREA) represent transaction control and inter-program communication with data flow. They belong with database/transaction operations.

**Display Format:**
```
DB Operations Tab:
| Line | Type | Category                    | Target/Operation     | Context                                        | Query/Statement |
|------|------|-----------------------------|----------------------|------------------------------------------------|----------------|
| 289  | CICS | CICS Transaction Control    | SYNCPOINT            | SYNCPOINT                                       | EXEC CICS... |
| 456  | CICS | CICS Transaction Control    | SYNCPOINT ROLLBACK   | SYNCPOINT ROLLBACK                              | EXEC CICS... |
| 523  | CICS | CICS Transaction Control    | WS-PGM-AUTH-FRAUD    | LINK, WS-PGM-AUTH-FRAUD, COMMAREA:WS-FRAUD-... | EXEC CICS... |
```

**Context Column Shows:**
- Operation type (SYNCPOINT, SYNCPOINT ROLLBACK, LINK)
- Target program (for LINK)
- COMMAREA (communication area for data exchange)

**Benefits:**
- ✅ Groups transaction control with data operations
- ✅ Shows commit/rollback boundaries
- ✅ Tracks inter-program data flow via COMMAREA
- ✅ Better transaction analysis

---

## What Remains in Dependencies Tab

**Only true code dependencies:**
- ✅ PROGRAM_CALL - External program calls
- ✅ CICS_XCTL - Program transfer control
- ✅ CICS_LINK - (Note: Now tracked as CICS_OP in DB Operations)
- ✅ CICS_RETURN_TRANSID - Transaction flow
- ✅ CICS_START_TRANSID - Async transaction start
- ✅ BMS_SEND_MAP - Screen output dependencies
- ✅ BMS_RECEIVE_MAP - Screen input dependencies
- ✅ IMS_DLI_CALL - Original CALL 'CBLTDLI'
- ✅ IMS_PSB - PSB ENTRY statements
- ✅ PERFORM_PARAGRAPH - Internal paragraph calls
- ✅ COPYBOOK - Copybook includes
- ✅ MESSAGE_USAGE - Error messages (still in Dependencies)

**Why MESSAGE_USAGE Stays:**
- Represents error handling dependencies
- Not business logic (just diagnostic output)
- Not database operations
- Fits with other dependency tracking

---

## Implementation Details

### Frontend Changes

#### File: `frontend/src/pages/FileDetailPage.tsx`

**Lines 155-223: Reorganization Logic**
```typescript
// Separate dependencies into different categories
const condition88Deps = fileDependencies.filter(dep => dep.type === 'CONDITION_88')
const dbOperationDeps = fileDependencies.filter(dep => ['IMS_DLI_OP', 'CICS_OP'].includes(dep.type))
const regularDeps = fileDependencies.filter(dep => !['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP'].includes(dep.type))

// Combine DB operations from both sources
const fileDbOps = [
  ...(analysisData.database_operations?.queries?.filter(...) || []),
  ...dbOperationDeps.map(dep => ({
    line: dep.line,
    type: dep.type === 'IMS_DLI_OP' ? 'IMS DL/I' : 'CICS',
    category: dep.type === 'IMS_DLI_OP' ? 'IMS Database Operation' : 'CICS Transaction Control',
    query: dep.signature,
    parameters: dep.parameters,
    target: dep.target,
    description: dep.description
  }))
]

// Combine business rules from both sources
const fileRules = [
  ...(analysisData.business_rules?.filter(...) || []),
  ...condition88Deps.map(dep => ({
    line: dep.line,
    type: '88-Level Condition',
    description: dep.description,
    code_snippet: dep.signature,
    condition_name: dep.target,
    value: dep.parameters && dep.parameters.length > 0 ? dep.parameters[0] : ''
  }))
]
```

**Lines 584-642: Enhanced DB Operations Tab**
- Added "Target/Operation" column
- Added "Context" column with parameter badges
- Shows all IMS_DLI_OP and CICS_OP entries
- Color-coded parameter badges

**Lines 644-697: Enhanced Business Rules Tab**
- Added "Condition Name" column
- Added "Value" column with yellow-amber badge
- Shows all CONDITION_88 entries
- Maintains existing business rules

#### File: `frontend/src/components/DependenciesTab.tsx`

**Lines 24-48: Statistics Calculation**
```typescript
// Exclude CONDITION_88, IMS_DLI_OP, and CICS_OP from counts
Object.values(dependencies).forEach(deps => {
  deps.forEach((dep: any) => {
    if (['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP'].includes(dep.type)) {
      return  // Skip special types
    }
    total++
    typeCounts[dep.type] = (typeCounts[dep.type] || 0) + 1
    targetCounts[dep.target] = (targetCounts[dep.target] || 0) + 1
  })
})
```

**Lines 55-92: Grouping Logic**
```typescript
Object.entries(filteredDependencies).forEach(([file, deps]) => {
  (deps as any[])
    .filter(dep => !['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP'].includes(dep.type)) // Filter out
    .filter(dep => filterByType === 'ALL' || dep.type === filterByType)
    .forEach(dep => {
      // Group remaining dependencies
    })
})
```

---

## Before vs After

### Before Reorganization

**Dependencies Tab:**
```
✗ Mixed purpose items
✗ Business rules (88-levels) with code dependencies
✗ Database operations with program calls
✗ Transaction control with copybook includes
✗ Hard to find related items
```

**Business Rules Tab:**
```
- Only extracted business rules
- No 88-level conditions
```

**DB Operations Tab:**
```
- Only SQL queries
- No IMS operations
- No transaction control
```

### After Reorganization

**Dependencies Tab:**
```
✓ Only code dependencies
✓ Program calls
✓ Paragraph performs
✓ Copybook includes
✓ Screen dependencies (BMS)
✓ Error messages
✓ Clean, focused view
```

**Business Rules Tab:**
```
✓ Extracted business rules
✓ 88-level conditions with values
✓ Complete business logic catalog
✓ Easy value lookup
```

**DB Operations Tab:**
```
✓ SQL queries
✓ IMS DL/I operations (GU, GNP, REPL, etc.)
✓ CICS transaction control (SYNCPOINT, LINK)
✓ Complete data access view
✓ Transaction boundaries visible
```

---

## User Benefits

### 1. Better Organization
- Each section has items of the same purpose
- No mental overhead to separate types
- Logical grouping by function

### 2. Complete Context
- DB Operations shows all data-related operations
- Business Rules shows all business logic
- Dependencies shows only code structure

### 3. Migration Planning
- DB Operations: See all IMS → SQL conversions needed
- Business Rules: Map 88-levels to enums/constants
- Dependencies: Understand code structure

### 4. Transaction Analysis
- CICS SYNCPOINT/ROLLBACK in DB Operations
- Easy to identify transaction boundaries
- COMMAREA tracking for data flow

### 5. Business Rules Extraction
- All conditions in one place
- Value lookup table
- Business logic documentation

---

## Testing Checklist

### Dependencies Tab
- [ ] CONDITION_88 **NOT** visible
- [ ] IMS_DLI_OP **NOT** visible
- [ ] CICS_OP **NOT** visible
- [ ] Only code dependencies visible
- [ ] Counts exclude special types
- [ ] Type breakdown excludes special types

### Business Rules Tab
- [ ] 88-level conditions visible
- [ ] Condition Name column populated
- [ ] Value column shows VALUE clause
- [ ] Yellow-amber badge for values
- [ ] Original business rules still present
- [ ] Count includes 88-levels

### DB Operations Tab
- [ ] IMS DL/I operations visible
- [ ] CICS operations visible
- [ ] Target/Operation column populated
- [ ] Context column shows parameters
- [ ] Parameter badges displayed
- [ ] SQL queries still present
- [ ] Count includes all operations

---

## Summary

**Moved:**
- ✅ CONDITION_88 → Business Rules
- ✅ IMS_DLI_OP → DB Operations
- ✅ CICS_OP → DB Operations

**Result:**
- Clear separation of concerns
- Each section has related items
- Better user experience
- More actionable insights

**Files Modified:**
- `frontend/src/pages/FileDetailPage.tsx` - Reorganization logic, enhanced tabs
- `frontend/src/components/DependenciesTab.tsx` - Filtered out special types

**Lines of Code:** ~150 lines modified/added

**Impact:** Professional-grade mainframe analysis with logical organization! 🎉
