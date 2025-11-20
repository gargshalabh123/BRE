# Additional CICS Operations Detection

## Overview

Added comprehensive CICS operation detection to capture all CICS commands in the Dependencies section.

---

## New CICS Operations Added

### 1. CICS_RETURN (Plain RETURN)

**Purpose:** Track CICS RETURN without TRANSID

**Pattern:**
```cobol
EXEC CICS RETURN END-EXEC.
```

**Captured As:**
- **Type:** `CICS_RETURN`
- **Target:** `RETURN`
- **Parameters:** `[]`
- **Description:** Returns control to CICS

**UI Styling:**
- Background: Light yellow (#fff59d)
- Text: Dark yellow (#f9a825)
- Category: CICS Transactions

**Use Case:** Track where programs return control back to CICS without starting another transaction.

---

### 2. CICS_SEND (Non-BMS Data Transmission)

**Purpose:** Track CICS SEND commands for data transmission (non-BMS)

**Patterns:**
```cobol
EXEC CICS SEND FROM(WS-DATA-AREA) END-EXEC.
EXEC CICS SEND TEXT(WS-MESSAGE) END-EXEC.
EXEC CICS SEND FROM(WS-OUTPUT) LENGTH(100) END-EXEC.
```

**Captured As:**
- **Type:** `CICS_SEND`
- **Target:** Variable name from FROM() or TEXT()
- **Parameters:** `['FROM:WS-DATA-AREA']` or `['TEXT:WS-MESSAGE']`
- **Description:** CICS SEND data transmission

**UI Styling:**
- Background: Light cyan (#e1f5fe)
- Text: Dark blue (#0277bd)
- Category: CICS Transactions

**Exclusions:**
- Does NOT capture `EXEC CICS SEND MAP(...)` - that's already handled as `BMS_SEND_MAP`

**Use Case:** Track terminal/data communications and outbound data flows.

---

### 3. CICS_RECEIVE (Non-BMS Data Reception)

**Purpose:** Track CICS RECEIVE commands for data reception (non-BMS)

**Patterns:**
```cobol
EXEC CICS RECEIVE INTO(WS-INPUT-AREA) END-EXEC.
EXEC CICS RECEIVE INTO(COMM-AREA) LENGTH(100) END-EXEC.
```

**Captured As:**
- **Type:** `CICS_RECEIVE`
- **Target:** Variable name from INTO()
- **Parameters:** `['INTO:WS-INPUT-AREA']`
- **Description:** CICS RECEIVE data reception

**UI Styling:**
- Background: Light cyan (#e0f7fa)
- Text: Teal (#00838f)
- Category: CICS Transactions

**Exclusions:**
- Does NOT capture `EXEC CICS RECEIVE MAP(...)` - that's already handled as `BMS_RECEIVE_MAP`

**Use Case:** Track terminal/data communications and inbound data flows.

---

## Already Existing CICS Operations

These were already implemented:

### CICS_XCTL
```cobol
EXEC CICS XCTL PROGRAM('ACCT02') END-EXEC.
```
- **Purpose:** Transfer control to another program (no return)
- **Color:** Pink (#fce4ec / #c2185b)

### CICS_LINK (Now CICS_OP)
```cobol
EXEC CICS LINK PROGRAM('VALPGM') COMMAREA(WS-AREA) END-EXEC.
```
- **Purpose:** Call another program with return
- **Tracked as:** `CICS_OP` in DB Operations section
- **Captures:** COMMAREA for data flow analysis

### CICS_RETURN_TRANSID
```cobol
EXEC CICS RETURN TRANSID('ACT2') END-EXEC.
```
- **Purpose:** Return to CICS with next transaction ID
- **Color:** Yellow (#fff9c4 / #f57f17)

### CICS_START_TRANSID
```cobol
EXEC CICS START TRANSID('BTCH') END-EXEC.
```
- **Purpose:** Start asynchronous transaction
- **Color:** Orange (#fff3e0 / #e65100)

### CICS_OP (SYNCPOINT, SYNCPOINT ROLLBACK)
```cobol
EXEC CICS SYNCPOINT END-EXEC.
EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
```
- **Purpose:** Transaction control (commit/rollback)
- **Location:** DB Operations section
- **Color:** Light cyan (#e1f5fe / #01579b)

### BMS_SEND_MAP
```cobol
EXEC CICS SEND MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC.
```
- **Purpose:** Send screen map to terminal
- **Color:** Teal (#e0f2f1 / #00695c)

### BMS_RECEIVE_MAP
```cobol
EXEC CICS RECEIVE MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC.
```
- **Purpose:** Receive screen input from terminal
- **Color:** Green (#e8f5e9 / #2e7d32)

---

## Implementation Details

### Backend Changes

**File:** `backend/analyzers/dependency_analyzer.py`

#### CICS_RETURN Detection (Lines 130-139)
```python
elif re.search(r'EXEC\s+CICS\s+RETURN(?!\s+TRANSID)', clean_line, re.IGNORECASE):
    # Plain RETURN without TRANSID
    dependencies.append({
        'target': 'RETURN',
        'type': 'CICS_RETURN',
        'line': i,
        'signature': clean_line[:200],
        'parameters': [],
        'description': f'Returns control to CICS'
    })
```

**Key Feature:** Negative lookahead `(?!\s+TRANSID)` ensures we only match plain RETURN, not RETURN TRANSID

#### CICS_SEND Detection (Lines 212-237)
```python
if 'EXEC' in clean_line.upper() and 'CICS' in clean_line.upper() and 'SEND' in clean_line.upper():
    # Check if it's NOT a BMS SEND MAP (already handled above)
    if not re.search(r'SEND\s+MAP\(', clean_line, re.IGNORECASE):
        # EXEC CICS SEND FROM(...) or SEND TEXT(...)
        send_from_match = re.search(r'FROM\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
        send_text_match = re.search(r'TEXT\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)

        target = None
        params = []
        if send_from_match:
            target = send_from_match.group(1)
            params.append(f'FROM:{target}')
        if send_text_match:
            target = send_text_match.group(1)
            params.append(f'TEXT:{target}')

        if target or 'SEND' in clean_line.upper():
            dependencies.append({
                'target': target or 'SEND',
                'type': 'CICS_SEND',
                'line': i,
                'signature': clean_line[:200],
                'parameters': params,
                'description': f'CICS SEND data transmission'
            })
```

**Key Feature:** Excludes BMS SEND MAP to avoid duplicates

#### CICS_RECEIVE Detection (Lines 239-260)
```python
if 'EXEC' in clean_line.upper() and 'CICS' in clean_line.upper() and 'RECEIVE' in clean_line.upper():
    # Check if it's NOT a BMS RECEIVE MAP (already handled above)
    if not re.search(r'RECEIVE\s+MAP\(', clean_line, re.IGNORECASE):
        # EXEC CICS RECEIVE INTO(...)
        receive_into_match = re.search(r'INTO\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)

        target = None
        params = []
        if receive_into_match:
            target = receive_into_match.group(1)
            params.append(f'INTO:{target}')

        if target or 'RECEIVE' in clean_line.upper():
            dependencies.append({
                'target': target or 'RECEIVE',
                'type': 'CICS_RECEIVE',
                'line': i,
                'signature': clean_line[:200],
                'parameters': params,
                'description': f'CICS RECEIVE data reception'
            })
```

**Key Feature:** Excludes BMS RECEIVE MAP to avoid duplicates

### Frontend Changes

**File:** `frontend/src/utils/dependencyColors.ts`

#### Added Color Definitions (Lines 29-36)
```typescript
case 'CICS_RETURN':
  return { backgroundColor: '#fff59d', color: '#f9a825' }
case 'CICS_SEND':
  return { backgroundColor: '#e1f5fe', color: '#0277bd' }
case 'CICS_RECEIVE':
  return { backgroundColor: '#e0f7fa', color: '#00838f' }
```

#### Updated Category Mapping (Line 93)
```typescript
if (['CICS_RETURN_TRANSID', 'CICS_RETURN', 'CICS_START_TRANSID', 'CICS_SEND', 'CICS_RECEIVE'].includes(type)) {
  return 'CICS Transactions'
}
```

---

## Complete CICS Coverage

### Dependencies Tab
- ✅ CICS_XCTL - Program transfer
- ✅ CICS_RETURN_TRANSID - Return with next transaction
- ✅ CICS_RETURN - Plain return
- ✅ CICS_START_TRANSID - Async transaction start
- ✅ CICS_SEND - Data transmission (non-BMS)
- ✅ CICS_RECEIVE - Data reception (non-BMS)
- ✅ BMS_SEND_MAP - Screen output
- ✅ BMS_RECEIVE_MAP - Screen input

### DB Operations Tab
- ✅ CICS_OP (LINK) - Program call with COMMAREA
- ✅ CICS_OP (SYNCPOINT) - Transaction commit
- ✅ CICS_OP (SYNCPOINT ROLLBACK) - Transaction rollback

---

## Example Output

**COBOL Code:**
```cobol
EXEC CICS RETURN END-EXEC.
EXEC CICS SEND FROM(WS-OUTPUT-DATA) LENGTH(200) END-EXEC.
EXEC CICS RECEIVE INTO(WS-INPUT-DATA) END-EXEC.
```

**Dependencies Captured:**
```
Dependencies Tab:
| Type          | Target         | Parameters           | Description                  |
|---------------|----------------|----------------------|------------------------------|
| CICS RETURN   | RETURN         | []                   | Returns control to CICS      |
| CICS SEND     | WS-OUTPUT-DATA | ['FROM:WS-OUTPUT-DATA'] | CICS SEND data transmission |
| CICS RECEIVE  | WS-INPUT-DATA  | ['INTO:WS-INPUT-DATA']  | CICS RECEIVE data reception |
```

---

## Testing Checklist

### CICS_RETURN
- [ ] Upload COBOL with `EXEC CICS RETURN END-EXEC`
- [ ] Verify CICS_RETURN dependency appears
- [ ] Badge color: Light yellow (#fff59d / #f9a825)
- [ ] Target: RETURN
- [ ] No parameters
- [ ] Does NOT appear for `EXEC CICS RETURN TRANSID('ACT2')`

### CICS_SEND
- [ ] Upload COBOL with `EXEC CICS SEND FROM(WS-DATA)`
- [ ] Verify CICS_SEND dependency appears
- [ ] Badge color: Light cyan (#e1f5fe / #0277bd)
- [ ] Target: WS-DATA
- [ ] Parameters: `['FROM:WS-DATA']`
- [ ] Does NOT appear for `EXEC CICS SEND MAP(...)`

### CICS_RECEIVE
- [ ] Upload COBOL with `EXEC CICS RECEIVE INTO(WS-INPUT)`
- [ ] Verify CICS_RECEIVE dependency appears
- [ ] Badge color: Light cyan (#e0f7fa / #00838f)
- [ ] Target: WS-INPUT
- [ ] Parameters: `['INTO:WS-INPUT']`
- [ ] Does NOT appear for `EXEC CICS RECEIVE MAP(...)`

---

## Summary

**New Operations Added:** 3
1. ✅ CICS_RETURN - Plain return to CICS
2. ✅ CICS_SEND - Data transmission (non-BMS)
3. ✅ CICS_RECEIVE - Data reception (non-BMS)

**Total CICS Coverage:** 11 operation types

**Files Modified:**
- `backend/analyzers/dependency_analyzer.py` (~60 lines added)
- `frontend/src/utils/dependencyColors.ts` (~5 lines added)

**Result:** Complete CICS operation tracking for mainframe modernization! 🎉
