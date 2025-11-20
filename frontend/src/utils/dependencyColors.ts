/**
 * Utility functions for dependency type styling
 */

export interface DependencyTypeStyle {
  backgroundColor: string
  color: string
}

export const getDependencyTypeStyle = (type: string): DependencyTypeStyle => {
  switch (type) {
    // Program calls
    case 'PROGRAM_CALL':
      return { backgroundColor: '#e3f2fd', color: '#1565c0' }

    // CICS Program Control
    case 'CICS_XCTL':
      return { backgroundColor: '#fce4ec', color: '#c2185b' }
    case 'CICS_LINK':
      return { backgroundColor: '#f3e5f5', color: '#7b1fa2' }

    // CICS Operations (New category)
    case 'CICS_OP':
      return { backgroundColor: '#e1f5fe', color: '#01579b' }

    // CICS Transactions
    case 'CICS_RETURN_TRANSID':
      return { backgroundColor: '#fff9c4', color: '#f57f17' }
    case 'CICS_RETURN':
      return { backgroundColor: '#fff59d', color: '#f9a825' }
    case 'CICS_START_TRANSID':
      return { backgroundColor: '#fff3e0', color: '#e65100' }
    case 'CICS_SEND':
      return { backgroundColor: '#e1f5fe', color: '#0277bd' }
    case 'CICS_RECEIVE':
      return { backgroundColor: '#e0f7fa', color: '#00838f' }

    // BMS Maps (Screen UI)
    case 'BMS_SEND_MAP':
      return { backgroundColor: '#e0f2f1', color: '#00695c' }
    case 'BMS_RECEIVE_MAP':
      return { backgroundColor: '#e8f5e9', color: '#2e7d32' }

    // IMS DL/I Operations (New category)
    case 'IMS_DLI_OP':
      return { backgroundColor: '#f3e5f5', color: '#4a148c' }
    case 'IMS_DLI_CALL':
      return { backgroundColor: '#ede7f6', color: '#4527a0' }
    case 'IMS_PSB':
      return { backgroundColor: '#f3e5f5', color: '#6a1b9a' }

    // COBOL Data Definitions (New categories)
    case 'CONDITION_88':
      return { backgroundColor: '#fff8e1', color: '#f57f00' }
    case 'MESSAGE_USAGE':
      return { backgroundColor: '#ffebee', color: '#c62828' }

    // Internal calls
    case 'PERFORM_PARAGRAPH':
      return { backgroundColor: '#f3e5f5', color: '#6a1b9a' }
    case 'COPYBOOK':
      return { backgroundColor: '#e8f5e9', color: '#2e7d32' }

    // Modern language dependencies
    case 'IMPORT':
      return { backgroundColor: '#fff3e0', color: '#e65100' }
    case 'FROM_IMPORT':
      return { backgroundColor: '#ffe0b2', color: '#ef6c00' }
    case 'REQUIRE':
      return { backgroundColor: '#fff8e1', color: '#f57c00' }
    case 'METHOD_CALL':
      return { backgroundColor: '#fce4ec', color: '#c2185b' }
    case 'FUNCTION_CALL':
      return { backgroundColor: '#e0f2f1', color: '#00695c' }

    // Unknown/default
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
  if (['CICS_OP'].includes(type)) {
    return 'CICS Operations'
  }
  if (['CICS_RETURN_TRANSID', 'CICS_RETURN', 'CICS_START_TRANSID', 'CICS_SEND', 'CICS_RECEIVE'].includes(type)) {
    return 'CICS Transactions'
  }
  if (['BMS_SEND_MAP', 'BMS_RECEIVE_MAP'].includes(type)) {
    return 'Screen/BMS'
  }
  if (['IMS_DLI_OP'].includes(type)) {
    return 'IMS DL/I Operations'
  }
  if (['IMS_DLI_CALL', 'IMS_PSB'].includes(type)) {
    return 'IMS Database'
  }
  if (['CONDITION_88'].includes(type)) {
    return 'Data Definitions'
  }
  if (['MESSAGE_USAGE'].includes(type)) {
    return 'Error Handling'
  }
  if (['PERFORM_PARAGRAPH'].includes(type)) {
    return 'Internal Flow'
  }
  if (['COPYBOOK'].includes(type)) {
    return 'Code Reuse'
  }
  if (['IMPORT', 'FROM_IMPORT', 'REQUIRE'].includes(type)) {
    return 'Module Import'
  }
  if (['METHOD_CALL', 'FUNCTION_CALL'].includes(type)) {
    return 'Function Call'
  }
  return 'Other'
}
