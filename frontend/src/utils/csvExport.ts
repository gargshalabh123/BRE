/**
 * CSV Export Utility
 * Provides functions to export table data to CSV format
 */

export const exportToCSV = (data: any[], filename: string, headers?: string[]) => {
  if (data.length === 0) {
    alert('No data to export')
    return
  }

  // Get headers from first object if not provided
  const csvHeaders = headers || Object.keys(data[0])

  // Create CSV content
  const csvContent = [
    csvHeaders.join(','), // Header row
    ...data.map(row => {
      return csvHeaders.map(header => {
        const value = row[header]
        // Handle arrays and objects
        if (Array.isArray(value)) {
          return `"${value.join('; ')}"`
        }
        if (typeof value === 'object' && value !== null) {
          return `"${JSON.stringify(value)}"`
        }
        // Escape quotes and wrap in quotes if contains comma
        const stringValue = String(value || '')
        if (stringValue.includes(',') || stringValue.includes('"') || stringValue.includes('\n')) {
          return `"${stringValue.replace(/"/g, '""')}"`
        }
        return stringValue
      }).join(',')
    })
  ].join('\n')

  // Create blob and download
  const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' })
  const link = document.createElement('a')
  const url = URL.createObjectURL(blob)

  link.setAttribute('href', url)
  link.setAttribute('download', filename)
  link.style.visibility = 'hidden'

  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
}

export const copyToClipboard = async (text: string): Promise<boolean> => {
  try {
    await navigator.clipboard.writeText(text)
    return true
  } catch (err) {
    // Fallback for older browsers
    const textArea = document.createElement('textarea')
    textArea.value = text
    textArea.style.position = 'fixed'
    textArea.style.left = '-999999px'
    textArea.style.top = '-999999px'
    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()

    try {
      document.execCommand('copy')
      document.body.removeChild(textArea)
      return true
    } catch (err) {
      document.body.removeChild(textArea)
      return false
    }
  }
}
