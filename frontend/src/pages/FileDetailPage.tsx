import React, { useState, useEffect } from 'react'
import { useParams, useNavigate, useLocation } from 'react-router-dom'
import { ArrowLeft, FileCode, Database, GitBranch, AlertCircle, Info, Copy, Download } from 'lucide-react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism'
import api, { AnalysisResults } from '../services/api'
import { getDependencyTypeStyle, getDependencyTypeLabel } from '../utils/dependencyColors'
import { exportToCSV, copyToClipboard } from '../utils/csvExport'

interface FileContent {
  path: string
  content: string
  lines: number
  size: number
}

const FileDetailPage: React.FC = () => {
  const { uploadId } = useParams<{ uploadId: string }>()
  const location = useLocation()
  const navigate = useNavigate()

  // Extract file path from the wildcard route
  const filePath = decodeURIComponent(location.pathname.split('/file/')[1] || '')

  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [fileContent, setFileContent] = useState<FileContent | null>(null)
  const [analysisData, setAnalysisData] = useState<AnalysisResults | null>(null)
  const [activeSection, setActiveSection] = useState<'source' | 'metrics' | 'deps' | 'db' | 'rules'>('metrics')
  const [showComplexityInfo, setShowComplexityInfo] = useState(false)
  const [copySuccess, setCopySuccess] = useState<string | null>(null)

  useEffect(() => {
    const loadFileData = async () => {
      console.log('=== FILE DETAIL PAGE DEBUG ===')
      console.log('uploadId:', uploadId)
      console.log('filePath:', filePath)
      console.log('location.pathname:', location.pathname)

      if (!uploadId || !filePath) {
        console.log('Missing uploadId or filePath, aborting')
        return
      }

      try {
        setLoading(true)

        // Encode the file path properly for the URL
        const encodedFilePath = filePath.split('/').map(part => encodeURIComponent(part)).join('/')
        const fileUrl = `http://localhost:8000/api/analysis/${uploadId}/file/${encodedFilePath}`
        console.log('Fetching file from:', fileUrl)

        // Load file content and full analysis in parallel
        const [contentResponse, analysis] = await Promise.all([
          fetch(fileUrl),
          api.analyzeFullCodebase(uploadId)
        ])

        console.log('File response status:', contentResponse.status)

        if (!contentResponse.ok) {
          throw new Error(`Failed to fetch file: ${contentResponse.status} ${contentResponse.statusText}`)
        }

        const content = await contentResponse.json()
        console.log('File content loaded:', content.path)
        console.log('Analysis data loaded:', analysis)

        setFileContent(content)
        setAnalysisData(analysis)
        setError(null)
      } catch (err: any) {
        console.error('Error loading file data:', err)
        setError(err.message || 'Failed to load file data')
      } finally {
        setLoading(false)
      }
    }

    loadFileData()
  }, [uploadId, filePath, location.pathname])

  if (loading) {
    return (
      <div className="container">
        <div style={{ padding: '60px', textAlign: 'center' }}>
          <p>Loading file details...</p>
        </div>
      </div>
    )
  }

  if (error || !fileContent || !analysisData) {
    return (
      <div className="container">
        <div className="error">
          <h3>Error Loading File</h3>
          <p>{error || 'Failed to load file data'}</p>
          <div style={{ marginTop: '10px', fontSize: '0.9em', color: '#666' }}>
            <p>Upload ID: {uploadId}</p>
            <p>File Path: {filePath}</p>
            <p>Has File Content: {fileContent ? 'Yes' : 'No'}</p>
            <p>Has Analysis Data: {analysisData ? 'Yes' : 'No'}</p>
          </div>
        </div>
        <button className="button" onClick={() => navigate(-1)}>
          <ArrowLeft size={16} /> Go Back
        </button>
      </div>
    )
  }

  // Extract data for this specific file (safely)
  // Normalize paths for comparison (handle both forward and back slashes)
  const normalizePath = (path: string) => path.replace(/\\/g, '/').toLowerCase()
  const normalizedFilePath = normalizePath(filePath)

  console.log('Looking for file:', filePath)
  console.log('Normalized path:', normalizedFilePath)

  const fileMetrics = analysisData.metrics?.by_file?.find(f =>
    normalizePath(f.file) === normalizedFilePath
  )

  // Use detailed dependencies if available, otherwise fallback to simple dependencies
  const fileDetailedDependencies = analysisData.detailed_dependencies
    ? Object.entries(analysisData.detailed_dependencies)
        .filter(([file]) => {
          const normalized = normalizePath(file)
          return normalized === normalizedFilePath
        })
        .flatMap(([, deps]) => deps)
    : []

  // Fallback to simple dependencies if detailed not available
  const fileDependencies = fileDetailedDependencies.length > 0
    ? fileDetailedDependencies
    : (analysisData.dependencies
        ? Object.entries(analysisData.dependencies)
            .filter(([file]) => {
              const normalized = normalizePath(file)
              console.log('Checking dependency key:', file, '-> normalized:', normalized)
              return normalized === normalizedFilePath
            })
            .flatMap(([, deps]) => deps.map(dep => ({
              target: dep,
              type: 'UNKNOWN',
              line: 0,
              signature: dep,
              parameters: [],
              description: `Dependency: ${dep}`
            })))
        : [])

  console.log('Found dependencies:', fileDependencies)

  // Separate dependencies into different categories
  const condition88Deps = fileDependencies.filter(dep => dep.type === 'CONDITION_88')
  const dbOperationDeps = fileDependencies.filter(dep => ['IMS_DLI_OP', 'CICS_OP'].includes(dep.type))
  const regularDeps = fileDependencies.filter(dep => !['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP', 'PERFORM_PARAGRAPH'].includes(dep.type))

  // Group regular dependencies by type and target
  const groupedFileDependencies = (() => {
    const grouped: Record<string, any[]> = {}

    regularDeps.forEach(dep => {
      // Create a unique key for type + target
      const key = `${dep.type}|||${dep.target}`

      if (!grouped[key]) {
        grouped[key] = []
      }
      grouped[key].push(dep)
    })

    // Convert grouped dependencies to consolidated format
    return Object.entries(grouped).map(([key, deps]) => {
      const [type, target] = key.split('|||')
      const lines = deps.map(d => d.line).sort((a, b) => a - b)
      const firstDep = deps[0]

      return {
        type,
        target,
        lines, // Array of all line numbers
        count: deps.length, // How many times this dependency appears
        signature: firstDep.signature,
        parameters: firstDep.parameters,
        description: firstDep.description
      }
    })
  })()

  // Combine DB operations from both sources
  const fileDbOps = [
    ...(analysisData.database_operations?.queries?.filter(q =>
      normalizePath(q.file) === normalizedFilePath
    ) || []),
    // Add IMS_DLI_OP and CICS_OP as DB operations
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
    ...(analysisData.business_rules?.filter(r =>
      normalizePath(r.file) === normalizedFilePath
    ) || []),
    // Add CONDITION_88 as business rules
    ...condition88Deps.map(dep => ({
      line: dep.line,
      type: '88-Level Condition',
      description: dep.description,
      code_snippet: dep.signature,
      condition_name: dep.target,
      value: dep.parameters && dep.parameters.length > 0 ? dep.parameters[0] : ''
    }))
  ]

  // Get file extension for syntax highlighting
  const getLanguage = (path: string) => {
    const ext = path.split('.').pop()?.toLowerCase()
    const langMap: Record<string, string> = {
      'cbl': 'cobol',
      'cob': 'cobol',
      'cobol': 'cobol',
      'cpy': 'cobol',
      'jcl': 'jcl',
      'sql': 'sql',
      'js': 'javascript',
      'ts': 'typescript',
      'py': 'python',
      'java': 'java',
      'c': 'c',
      'cpp': 'cpp',
      'cs': 'csharp'
    }
    return langMap[ext || ''] || 'text'
  }

  const handleCopyCode = async () => {
    if (fileContent) {
      const success = await copyToClipboard(fileContent.content)
      if (success) {
        setCopySuccess('Code copied to clipboard!')
        setTimeout(() => setCopySuccess(null), 3000)
      } else {
        setCopySuccess('Failed to copy code')
        setTimeout(() => setCopySuccess(null), 3000)
      }
    }
  }

  const handleExportDependencies = () => {
    if (groupedFileDependencies.length === 0) {
      alert('No dependencies to export')
      return
    }

    const data = groupedFileDependencies.map(dep => ({
      Type: getDependencyTypeLabel(dep.type),
      Target: dep.target,
      Count: dep.count,
      Lines: dep.lines.join('; '),
      Signature: dep.signature,
      Parameters: dep.parameters?.join('; ') || '',
      Description: dep.description
    }))

    const filename = `${filePath.split(/[/\\]/).pop()}_dependencies.csv`
    exportToCSV(data, filename)
  }

  const handleExportDBOperations = () => {
    if (fileDbOps.length === 0) {
      alert('No database operations to export')
      return
    }

    const data = fileDbOps.map(op => ({
      Line: op.line,
      Type: op.type,
      Category: op.category,
      Target: op.target || '',
      Context: op.parameters?.join('; ') || '',
      Query: op.query
    }))

    const filename = `${filePath.split(/[/\\]/).pop()}_db_operations.csv`
    exportToCSV(data, filename)
  }

  const handleExportBusinessRules = () => {
    if (fileRules.length === 0) {
      alert('No business rules to export')
      return
    }

    const data = fileRules.map(rule => ({
      Line: rule.line,
      Type: rule.type,
      'Condition Name': rule.condition_name || '',
      Value: rule.value || '',
      Description: rule.description,
      'Code Snippet': rule.code_snippet
    }))

    const filename = `${filePath.split(/[/\\]/).pop()}_business_rules.csv`
    exportToCSV(data, filename)
  }

  return (
    <div className="container">
      {/* Header */}
      <div className="header">
        <div style={{ display: 'flex', alignItems: 'center', gap: '15px' }}>
          <button
            className="button button-secondary"
            onClick={() => navigate(`/analysis/${uploadId}`)}
            style={{ padding: '8px 15px' }}
          >
            <ArrowLeft size={16} /> Back to Analysis
          </button>
          <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
            <FileCode size={24} />
            <h1 style={{ margin: 0 }}>{filePath.split(/[/\\]/).pop()}</h1>
          </div>
        </div>
      </div>

      {/* File Metadata */}
      <div className="card">
        <h3 style={{ marginBottom: '15px' }}>File Information</h3>
        <div className="grid">
          <div className="stat-card">
            <h3>Lines of Code</h3>
            <div className="value">{fileMetrics?.loc || fileContent.lines}</div>
          </div>
          <div className="stat-card">
            <h3>Source Lines</h3>
            <div className="value">{fileMetrics?.sloc || '-'}</div>
          </div>
          <div className="stat-card">
            <h3>Comments</h3>
            <div className="value">{fileMetrics?.comments || '-'}</div>
          </div>
          <div className="stat-card">
            <h3>Blank Lines</h3>
            <div className="value">{fileMetrics?.blank || '-'}</div>
          </div>
          <div className="stat-card" style={{ position: 'relative' }}>
            <h3 style={{ display: 'flex', alignItems: 'center', gap: '5px' }}>
              Complexity
              <Info
                size={16}
                style={{ cursor: 'help', color: '#667eea' }}
                onClick={() => setShowComplexityInfo(!showComplexityInfo)}
              />
            </h3>
            <div className="value" style={{
              color: fileMetrics?.complexity ?
                (fileMetrics.complexity <= 10 ? '#155724' :
                 fileMetrics.complexity <= 20 ? '#856404' :
                 fileMetrics.complexity <= 50 ? '#d63031' : '#721c24') : 'inherit'
            }}>
              {fileMetrics?.complexity || '-'}
            </div>
            {fileMetrics?.complexity && (
              <div style={{
                marginTop: '8px',
                fontSize: '0.8em',
                padding: '4px 8px',
                borderRadius: '4px',
                display: 'inline-block',
                backgroundColor:
                  fileMetrics.complexity <= 10 ? '#d4edda' :
                  fileMetrics.complexity <= 20 ? '#fff3cd' :
                  fileMetrics.complexity <= 50 ? '#ffeaa7' : '#f8d7da',
                color:
                  fileMetrics.complexity <= 10 ? '#155724' :
                  fileMetrics.complexity <= 20 ? '#856404' :
                  fileMetrics.complexity <= 50 ? '#d63031' : '#721c24'
              }}>
                {fileMetrics.complexity <= 10 ? 'Simple' :
                 fileMetrics.complexity <= 20 ? 'Moderate' :
                 fileMetrics.complexity <= 50 ? 'Complex' : 'Very Complex'}
              </div>
            )}
            {showComplexityInfo && (
              <div style={{
                position: 'absolute',
                top: '100%',
                left: 0,
                marginTop: '10px',
                padding: '15px',
                backgroundColor: '#fff',
                border: '2px solid #667eea',
                borderRadius: '8px',
                boxShadow: '0 4px 12px rgba(0,0,0,0.15)',
                zIndex: 1000,
                width: '350px',
                textAlign: 'left',
                fontSize: '0.85em'
              }}>
                <h4 style={{ marginTop: 0, marginBottom: '10px', color: '#667eea' }}>Cyclomatic Complexity</h4>
                <p style={{ marginBottom: '10px', lineHeight: '1.5' }}>
                  Measures the number of independent paths through the code.
                </p>
                <p style={{ marginBottom: '10px', lineHeight: '1.5' }}>
                  <strong>Formula:</strong> 1 (base) + number of decision points
                </p>
                <div style={{ marginBottom: '10px' }}>
                  <strong>Decision points include:</strong>
                  <ul style={{ marginTop: '5px', marginBottom: '0', paddingLeft: '20px' }}>
                    <li>if, else if statements</li>
                    <li>for, while, do-while loops</li>
                    <li>case statements</li>
                    <li>&&, || logical operators</li>
                    <li>? ternary operators</li>
                    <li>catch, except blocks</li>
                  </ul>
                </div>
                <div style={{ marginBottom: '10px' }}>
                  <strong>Ratings:</strong>
                  <div style={{ marginTop: '5px' }}>
                    <span style={{ padding: '2px 6px', backgroundColor: '#d4edda', color: '#155724', borderRadius: '3px', marginRight: '5px' }}>1-10</span> Simple
                  </div>
                  <div style={{ marginTop: '3px' }}>
                    <span style={{ padding: '2px 6px', backgroundColor: '#fff3cd', color: '#856404', borderRadius: '3px', marginRight: '5px' }}>11-20</span> Moderate
                  </div>
                  <div style={{ marginTop: '3px' }}>
                    <span style={{ padding: '2px 6px', backgroundColor: '#ffeaa7', color: '#d63031', borderRadius: '3px', marginRight: '5px' }}>21-50</span> Complex
                  </div>
                  <div style={{ marginTop: '3px' }}>
                    <span style={{ padding: '2px 6px', backgroundColor: '#f8d7da', color: '#721c24', borderRadius: '3px', marginRight: '5px' }}>50+</span> Very Complex
                  </div>
                </div>
                <button
                  onClick={() => setShowComplexityInfo(false)}
                  style={{
                    marginTop: '10px',
                    padding: '5px 15px',
                    backgroundColor: '#667eea',
                    color: '#fff',
                    border: 'none',
                    borderRadius: '4px',
                    cursor: 'pointer'
                  }}
                >
                  Close
                </button>
              </div>
            )}
          </div>
          <div className="stat-card">
            <h3>File Size</h3>
            <div className="value">{(fileContent.size / 1024).toFixed(2)} KB</div>
          </div>
        </div>
      </div>

      {/* Tabbed Content */}
      <div className="card">
        <div className="tabs">
          <button
            className={`tab ${activeSection === 'metrics' ? 'active' : ''}`}
            onClick={() => setActiveSection('metrics')}
          >
            Overview
          </button>
          <button
            className={`tab ${activeSection === 'source' ? 'active' : ''}`}
            onClick={() => setActiveSection('source')}
          >
            Source Code
          </button>
          <button
            className={`tab ${activeSection === 'deps' ? 'active' : ''}`}
            onClick={() => setActiveSection('deps')}
          >
            <GitBranch size={16} /> Dependencies ({groupedFileDependencies.length})
          </button>
          <button
            className={`tab ${activeSection === 'db' ? 'active' : ''}`}
            onClick={() => setActiveSection('db')}
          >
            <Database size={16} /> DB Operations ({fileDbOps.length})
          </button>
          <button
            className={`tab ${activeSection === 'rules' ? 'active' : ''}`}
            onClick={() => setActiveSection('rules')}
          >
            <AlertCircle size={16} /> Business Rules ({fileRules.length})
          </button>
        </div>

        {/* Overview Tab */}
        {activeSection === 'metrics' && (
          <div style={{ padding: '20px' }}>
            <h3>File Summary</h3>
            <div style={{ marginTop: '20px' }}>
              <table>
                <tbody>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>Full Path:</td>
                    <td style={{ fontFamily: 'monospace', padding: '10px' }}>{filePath}</td>
                  </tr>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>Language:</td>
                    <td style={{ padding: '10px' }}>{getLanguage(filePath).toUpperCase()}</td>
                  </tr>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>Dependencies:</td>
                    <td style={{ padding: '10px' }}>{fileDependencies.length} file(s)</td>
                  </tr>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>DB Operations:</td>
                    <td style={{ padding: '10px' }}>{fileDbOps.length} operation(s)</td>
                  </tr>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>Business Rules:</td>
                    <td style={{ padding: '10px' }}>{fileRules.length} rule(s)</td>
                  </tr>
                  <tr>
                    <td style={{ fontWeight: 'bold', padding: '10px' }}>Functions:</td>
                    <td style={{ padding: '10px' }}>{fileMetrics?.functions || '0'}</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* Source Code Tab */}
        {activeSection === 'source' && (
          <div style={{ padding: '20px' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
              <h3 style={{ margin: 0 }}>Source Code</h3>
              <div style={{ display: 'flex', gap: '10px', alignItems: 'center' }}>
                {copySuccess && (
                  <span style={{ fontSize: '0.9em', color: copySuccess.includes('Failed') ? '#d63031' : '#00b894', fontWeight: 'bold' }}>
                    {copySuccess}
                  </span>
                )}
                <button
                  className="button button-primary"
                  onClick={handleCopyCode}
                  style={{ padding: '8px 15px', display: 'flex', alignItems: 'center', gap: '8px' }}
                >
                  <Copy size={16} /> Copy Code
                </button>
              </div>
            </div>
            <div style={{ border: '1px solid #ddd', borderRadius: '5px', overflow: 'auto', maxHeight: '600px' }}>
              <SyntaxHighlighter
                language={getLanguage(filePath)}
                style={vscDarkPlus}
                showLineNumbers
                wrapLines
                customStyle={{ margin: 0, fontSize: '0.9em' }}
              >
                {fileContent.content}
              </SyntaxHighlighter>
            </div>
          </div>
        )}

        {/* Dependencies Tab */}
        {activeSection === 'deps' && (
          <div style={{ padding: '20px' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
              <h3 style={{ margin: 0 }}>Dependencies ({groupedFileDependencies.length} unique, {fileDependencies.length} total)</h3>
              {groupedFileDependencies.length > 0 && (
                <button
                  className="button button-primary"
                  onClick={handleExportDependencies}
                  style={{ padding: '8px 15px', display: 'flex', alignItems: 'center', gap: '8px' }}
                >
                  <Download size={16} /> Export CSV
                </button>
              )}
            </div>
            {groupedFileDependencies.length > 0 ? (
              <div style={{ overflowX: 'auto' }}>
                <table>
                  <thead>
                    <tr>
                      <th>Type</th>
                      <th>Target</th>
                      <th>Count</th>
                      <th>Lines</th>
                      <th>Signature</th>
                      <th>Parameters</th>
                      <th>Description</th>
                    </tr>
                  </thead>
                  <tbody>
                    {groupedFileDependencies.map((dep, idx) => (
                      <tr key={idx}>
                        <td>
                          <span style={{
                            padding: '3px 8px',
                            borderRadius: '4px',
                            fontSize: '0.85em',
                            fontWeight: 'bold',
                            ...getDependencyTypeStyle(dep.type)
                          }}>
                            {getDependencyTypeLabel(dep.type)}
                          </span>
                        </td>
                        <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>{dep.target}</td>
                        <td style={{ textAlign: 'center' }}>
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
                        </td>
                        <td style={{ fontFamily: 'monospace', fontSize: '0.8em' }}>
                          {dep.lines.length <= 5 ? (
                            // Show all lines if 5 or fewer
                            dep.lines.join(', ')
                          ) : (
                            // Show first 3, ..., last 2 if more than 5
                            `${dep.lines.slice(0, 3).join(', ')}, ... ${dep.lines.slice(-2).join(', ')}`
                          )}
                        </td>
                        <td style={{
                          fontFamily: 'monospace',
                          fontSize: '0.85em',
                          maxWidth: '300px',
                          overflow: 'hidden',
                          textOverflow: 'ellipsis',
                          whiteSpace: 'nowrap'
                        }}>
                          {dep.signature}
                        </td>
                        <td>
                          {dep.parameters && dep.parameters.length > 0 ? (
                            <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px' }}>
                              {dep.parameters.map((param, pidx) => (
                                <span
                                  key={pidx}
                                  style={{
                                    padding: '2px 6px',
                                    backgroundColor: '#f0f0f0',
                                    borderRadius: '3px',
                                    fontSize: '0.8em',
                                    fontFamily: 'monospace'
                                  }}
                                >
                                  {param}
                                </span>
                              ))}
                            </div>
                          ) : (
                            <span style={{ color: '#999' }}>-</span>
                          )}
                        </td>
                        <td style={{ fontSize: '0.9em', color: '#666' }}>{dep.description}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <p>No dependencies found</p>
            )}
          </div>
        )}

        {/* DB Operations Tab */}
        {activeSection === 'db' && (
          <div style={{ padding: '20px' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
              <h3 style={{ margin: 0 }}>Database Operations ({fileDbOps.length})</h3>
              {fileDbOps.length > 0 && (
                <button
                  className="button button-primary"
                  onClick={handleExportDBOperations}
                  style={{ padding: '8px 15px', display: 'flex', alignItems: 'center', gap: '8px' }}
                >
                  <Download size={16} /> Export CSV
                </button>
              )}
            </div>
            {fileDbOps.length > 0 ? (
              <div style={{ overflowX: 'auto' }}>
                <table>
                  <thead>
                    <tr>
                      <th>Line</th>
                      <th>Type</th>
                      <th>Category</th>
                      <th>Target/Operation</th>
                      <th>Context</th>
                      <th>Query/Statement</th>
                    </tr>
                  </thead>
                  <tbody>
                    {fileDbOps.map((op, idx) => (
                      <tr key={idx}>
                        <td>{op.line}</td>
                        <td><span style={{ padding: '2px 6px', backgroundColor: '#667eea', color: '#fff', borderRadius: '3px', fontSize: '0.85em' }}>{op.type}</span></td>
                        <td>{op.category}</td>
                        <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>
                          {op.target || '-'}
                        </td>
                        <td>
                          {op.parameters && op.parameters.length > 0 ? (
                            <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px' }}>
                              {op.parameters.map((param: string, pidx: number) => (
                                <span
                                  key={pidx}
                                  style={{
                                    padding: '2px 6px',
                                    backgroundColor: '#f0f0f0',
                                    borderRadius: '3px',
                                    fontSize: '0.75em',
                                    fontFamily: 'monospace'
                                  }}
                                >
                                  {param}
                                </span>
                              ))}
                            </div>
                          ) : (
                            <span style={{ color: '#999', fontSize: '0.85em' }}>-</span>
                          )}
                        </td>
                        <td style={{ fontFamily: 'monospace', fontSize: '0.85em', maxWidth: '500px', overflow: 'hidden', textOverflow: 'ellipsis' }}>{op.query}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <p>No database operations found</p>
            )}
          </div>
        )}

        {/* Business Rules Tab */}
        {activeSection === 'rules' && (
          <div style={{ padding: '20px' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
              <h3 style={{ margin: 0 }}>Business Rules ({fileRules.length})</h3>
              {fileRules.length > 0 && (
                <button
                  className="button button-primary"
                  onClick={handleExportBusinessRules}
                  style={{ padding: '8px 15px', display: 'flex', alignItems: 'center', gap: '8px' }}
                >
                  <Download size={16} /> Export CSV
                </button>
              )}
            </div>
            {fileRules.length > 0 ? (
              <div style={{ overflowX: 'auto' }}>
                <table>
                  <thead>
                    <tr>
                      <th>Line</th>
                      <th>Type</th>
                      <th>Condition Name</th>
                      <th>Value</th>
                      <th>Description</th>
                      <th>Code Snippet</th>
                    </tr>
                  </thead>
                  <tbody>
                    {fileRules.map((rule, idx) => (
                      <tr key={idx}>
                        <td>{rule.line}</td>
                        <td><span style={{ padding: '2px 6px', backgroundColor: '#764ba2', color: '#fff', borderRadius: '3px', fontSize: '0.85em' }}>{rule.type}</span></td>
                        <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>
                          {rule.condition_name || '-'}
                        </td>
                        <td>
                          {rule.value ? (
                            <span style={{
                              padding: '2px 6px',
                              backgroundColor: '#fff8e1',
                              color: '#f57f00',
                              borderRadius: '3px',
                              fontSize: '0.85em',
                              fontFamily: 'monospace',
                              fontWeight: 'bold'
                            }}>
                              {rule.value}
                            </span>
                          ) : (
                            <span style={{ color: '#999', fontSize: '0.85em' }}>-</span>
                          )}
                        </td>
                        <td>{rule.description}</td>
                        <td style={{ fontFamily: 'monospace', fontSize: '0.85em', maxWidth: '400px', overflow: 'hidden', textOverflow: 'ellipsis' }}>{rule.code_snippet}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <p>No business rules found</p>
            )}
          </div>
        )}
      </div>
    </div>
  )
}

export default FileDetailPage
