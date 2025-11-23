import React, { useState } from 'react'
import { useParams, Link } from 'react-router-dom'
import { Download } from 'lucide-react'
import { AnalysisResults } from '../services/api'
import { getDependencyTypeStyle, getDependencyTypeLabel } from '../utils/dependencyColors'
import { exportToCSV } from '../utils/csvExport'
import AIAnalysisButton from './ai/AIAnalysisButton'

interface Props {
  data: AnalysisResults
  uploadId: string
}

const DependenciesTab: React.FC<Props> = ({ data, uploadId }) => {
  const [selectedFile, setSelectedFile] = useState<string | null>(null)
  const [filterByType, setFilterByType] = useState<string>('ALL')

  // Use detailed dependencies if available, otherwise fall back to simple
  const hasDetailedDeps = data.detailed_dependencies && Object.keys(data.detailed_dependencies).length > 0
  const dependencies = hasDetailedDeps ? data.detailed_dependencies : data.dependencies

  // For simple dependencies, count total
  const totalSimpleDeps = !hasDetailedDeps
    ? Object.values(data.dependencies || {}).reduce((acc, deps) => acc + deps.length, 0)
    : 0

  // For detailed dependencies, count total and group by type
  // Exclude CONDITION_88, IMS_DLI_OP, CICS_OP, and PERFORM_PARAGRAPH
  const detailedStats = hasDetailedDeps ? (() => {
    const typeCounts: Record<string, number> = {}
    const targetCounts: Record<string, number> = {}
    let total = 0

    Object.values(dependencies).forEach(deps => {
      deps.forEach((dep: any) => {
        // Skip special types that belong in other sections or internal flow
        if (['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP', 'PERFORM_PARAGRAPH'].includes(dep.type)) {
          return
        }
        total++
        typeCounts[dep.type] = (typeCounts[dep.type] || 0) + 1
        targetCounts[dep.target] = (targetCounts[dep.target] || 0) + 1
      })
    })

    const topTargets = Object.entries(targetCounts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 20)

    return { typeCounts, targetCounts, total, topTargets }
  })() : null

  // Get all dependency types for filter (excluding special types)
  const allTypes = hasDetailedDeps && detailedStats
    ? ['ALL', ...Object.keys(detailedStats.typeCounts).sort()]
    : []

  // Filter dependencies by selected file
  const filteredDependencies = selectedFile
    ? { [selectedFile]: dependencies[selectedFile] }
    : dependencies

  // Group dependencies by file, type, and target to consolidate duplicate entries
  // Filter out CONDITION_88, IMS_DLI_OP, CICS_OP, and PERFORM_PARAGRAPH
  const groupedDependencies = hasDetailedDeps ? (() => {
    const grouped: Record<string, any[]> = {}

    Object.entries(filteredDependencies).forEach(([file, deps]) => {
      (deps as any[])
        .filter(dep => !['CONDITION_88', 'IMS_DLI_OP', 'CICS_OP', 'PERFORM_PARAGRAPH'].includes(dep.type)) // Filter out special types
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

  const handleExportDependencies = () => {
    if (groupedDependencies.length === 0) {
      alert('No dependencies to export')
      return
    }

    const data = groupedDependencies.map(dep => ({
      File: dep.file,
      Type: getDependencyTypeLabel(dep.type),
      Target: dep.target,
      Count: dep.count,
      Lines: dep.lines.join('; '),
      Signature: dep.signature,
      Parameters: dep.parameters?.join('; ') || '',
      Description: dep.description
    }))

    const filename = `all_dependencies.csv`
    exportToCSV(data, filename)
  }

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Dependencies Analysis</h2>

      {/* Summary Statistics */}
      <div className="grid">
        <div className="stat-card">
          <h3>Files with Dependencies</h3>
          <div className="value">{Object.keys(dependencies).length}</div>
        </div>
        <div className="stat-card">
          <h3>Total Dependencies</h3>
          <div className="value">
            {hasDetailedDeps && detailedStats ? detailedStats.total : totalSimpleDeps}
          </div>
        </div>
        <div className="stat-card">
          <h3>Unique Targets</h3>
          <div className="value">
            {hasDetailedDeps && detailedStats
              ? Object.keys(detailedStats.targetCounts).length
              : Object.keys(data.dependencies || {}).reduce((acc, file) => {
                  data.dependencies[file].forEach((dep: string) => acc.add(dep))
                  return acc
                }, new Set()).size
            }
          </div>
        </div>
        {hasDetailedDeps && detailedStats && (
          <div className="stat-card">
            <h3>Dependency Types</h3>
            <div className="value">{Object.keys(detailedStats.typeCounts).length}</div>
          </div>
        )}
      </div>

      {/* Dependency Type Breakdown (if detailed dependencies available) */}
      {hasDetailedDeps && detailedStats && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Dependency Types Breakdown</h3>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: '10px', marginBottom: '20px' }}>
            {Object.entries(detailedStats.typeCounts).map(([type, count]) => (
              <div
                key={type}
                style={{
                  padding: '10px 15px',
                  borderRadius: '8px',
                  ...getDependencyTypeStyle(type),
                  fontWeight: 'bold',
                  fontSize: '0.9em'
                }}
              >
                {getDependencyTypeLabel(type)}: {count}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Most Common Targets */}
      {hasDetailedDeps && detailedStats && detailedStats.topTargets.length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Most Referenced Targets</h3>
          <table>
            <thead>
              <tr>
                <th>Target</th>
                <th>Reference Count</th>
              </tr>
            </thead>
            <tbody>
              {detailedStats.topTargets.map(([target, count], index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace' }}>{target}</td>
                  <td>{count}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {/* Filter Controls */}
      <div style={{ marginTop: '30px', marginBottom: '20px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
          <h3 style={{ margin: 0 }}>All Dependencies</h3>
          <div style={{ display: 'flex', gap: '10px', alignItems: 'center' }}>
            {hasDetailedDeps && groupedDependencies.length > 0 && (
              <button
                className="button button-primary"
                onClick={handleExportDependencies}
                style={{ padding: '8px 15px', display: 'flex', alignItems: 'center', gap: '8px' }}
              >
                <Download size={16} /> Export CSV
              </button>
            )}
            {hasDetailedDeps && (
              <>
                <label style={{ fontSize: '0.9em', fontWeight: 'bold' }}>Filter by Type:</label>
                <select
                  value={filterByType}
                  onChange={(e) => setFilterByType(e.target.value)}
                  style={{
                    padding: '5px 10px',
                    borderRadius: '4px',
                    border: '1px solid #ddd',
                    fontSize: '0.9em'
                  }}
                >
                  {allTypes.map(type => (
                    <option key={type} value={type}>{type.replace(/_/g, ' ')}</option>
                  ))}
                </select>
              </>
            )}
            {selectedFile && (
              <button
                className="button button-secondary"
                onClick={() => setSelectedFile(null)}
                style={{ padding: '5px 10px', fontSize: '0.9em' }}
              >
                Clear File Filter
              </button>
            )}
          </div>
        </div>
      </div>

      {/* Dependencies List */}
      <div style={{ overflowX: 'auto' }}>
        {hasDetailedDeps ? (
          // Detailed dependencies table
          <table>
            <thead>
              <tr>
                <th>File</th>
                <th>Type</th>
                <th>Target</th>
                <th>Count</th>
                <th>Lines</th>
                <th>Signature</th>
                <th>Parameters</th>
                <th>Description</th>
                <th style={{ textAlign: 'center' }}>AI Analysis</th>
              </tr>
            </thead>
            <tbody>
              {groupedDependencies.map((dep, idx) => (
                <tr key={idx}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.85em' }}>
                    <Link
                      to={`/analysis/${uploadId}/file/${encodeURIComponent(dep.file)}`}
                      style={{ color: '#667eea', textDecoration: 'none' }}
                      onMouseEnter={(e) => e.currentTarget.style.textDecoration = 'underline'}
                      onMouseLeave={(e) => e.currentTarget.style.textDecoration = 'none'}
                    >
                      {dep.file}
                    </Link>
                  </td>
                  <td>
                    <span style={{
                      padding: '3px 8px',
                      borderRadius: '4px',
                      fontSize: '0.75em',
                      fontWeight: 'bold',
                      whiteSpace: 'nowrap',
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
                    fontSize: '0.8em',
                    maxWidth: '250px',
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    whiteSpace: 'nowrap'
                  }}>
                    {dep.signature}
                  </td>
                  <td>
                    {dep.parameters && dep.parameters.length > 0 ? (
                      <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px' }}>
                        {dep.parameters.map((param: string, pidx: number) => (
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
                  <td style={{ fontSize: '0.85em', color: '#666' }}>{dep.description}</td>
                  <td style={{ textAlign: 'center' }}>
                    <AIAnalysisButton
                      uploadId={uploadId}
                      itemType="dependency"
                      itemData={dep}
                      variant="button"
                    />
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        ) : (
          // Simple dependencies table (fallback)
          <table>
            <thead>
              <tr>
                <th>File</th>
                <th>Dependencies</th>
              </tr>
            </thead>
            <tbody>
              {Object.entries(filteredDependencies).map(([file, deps], index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>
                    <Link
                      to={`/analysis/${uploadId}/file/${encodeURIComponent(file)}`}
                      style={{ color: '#667eea', textDecoration: 'none' }}
                      onMouseEnter={(e) => e.currentTarget.style.textDecoration = 'underline'}
                      onMouseLeave={(e) => e.currentTarget.style.textDecoration = 'none'}
                    >
                      {file}
                    </Link>
                  </td>
                  <td>
                    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '5px' }}>
                      {(deps as string[]).map((dep, i) => (
                        <span key={i} className="badge badge-primary" style={{ fontSize: '0.8em' }}>
                          {dep}
                        </span>
                      ))}
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </div>
  )
}

export default DependenciesTab
