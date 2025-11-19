import React, { useState } from 'react'
import { useParams, Link } from 'react-router-dom'
import { AnalysisResults } from '../services/api'

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
  const detailedStats = hasDetailedDeps ? (() => {
    const typeCounts: Record<string, number> = {}
    const targetCounts: Record<string, number> = {}
    let total = 0

    Object.values(dependencies).forEach(deps => {
      deps.forEach((dep: any) => {
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

  // Get all dependency types for filter
  const allTypes = hasDetailedDeps && detailedStats
    ? ['ALL', ...Object.keys(detailedStats.typeCounts).sort()]
    : []

  // Filter dependencies by selected file
  const filteredDependencies = selectedFile
    ? { [selectedFile]: dependencies[selectedFile] }
    : dependencies

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
                  backgroundColor:
                    type === 'PROGRAM_CALL' ? '#e3f2fd' :
                    type === 'PERFORM_PARAGRAPH' ? '#f3e5f5' :
                    type === 'COPYBOOK' ? '#e8f5e9' :
                    type === 'IMPORT' ? '#fff3e0' :
                    type === 'METHOD_CALL' ? '#fce4ec' :
                    type === 'FUNCTION_CALL' ? '#e0f2f1' : '#f5f5f5',
                  color:
                    type === 'PROGRAM_CALL' ? '#1565c0' :
                    type === 'PERFORM_PARAGRAPH' ? '#6a1b9a' :
                    type === 'COPYBOOK' ? '#2e7d32' :
                    type === 'IMPORT' ? '#e65100' :
                    type === 'METHOD_CALL' ? '#c2185b' :
                    type === 'FUNCTION_CALL' ? '#00695c' : '#424242',
                  fontWeight: 'bold',
                  fontSize: '0.9em'
                }}
              >
                {type.replace(/_/g, ' ')}: {count}
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
                <th>Line</th>
                <th>Signature</th>
                <th>Parameters</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              {Object.entries(filteredDependencies).flatMap(([file, deps]) =>
                (deps as any[])
                  .filter(dep => filterByType === 'ALL' || dep.type === filterByType)
                  .map((dep, idx) => (
                    <tr key={`${file}-${idx}`}>
                      <td style={{ fontFamily: 'monospace', fontSize: '0.85em' }}>
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
                        <span style={{
                          padding: '3px 8px',
                          borderRadius: '4px',
                          fontSize: '0.75em',
                          fontWeight: 'bold',
                          whiteSpace: 'nowrap',
                          backgroundColor:
                            dep.type === 'PROGRAM_CALL' ? '#e3f2fd' :
                            dep.type === 'PERFORM_PARAGRAPH' ? '#f3e5f5' :
                            dep.type === 'COPYBOOK' ? '#e8f5e9' :
                            dep.type === 'IMPORT' ? '#fff3e0' :
                            dep.type === 'METHOD_CALL' ? '#fce4ec' :
                            dep.type === 'FUNCTION_CALL' ? '#e0f2f1' : '#f5f5f5',
                          color:
                            dep.type === 'PROGRAM_CALL' ? '#1565c0' :
                            dep.type === 'PERFORM_PARAGRAPH' ? '#6a1b9a' :
                            dep.type === 'COPYBOOK' ? '#2e7d32' :
                            dep.type === 'IMPORT' ? '#e65100' :
                            dep.type === 'METHOD_CALL' ? '#c2185b' :
                            dep.type === 'FUNCTION_CALL' ? '#00695c' : '#424242'
                        }}>
                          {dep.type.replace(/_/g, ' ')}
                        </span>
                      </td>
                      <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>{dep.target}</td>
                      <td style={{ textAlign: 'center' }}>{dep.line}</td>
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
                    </tr>
                  ))
              )}
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
