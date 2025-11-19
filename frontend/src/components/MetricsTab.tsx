import React, { useState } from 'react'
import { useParams, Link } from 'react-router-dom'
import { Info } from 'lucide-react'
import { AnalysisResults } from '../services/api'

interface Props {
  data: AnalysisResults
}

const MetricsTab: React.FC<Props> = ({ data }) => {
  const { uploadId } = useParams<{ uploadId: string }>()
  const [sortBy, setSortBy] = useState<'type' | 'loc' | 'sloc' | 'complexity'>('type')
  const [sortDesc, setSortDesc] = useState(false)
  const [selectedTypes, setSelectedTypes] = useState<Set<string>>(new Set())
  const [showComplexityInfo, setShowComplexityInfo] = useState(false)

  // Helper function to extract file extension/type
  const getFileType = (filepath: string) => {
    const parts = filepath.split('.')
    return parts.length > 1 ? '.' + parts[parts.length - 1].toLowerCase() : 'no extension'
  }

  // Get all unique file types
  const allFileTypes = Array.from(new Set(data.metrics.by_file.map(f => getFileType(f.file)))).sort()

  // Filter and sort files
  const filteredAndSortedFiles = [...data.metrics.by_file]
    .filter(file => {
      if (selectedTypes.size === 0) return true // Show all if no filter selected
      return selectedTypes.has(getFileType(file.file))
    })
    .sort((a, b) => {
      if (sortBy === 'type') {
        const aType = getFileType(a.file)
        const bType = getFileType(b.file)
        const typeCompare = sortDesc ? bType.localeCompare(aType) : aType.localeCompare(bType)
        // Secondary sort by filename within same type
        if (typeCompare === 0) {
          return a.file.localeCompare(b.file)
        }
        return typeCompare
      } else {
        const aValue = a[sortBy] || 0
        const bValue = b[sortBy] || 0
        return sortDesc ? bValue - aValue : aValue - bValue
      }
    })

  const toggleTypeFilter = (type: string) => {
    const newSelected = new Set(selectedTypes)
    if (newSelected.has(type)) {
      newSelected.delete(type)
    } else {
      newSelected.add(type)
    }
    setSelectedTypes(newSelected)
  }

  const clearFilters = () => {
    setSelectedTypes(new Set())
  }

  const handleSort = (column: 'type' | 'loc' | 'sloc' | 'complexity') => {
    if (sortBy === column) {
      setSortDesc(!sortDesc)
    } else {
      setSortBy(column)
      setSortDesc(column === 'type' ? false : true) // Default ascending for type, descending for numbers
    }
  }

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Code Metrics</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Total Lines</h3>
          <div className="value">{data.metrics.total_loc.toLocaleString()}</div>
        </div>
        <div className="stat-card">
          <h3>Source Lines</h3>
          <div className="value">{data.metrics.total_sloc.toLocaleString()}</div>
        </div>
        <div className="stat-card">
          <h3>Comments</h3>
          <div className="value">{data.metrics.total_comments.toLocaleString()}</div>
        </div>
        <div className="stat-card">
          <h3>Blank Lines</h3>
          <div className="value">{data.metrics.total_blank.toLocaleString()}</div>
        </div>
      </div>

      <div style={{ marginTop: '30px' }}>
        <h3 style={{ marginBottom: '15px' }}>Metrics by File</h3>

        <div style={{ marginBottom: '20px', padding: '15px', backgroundColor: '#f5f5f5', borderRadius: '5px' }}>
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '10px' }}>
            <strong>Filter by File Type:</strong>
            <button
              className="button button-secondary"
              onClick={clearFilters}
              style={{ padding: '5px 10px', fontSize: '0.9em' }}
            >
              Clear Filters
            </button>
          </div>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: '10px' }}>
            {allFileTypes.map(type => (
              <label
                key={type}
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  cursor: 'pointer',
                  padding: '5px 10px',
                  backgroundColor: selectedTypes.has(type) ? '#667eea' : '#fff',
                  color: selectedTypes.has(type) ? '#fff' : '#333',
                  borderRadius: '3px',
                  border: '1px solid #ddd',
                  fontFamily: 'monospace',
                  fontWeight: 'bold',
                  transition: 'all 0.2s'
                }}
              >
                <input
                  type="checkbox"
                  checked={selectedTypes.has(type)}
                  onChange={() => toggleTypeFilter(type)}
                  style={{ marginRight: '5px' }}
                />
                {type}
              </label>
            ))}
          </div>
          <div style={{ marginTop: '10px', fontSize: '0.9em', color: '#666' }}>
            Showing {filteredAndSortedFiles.length} of {data.metrics.by_file.length} files
            {selectedTypes.size > 0 && ` (filtered by ${selectedTypes.size} type${selectedTypes.size > 1 ? 's' : ''})`}
          </div>
        </div>

        <div style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th onClick={() => handleSort('type')} style={{ cursor: 'pointer' }}>
                  Type {sortBy === 'type' && (sortDesc ? '↓' : '↑')}
                </th>
                <th>File</th>
                <th onClick={() => handleSort('loc')} style={{ cursor: 'pointer' }}>
                  LOC {sortBy === 'loc' && (sortDesc ? '↓' : '↑')}
                </th>
                <th onClick={() => handleSort('sloc')} style={{ cursor: 'pointer' }}>
                  SLOC {sortBy === 'sloc' && (sortDesc ? '↓' : '↑')}
                </th>
                <th>Comments</th>
                <th>Blank</th>
                <th style={{ position: 'relative' }}>
                  <div style={{ display: 'flex', alignItems: 'center', gap: '5px', justifyContent: 'center' }}>
                    <span onClick={() => handleSort('complexity')} style={{ cursor: 'pointer' }}>
                      Complexity {sortBy === 'complexity' && (sortDesc ? '↓' : '↑')}
                    </span>
                    <Info
                      size={16}
                      style={{ cursor: 'help', color: '#667eea' }}
                      onClick={() => setShowComplexityInfo(!showComplexityInfo)}
                    />
                  </div>
                  {showComplexityInfo && (
                    <div style={{
                      position: 'absolute',
                      top: '100%',
                      right: 0,
                      marginTop: '5px',
                      padding: '15px',
                      backgroundColor: '#fff',
                      border: '2px solid #667eea',
                      borderRadius: '8px',
                      boxShadow: '0 4px 12px rgba(0,0,0,0.15)',
                      zIndex: 1000,
                      width: '400px',
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
                </th>
                <th>Functions</th>
              </tr>
            </thead>
            <tbody>
              {filteredAndSortedFiles.map((file, index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>{getFileType(file.file)}</td>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>
                    <Link
                      to={`/analysis/${uploadId}/file/${encodeURIComponent(file.file)}`}
                      style={{ color: '#667eea', textDecoration: 'none' }}
                      onMouseEnter={(e) => e.currentTarget.style.textDecoration = 'underline'}
                      onMouseLeave={(e) => e.currentTarget.style.textDecoration = 'none'}
                    >
                      {file.file}
                    </Link>
                  </td>
                  <td>{file.loc}</td>
                  <td>{file.sloc}</td>
                  <td>{file.comments}</td>
                  <td>{file.blank}</td>
                  <td>
                    {file.complexity ? (
                      <span style={{
                        padding: '3px 8px',
                        borderRadius: '4px',
                        fontWeight: 'bold',
                        backgroundColor:
                          file.complexity <= 10 ? '#d4edda' :
                          file.complexity <= 20 ? '#fff3cd' :
                          file.complexity <= 50 ? '#ffeaa7' : '#f8d7da',
                        color:
                          file.complexity <= 10 ? '#155724' :
                          file.complexity <= 20 ? '#856404' :
                          file.complexity <= 50 ? '#d63031' : '#721c24'
                      }}>
                        {file.complexity}
                      </span>
                    ) : '-'}
                  </td>
                  <td>{file.functions || '-'}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

export default MetricsTab
