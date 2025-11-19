import React, { useState } from 'react'
import { AnalysisResults } from '../services/api'

interface Props {
  data: AnalysisResults
}

const MetricsTab: React.FC<Props> = ({ data }) => {
  const [sortBy, setSortBy] = useState<'loc' | 'sloc' | 'complexity'>('loc')
  const [sortDesc, setSortDesc] = useState(true)

  const sortedFiles = [...data.metrics.by_file].sort((a, b) => {
    const aValue = a[sortBy] || 0
    const bValue = b[sortBy] || 0
    return sortDesc ? bValue - aValue : aValue - bValue
  })

  const handleSort = (column: 'loc' | 'sloc' | 'complexity') => {
    if (sortBy === column) {
      setSortDesc(!sortDesc)
    } else {
      setSortBy(column)
      setSortDesc(true)
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
        <div style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th>File</th>
                <th onClick={() => handleSort('loc')} style={{ cursor: 'pointer' }}>
                  LOC {sortBy === 'loc' && (sortDesc ? '↓' : '↑')}
                </th>
                <th onClick={() => handleSort('sloc')} style={{ cursor: 'pointer' }}>
                  SLOC {sortBy === 'sloc' && (sortDesc ? '↓' : '↑')}
                </th>
                <th>Comments</th>
                <th>Blank</th>
                <th onClick={() => handleSort('complexity')} style={{ cursor: 'pointer' }}>
                  Complexity {sortBy === 'complexity' && (sortDesc ? '↓' : '↑')}
                </th>
                <th>Functions</th>
              </tr>
            </thead>
            <tbody>
              {sortedFiles.map((file, index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>{file.file}</td>
                  <td>{file.loc}</td>
                  <td>{file.sloc}</td>
                  <td>{file.comments}</td>
                  <td>{file.blank}</td>
                  <td>{file.complexity || '-'}</td>
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
