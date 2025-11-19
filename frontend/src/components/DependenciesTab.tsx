import React from 'react'
import { AnalysisResults } from '../services/api'

interface Props {
  data: AnalysisResults
  uploadId: string
}

const DependenciesTab: React.FC<Props> = ({ data }) => {
  const dependencies = data.dependencies

  // Count total dependencies
  const totalDeps = Object.values(dependencies).reduce((acc, deps) => acc + deps.length, 0)

  // Get most imported modules
  const depCounts: Record<string, number> = {}
  Object.values(dependencies).forEach(deps => {
    deps.forEach(dep => {
      depCounts[dep] = (depCounts[dep] || 0) + 1
    })
  })

  const topDeps = Object.entries(depCounts)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 20)

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Dependencies Analysis</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Files with Dependencies</h3>
          <div className="value">{Object.keys(dependencies).length}</div>
        </div>
        <div className="stat-card">
          <h3>Total Imports</h3>
          <div className="value">{totalDeps}</div>
        </div>
        <div className="stat-card">
          <h3>Unique Modules</h3>
          <div className="value">{Object.keys(depCounts).length}</div>
        </div>
      </div>

      {topDeps.length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Most Imported Modules</h3>
          <table>
            <thead>
              <tr>
                <th>Module</th>
                <th>Import Count</th>
              </tr>
            </thead>
            <tbody>
              {topDeps.map(([module, count], index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace' }}>{module}</td>
                  <td>{count}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      <div style={{ marginTop: '30px' }}>
        <h3 style={{ marginBottom: '15px' }}>Dependencies by File</h3>
        <div style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th>File</th>
                <th>Dependencies</th>
              </tr>
            </thead>
            <tbody>
              {Object.entries(dependencies).map(([file, deps], index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>{file}</td>
                  <td>
                    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '5px' }}>
                      {deps.map((dep, i) => (
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
        </div>
      </div>
    </div>
  )
}

export default DependenciesTab
