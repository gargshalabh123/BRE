import React, { useState } from 'react'
import { AnalysisResults } from '../services/api'

interface Props {
  data: AnalysisResults
}

const DatabaseTab: React.FC<Props> = ({ data }) => {
  const [filterType, setFilterType] = useState<string>('ALL')
  const dbOps = data.database_operations

  const filteredQueries = filterType === 'ALL'
    ? dbOps.queries
    : dbOps.queries.filter(q => q.type === filterType)

  const queryTypes = ['ALL', ...Object.keys(dbOps.by_type)]

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Database Operations</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Total Queries</h3>
          <div className="value">{dbOps.total_count}</div>
        </div>
        {Object.entries(dbOps.by_type).map(([type, count]) => (
          <div key={type} className="stat-card">
            <h3>{type}</h3>
            <div className="value">{count}</div>
          </div>
        ))}
      </div>

      <div style={{ marginTop: '30px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
          <h3>SQL Queries</h3>
          <div>
            <label style={{ marginRight: '10px' }}>Filter by type:</label>
            <select
              value={filterType}
              onChange={(e) => setFilterType(e.target.value)}
              style={{
                padding: '8px',
                borderRadius: '4px',
                border: '1px solid #ddd'
              }}
            >
              {queryTypes.map(type => (
                <option key={type} value={type}>{type}</option>
              ))}
            </select>
          </div>
        </div>

        <div style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th>File</th>
                <th>Line</th>
                <th>Type</th>
                <th>Query</th>
              </tr>
            </thead>
            <tbody>
              {filteredQueries.map((query, index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>{query.file}</td>
                  <td>{query.line}</td>
                  <td>
                    <span className={`badge badge-${
                      query.type === 'SELECT' ? 'primary' :
                      query.type === 'INSERT' ? 'success' :
                      query.type === 'UPDATE' ? 'warning' :
                      query.type === 'DELETE' ? 'danger' : 'primary'
                    }`}>
                      {query.type}
                    </span>
                  </td>
                  <td>
                    <code style={{ fontSize: '0.85em', display: 'block', maxWidth: '500px', overflow: 'auto' }}>
                      {query.query}
                    </code>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>

        {filteredQueries.length === 0 && (
          <p style={{ textAlign: 'center', color: '#666', padding: '20px' }}>
            No {filterType !== 'ALL' ? filterType : ''} queries found
          </p>
        )}
      </div>
    </div>
  )
}

export default DatabaseTab
