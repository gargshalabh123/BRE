import React, { useState } from 'react'
import { AnalysisResults } from '../services/api'

interface Props {
  data: AnalysisResults
}

const BusinessRulesTab: React.FC<Props> = ({ data }) => {
  const [filterType, setFilterType] = useState<string>('ALL')
  const rules = data.business_rules

  // Get unique rule types
  const ruleTypes = ['ALL', ...Array.from(new Set(rules.map(r => r.type)))]

  const filteredRules = filterType === 'ALL'
    ? rules
    : rules.filter(r => r.type === filterType)

  // Count rules by type
  const ruleTypeCounts: Record<string, number> = {}
  rules.forEach(rule => {
    ruleTypeCounts[rule.type] = (ruleTypeCounts[rule.type] || 0) + 1
  })

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Business Rules</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Total Rules Found</h3>
          <div className="value">{rules.length}</div>
        </div>
        {Object.entries(ruleTypeCounts).slice(0, 5).map(([type, count]) => (
          <div key={type} className="stat-card">
            <h3>{type}</h3>
            <div className="value">{count}</div>
          </div>
        ))}
      </div>

      <div style={{ marginTop: '30px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
          <h3>Extracted Rules</h3>
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
              {ruleTypes.map(type => (
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
                <th>Code</th>
              </tr>
            </thead>
            <tbody>
              {filteredRules.map((rule, index) => (
                <tr key={index}>
                  <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>{rule.file}</td>
                  <td>{rule.line}</td>
                  <td>
                    <span className="badge badge-primary">
                      {rule.type}
                    </span>
                  </td>
                  <td>
                    <code style={{ fontSize: '0.85em', display: 'block', maxWidth: '600px', overflow: 'auto' }}>
                      {rule.code}
                    </code>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>

        {filteredRules.length === 0 && (
          <p style={{ textAlign: 'center', color: '#666', padding: '20px' }}>
            No {filterType !== 'ALL' ? filterType : ''} rules found
          </p>
        )}
      </div>

      <div style={{ marginTop: '30px', padding: '15px', background: '#f9f9f9', borderRadius: '6px' }}>
        <h4 style={{ marginBottom: '10px' }}>About Business Rules</h4>
        <p style={{ color: '#666', lineHeight: '1.6' }}>
          Business rules are extracted using pattern matching to identify common rule patterns such as:
          financial calculations, date/age validations, status checks, validation logic, and threshold definitions.
          These represent potential business logic that may require documentation or migration.
        </p>
      </div>
    </div>
  )
}

export default BusinessRulesTab
