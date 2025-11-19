import React, { useState, useEffect } from 'react'
import { BarChart, Bar, PieChart, Pie, Cell, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts'
import { Loader } from 'lucide-react'
import api, { AnalysisResults, ZipExplorerData } from '../services/api'

interface Props {
  data: AnalysisResults
  uploadId: string
}

const COLORS = ['#667eea', '#764ba2', '#f093fb', '#4facfe', '#43e97b', '#fa709a']

const OverviewTab: React.FC<Props> = ({ data, uploadId }) => {
  const { summary, metrics, database_operations, business_rules } = data
  const [zipData, setZipData] = useState<ZipExplorerData | null>(null)
  const [loadingZip, setLoadingZip] = useState(true)

  useEffect(() => {
    const loadZipData = async () => {
      try {
        const explorerData = await api.exploreZip(uploadId, false)
        setZipData(explorerData)
      } catch (err) {
        console.error('Failed to load ZIP data:', err)
      } finally {
        setLoadingZip(false)
      }
    }
    loadZipData()
  }, [uploadId])

  // Prepare file types chart data
  const fileTypesData = Object.entries(summary.file_types).map(([name, value]) => ({
    name,
    value
  }))

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Codebase Overview</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Total Files</h3>
          <div className="value">{summary.total_files}</div>
        </div>
        <div className="stat-card">
          <h3>Total Size</h3>
          <div className="value">{summary.total_size_mb} MB</div>
        </div>
        <div className="stat-card">
          <h3>Lines of Code</h3>
          <div className="value">{metrics.total_loc.toLocaleString()}</div>
        </div>
        <div className="stat-card">
          <h3>Source Lines</h3>
          <div className="value">{metrics.total_sloc.toLocaleString()}</div>
        </div>
        <div className="stat-card">
          <h3>DB Operations</h3>
          <div className="value">{database_operations.total_count}</div>
        </div>
        <div className="stat-card">
          <h3>Business Rules</h3>
          <div className="value">{business_rules.length}</div>
        </div>
      </div>

      <div style={{ marginTop: '30px' }}>
        <h3 style={{ marginBottom: '15px' }}>File Types Distribution</h3>
        <ResponsiveContainer width="100%" height={300}>
          <PieChart>
            <Pie
              data={fileTypesData}
              cx="50%"
              cy="50%"
              labelLine={false}
              label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
              outerRadius={100}
              fill="#8884d8"
              dataKey="value"
            >
              {fileTypesData.map((entry, index) => (
                <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
              ))}
            </Pie>
            <Tooltip />
          </PieChart>
        </ResponsiveContainer>
      </div>

      <div style={{ marginTop: '30px' }}>
        <h3 style={{ marginBottom: '15px' }}>Code Composition</h3>
        <ResponsiveContainer width="100%" height={300}>
          <BarChart
            data={[
              { name: 'Source Lines', value: metrics.total_sloc },
              { name: 'Comments', value: metrics.total_comments },
              { name: 'Blank Lines', value: metrics.total_blank }
            ]}
          >
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis dataKey="name" />
            <YAxis />
            <Tooltip />
            <Bar dataKey="value" fill="#667eea" />
          </BarChart>
        </ResponsiveContainer>
      </div>

      {Object.keys(database_operations.by_type).length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Database Operations by Type</h3>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart
              data={Object.entries(database_operations.by_type).map(([name, value]) => ({
                name,
                value
              }))}
            >
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" />
              <YAxis />
              <Tooltip />
              <Bar dataKey="value" fill="#764ba2" />
            </BarChart>
          </ResponsiveContainer>
        </div>
      )}

      {/* ZIP Explorer Data */}
      {loadingZip && (
        <div style={{ marginTop: '30px', textAlign: 'center', padding: '20px' }}>
          <Loader className="loading-spinner" size={24} />
          <p style={{ marginTop: '10px', color: '#666' }}>Loading detailed file analysis...</p>
        </div>
      )}

      {zipData && (
        <>
          <div style={{ marginTop: '40px' }}>
            <h3 style={{ marginBottom: '15px' }}>Language Distribution</h3>
            <ResponsiveContainer width="100%" height={300}>
              <PieChart>
                <Pie
                  data={zipData.language_distribution.languages_sorted
                    .filter(lang => lang.language !== 'unknown')
                    .map(lang => ({
                      name: lang.language.toUpperCase(),
                      value: lang.total_loc
                    }))}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
                  outerRadius={100}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {zipData.language_distribution.languages_sorted
                    .filter(lang => lang.language !== 'unknown')
                    .map((entry, index) => (
                      <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                    ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          </div>

          <div style={{ marginTop: '30px' }}>
            <h3 style={{ marginBottom: '15px' }}>Detailed Breakdown by Extension</h3>
            <div style={{ overflowX: 'auto' }}>
              <table>
                <thead>
                  <tr>
                    <th>Extension</th>
                    <th>Language</th>
                    <th>Files</th>
                    <th>Total LOC</th>
                  </tr>
                </thead>
                <tbody>
                  {Object.entries(zipData.statistics.by_extension)
                    .sort(([, a], [, b]) => b.loc - a.loc)
                    .map(([ext, stats]: [string, any]) => (
                      <tr key={ext}>
                        <td style={{ fontFamily: 'monospace', fontWeight: 'bold' }}>{ext}</td>
                        <td>
                          {zipData.files.find(f => f.extension === ext)?.language?.toUpperCase() || 'Unknown'}
                        </td>
                        <td>{stats.count}</td>
                        <td>{stats.loc.toLocaleString()}</td>
                      </tr>
                    ))}
                </tbody>
              </table>
            </div>
          </div>

          <div style={{ marginTop: '30px' }}>
            <h3 style={{ marginBottom: '15px' }}>Top 10 Largest Files by LOC</h3>
            <div style={{ overflowX: 'auto' }}>
              <table>
                <thead>
                  <tr>
                    <th>File</th>
                    <th>Language</th>
                    <th>Lines of Code</th>
                    <th>Size (KB)</th>
                  </tr>
                </thead>
                <tbody>
                  {zipData.statistics.largest_files.map((file, index) => (
                    <tr key={index}>
                      <td style={{ fontFamily: 'monospace', fontSize: '0.9em' }}>{file.path}</td>
                      <td>{zipData.files.find(f => f.path === file.path)?.language || 'Unknown'}</td>
                      <td>{file.loc.toLocaleString()}</td>
                      <td>{file.size_kb.toFixed(2)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          <div style={{ marginTop: '30px' }}>
            <h3 style={{ marginBottom: '15px' }}>Top 10 Directories by LOC</h3>
            <ResponsiveContainer width="100%" height={300}>
              <BarChart
                data={Object.entries(zipData.statistics.by_directory)
                  .sort(([, a], [, b]) => b.loc - a.loc)
                  .slice(0, 10)
                  .map(([name, data]) => ({
                    name: name || 'root',
                    loc: data.loc,
                    files: data.count
                  }))}
              >
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="name" angle={-45} textAnchor="end" height={100} />
                <YAxis />
                <Tooltip />
                <Bar dataKey="loc" fill="#43e97b" name="Lines of Code" />
              </BarChart>
            </ResponsiveContainer>
          </div>

          <div className="grid" style={{ marginTop: '30px' }}>
            <div className="stat-card">
              <h3>Unique Extensions</h3>
              <div className="value">{zipData.statistics.unique_extensions}</div>
            </div>
            <div className="stat-card">
              <h3>Unique Directories</h3>
              <div className="value">{zipData.statistics.unique_directories}</div>
            </div>
            <div className="stat-card">
              <h3>Text Files</h3>
              <div className="value">{zipData.statistics.text_files}</div>
            </div>
            <div className="stat-card">
              <h3>Binary Files</h3>
              <div className="value">{zipData.statistics.binary_files}</div>
            </div>
            <div className="stat-card">
              <h3>Compression Ratio</h3>
              <div className="value">{zipData.statistics.overall_compression_ratio.toFixed(1)}x</div>
            </div>
          </div>
        </>
      )}
    </div>
  )
}

export default OverviewTab
