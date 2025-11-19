import React, { useState, useEffect } from 'react'
import { PieChart, Pie, Cell, BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts'
import { Loader, FolderTree, ListTree } from 'lucide-react'
import api, { ZipExplorerData, FileMetadata } from '../services/api'
import ZipSummaryCards from './ZipSummaryCards'
import FileTreeViewer from './FileTreeViewer'
import FileListTable from './FileListTable'
import FilePreviewModal from './FilePreviewModal'

interface Props {
  uploadId: string
}

const COLORS = ['#667eea', '#764ba2', '#f093fb', '#4facfe', '#43e97b', '#fa709a']

const ZipExplorerTab: React.FC<Props> = ({ uploadId }) => {
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [explorerData, setExplorerData] = useState<ZipExplorerData | null>(null)
  const [viewMode, setViewMode] = useState<'tree' | 'list'>('tree')
  const [previewFile, setPreviewFile] = useState<FileMetadata | null>(null)

  useEffect(() => {
    const loadZipData = async () => {
      try {
        setLoading(true)
        setError(null)
        const data = await api.exploreZip(uploadId, false)
        setExplorerData(data)
      } catch (err: any) {
        setError(err.response?.data?.detail || 'Failed to load ZIP explorer data')
      } finally {
        setLoading(false)
      }
    }

    loadZipData()
  }, [uploadId])

  const handleFileClick = (file: FileMetadata) => {
    if (file.is_text) {
      setPreviewFile(file)
    }
  }

  const handleTreeFileClick = (filePath: string) => {
    const file = explorerData?.files.find(f => f.path === filePath)
    if (file && file.is_text) {
      setPreviewFile(file)
    }
  }

  if (loading) {
    return (
      <div style={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        padding: '60px',
        flexDirection: 'column',
        gap: '15px'
      }}>
        <Loader className="loading-spinner" size={40} />
        <p style={{ fontSize: '16px', color: '#666' }}>Analyzing ZIP file structure...</p>
      </div>
    )
  }

  if (error) {
    return (
      <div className="error" style={{ margin: '20px 0' }}>
        <strong>Error:</strong> {error}
      </div>
    )
  }

  if (!explorerData) return null

  const { statistics, language_distribution, file_tree, files } = explorerData

  // Prepare language distribution data for chart
  const languageChartData = language_distribution.languages_sorted
    .filter(lang => lang.language !== 'unknown')
    .map(lang => ({
      name: lang.language.toUpperCase(),
      value: lang.total_loc,
      count: lang.file_count,
      percentage: lang.percentage
    }))

  // Prepare directory breakdown data
  const directoryData = Object.entries(statistics.by_directory)
    .sort(([, a], [, b]) => b.loc - a.loc)
    .slice(0, 10)
    .map(([name, data]) => ({
      name: name || 'root',
      files: data.count,
      loc: data.loc
    }))

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>ZIP File Explorer</h2>

      {/* Summary Cards */}
      <ZipSummaryCards data={explorerData} />

      {/* Language Distribution Chart */}
      {languageChartData.length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Language Distribution</h3>
          <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '20px' }}>
            {/* Pie Chart */}
            <div style={{
              backgroundColor: 'white',
              padding: '20px',
              borderRadius: '8px',
              border: '1px solid #e0e0e0'
            }}>
              <ResponsiveContainer width="100%" height={300}>
                <PieChart>
                  <Pie
                    data={languageChartData}
                    cx="50%"
                    cy="50%"
                    labelLine={false}
                    label={({ name, percentage }) => `${name}: ${percentage.toFixed(1)}%`}
                    outerRadius={100}
                    fill="#8884d8"
                    dataKey="value"
                  >
                    {languageChartData.map((entry, index) => (
                      <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                    ))}
                  </Pie>
                  <Tooltip />
                </PieChart>
              </ResponsiveContainer>
            </div>

            {/* Language Stats Table */}
            <div style={{
              backgroundColor: 'white',
              padding: '20px',
              borderRadius: '8px',
              border: '1px solid #e0e0e0'
            }}>
              <h4 style={{ margin: '0 0 15px 0', fontSize: '16px' }}>By Language</h4>
              <table style={{ width: '100%', fontSize: '14px' }}>
                <thead>
                  <tr style={{ borderBottom: '2px solid #e0e0e0' }}>
                    <th style={{ textAlign: 'left', padding: '8px' }}>Language</th>
                    <th style={{ textAlign: 'right', padding: '8px' }}>Files</th>
                    <th style={{ textAlign: 'right', padding: '8px' }}>LOC</th>
                    <th style={{ textAlign: 'right', padding: '8px' }}>%</th>
                  </tr>
                </thead>
                <tbody>
                  {language_distribution.languages_sorted
                    .filter(lang => lang.language !== 'unknown')
                    .map((lang, index) => (
                      <tr key={lang.language} style={{ borderBottom: '1px solid #f0f0f0' }}>
                        <td style={{ padding: '8px' }}>
                          <span style={{
                            display: 'inline-block',
                            width: '10px',
                            height: '10px',
                            borderRadius: '50%',
                            backgroundColor: COLORS[index % COLORS.length],
                            marginRight: '8px'
                          }}></span>
                          {lang.language.toUpperCase()}
                        </td>
                        <td style={{ textAlign: 'right', padding: '8px' }}>{lang.file_count}</td>
                        <td style={{ textAlign: 'right', padding: '8px' }}>
                          {lang.total_loc.toLocaleString()}
                        </td>
                        <td style={{ textAlign: 'right', padding: '8px', fontWeight: 500 }}>
                          {lang.percentage.toFixed(1)}%
                        </td>
                      </tr>
                    ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}

      {/* Directory Breakdown */}
      {directoryData.length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Top Directories by Lines of Code</h3>
          <div style={{
            backgroundColor: 'white',
            padding: '20px',
            borderRadius: '8px',
            border: '1px solid #e0e0e0'
          }}>
            <ResponsiveContainer width="100%" height={300}>
              <BarChart data={directoryData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis
                  dataKey="name"
                  angle={-45}
                  textAnchor="end"
                  height={100}
                  interval={0}
                />
                <YAxis />
                <Tooltip />
                <Legend />
                <Bar dataKey="files" fill="#667eea" name="Files" />
                <Bar dataKey="loc" fill="#764ba2" name="Lines of Code" />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* File Browser */}
      <div style={{ marginTop: '30px' }}>
        <div style={{
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          marginBottom: '15px'
        }}>
          <h3 style={{ margin: 0 }}>File Browser</h3>
          <div style={{ display: 'flex', gap: '10px' }}>
            <button
              onClick={() => setViewMode('tree')}
              className={`button ${viewMode === 'tree' ? '' : 'button-secondary'}`}
              style={{ display: 'flex', alignItems: 'center', gap: '6px' }}
            >
              <FolderTree size={16} />
              Tree View
            </button>
            <button
              onClick={() => setViewMode('list')}
              className={`button ${viewMode === 'list' ? '' : 'button-secondary'}`}
              style={{ display: 'flex', alignItems: 'center', gap: '6px' }}
            >
              <ListTree size={16} />
              List View
            </button>
          </div>
        </div>

        {viewMode === 'tree' ? (
          <FileTreeViewer tree={file_tree} onFileClick={handleTreeFileClick} />
        ) : (
          <FileListTable files={files} onFileClick={handleFileClick} />
        )}
      </div>

      {/* Largest Files */}
      {statistics.largest_files.length > 0 && (
        <div style={{ marginTop: '30px' }}>
          <h3 style={{ marginBottom: '15px' }}>Largest Files</h3>
          <div style={{
            backgroundColor: 'white',
            padding: '20px',
            borderRadius: '8px',
            border: '1px solid #e0e0e0'
          }}>
            <table style={{ width: '100%', fontSize: '14px' }}>
              <thead>
                <tr style={{ borderBottom: '2px solid #e0e0e0' }}>
                  <th style={{ textAlign: 'left', padding: '8px' }}>File</th>
                  <th style={{ textAlign: 'right', padding: '8px' }}>Size</th>
                  <th style={{ textAlign: 'right', padding: '8px' }}>LOC</th>
                </tr>
              </thead>
              <tbody>
                {statistics.largest_files.slice(0, 10).map((file, index) => (
                  <tr key={index} style={{ borderBottom: '1px solid #f0f0f0' }}>
                    <td style={{ padding: '8px', fontFamily: 'monospace', fontSize: '13px' }}>
                      {file.path}
                    </td>
                    <td style={{ textAlign: 'right', padding: '8px' }}>
                      {file.size_kb.toFixed(1)} KB
                    </td>
                    <td style={{ textAlign: 'right', padding: '8px', fontWeight: 500 }}>
                      {file.loc.toLocaleString()}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* File Preview Modal */}
      {previewFile && (
        <FilePreviewModal
          uploadId={uploadId}
          filePath={previewFile.path}
          fileName={previewFile.name}
          onClose={() => setPreviewFile(null)}
        />
      )}
    </div>
  )
}

export default ZipExplorerTab
