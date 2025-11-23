import React, { useState, useEffect } from 'react'
import { useParams } from 'react-router-dom'
import { Loader } from 'lucide-react'
import api, { AnalysisResults, ZipExplorerData } from '../services/api'
import FileTreeViewer from './FileTreeViewer'

interface Props {
  data: AnalysisResults
  uploadId: string
}

const OverviewTab: React.FC<Props> = ({ data, uploadId }) => {
  const { summary, metrics, database_operations, business_rules } = data
  const [zipData, setZipData] = useState<ZipExplorerData | null>(null)
  const [loadingZip, setLoadingZip] = useState(true)
  const [selectedExtensions, setSelectedExtensions] = useState<string[] | null>(null)

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

  // Get selected extensions from analysis data
  useEffect(() => {
    if (data.selected_extensions) {
      setSelectedExtensions(data.selected_extensions)
    }
  }, [data])

  // Navigate to file detail page when file is clicked in tree
  const handleFileClick = (filePath: string) => {
    window.location.href = `/analysis/${uploadId}/file/${encodeURIComponent(filePath)}`
  }

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>Codebase Overview</h2>

      <div className="grid">
        <div className="stat-card">
          <h3>Total Files</h3>
          <div className="value">{summary?.total_files || metrics?.by_file?.length || 0}</div>
        </div>
        <div className="stat-card">
          <h3>Total Size</h3>
          <div className="value">{summary?.total_size_mb || 'N/A'} {summary?.total_size_mb ? 'MB' : ''}</div>
        </div>
        <div className="stat-card">
          <h3>Lines of Code</h3>
          <div className="value">{metrics?.total_loc?.toLocaleString() || 0}</div>
        </div>
        <div className="stat-card">
          <h3>Source Lines</h3>
          <div className="value">{metrics?.total_sloc?.toLocaleString() || 0}</div>
        </div>
        <div className="stat-card">
          <h3>DB Operations</h3>
          <div className="value">{database_operations?.total_count || 0}</div>
        </div>
        <div className="stat-card">
          <h3>Business Rules</h3>
          <div className="value">{business_rules?.length || 0}</div>
        </div>
      </div>

      {/* ZIP Explorer Data */}
      {loadingZip && (
        <div style={{ marginTop: '30px', textAlign: 'center', padding: '20px' }}>
          <Loader className="loading-spinner" size={24} />
          <p style={{ marginTop: '10px', color: '#666' }}>Loading detailed file analysis...</p>
        </div>
      )}

      {zipData && (
        <>
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
                    .filter(([ext]) => {
                      // If selectedExtensions is set, only show those extensions
                      if (selectedExtensions && selectedExtensions.length > 0) {
                        return selectedExtensions.includes(ext)
                      }
                      // Otherwise show all (fallback for old analyses without selected_extensions)
                      return true
                    })
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

          {/* File Structure Tree */}
          <div style={{ marginTop: '30px' }}>
            <h3 style={{ marginBottom: '15px' }}>File Structure</h3>
            <div style={{
              backgroundColor: 'white',
              padding: '20px',
              borderRadius: '8px',
              border: '1px solid #e0e0e0',
              maxHeight: '600px',
              overflow: 'auto'
            }}>
              <FileTreeViewer tree={zipData.file_tree} onFileClick={handleFileClick} />
            </div>
          </div>
        </>
      )}
    </div>
  )
}

export default OverviewTab
