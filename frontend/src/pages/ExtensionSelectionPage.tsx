import React, { useState, useEffect } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import { ArrowLeft, CheckSquare, Square, Loader, FileCode, AlertCircle } from 'lucide-react'
import api, { ZipExplorerData } from '../services/api'

const ExtensionSelectionPage: React.FC = () => {
  const { uploadId } = useParams<{ uploadId: string }>()
  const navigate = useNavigate()
  const [zipData, setZipData] = useState<ZipExplorerData | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [selectedExtensions, setSelectedExtensions] = useState<Set<string>>(new Set())
  const [analyzing, setAnalyzing] = useState(false)

  useEffect(() => {
    const isAuth = sessionStorage.getItem('isAuthenticated')
    if (!isAuth) {
      navigate('/login')
      return
    }

    if (!uploadId) {
      navigate('/upload')
      return
    }

    loadZipData()
  }, [uploadId, navigate])

  const loadZipData = async () => {
    try {
      setLoading(true)
      setError(null)
      const data = await api.exploreZip(uploadId!, false)
      setZipData(data)

      // Pre-select common code extensions
      const codeExtensions = new Set<string>()
      Object.keys(data.statistics.by_extension).forEach(ext => {
        const extLower = ext.toLowerCase()
        // Pre-select common code file extensions
        if (['.cbl', '.cob', '.cobol', '.cpy', '.sql', '.ddl', '.dml',
             '.rpg', '.rpgle', '.rpglec', '.sqlrpgle', '.rpg4', '.rpgiv',
             '.dspf', '.prtf', '.lf', '.pf', '.java', '.py', '.js', '.ts',
             '.c', '.cpp', '.h', '.cs', '.txt'].includes(extLower)) {
          codeExtensions.add(ext)
        }
      })
      setSelectedExtensions(codeExtensions)
      setLoading(false)
    } catch (err) {
      console.error('Failed to load ZIP data:', err)
      setError('Failed to load ZIP file information. Please try again.')
      setLoading(false)
    }
  }

  const toggleExtension = (extension: string) => {
    const newSelected = new Set(selectedExtensions)
    if (newSelected.has(extension)) {
      newSelected.delete(extension)
    } else {
      newSelected.add(extension)
    }
    setSelectedExtensions(newSelected)
  }

  const toggleAll = () => {
    if (!zipData) return

    if (selectedExtensions.size === Object.keys(zipData.statistics.by_extension).length) {
      setSelectedExtensions(new Set())
    } else {
      setSelectedExtensions(new Set(Object.keys(zipData.statistics.by_extension)))
    }
  }

  const handleProceed = async () => {
    if (selectedExtensions.size === 0) {
      setError('Please select at least one file type to analyze.')
      return
    }

    try {
      setAnalyzing(true)
      setError(null)

      // Store selected extensions in sessionStorage for the analysis to use
      sessionStorage.setItem(`extensions_${uploadId}`, JSON.stringify(Array.from(selectedExtensions)))

      // Navigate to analysis page
      navigate(`/analysis/${uploadId}`)
    } catch (err) {
      console.error('Failed to start analysis:', err)
      setError('Failed to start analysis. Please try again.')
      setAnalyzing(false)
    }
  }

  if (loading) {
    return (
      <div style={{ minHeight: '100vh', backgroundColor: '#f5f7fa', padding: '60px 30px' }}>
        <div style={{ textAlign: 'center' }}>
          <Loader className="loading-spinner" size={48} />
          <p style={{ marginTop: '20px', fontSize: '16px', color: '#666' }}>Loading file information...</p>
        </div>
      </div>
    )
  }

  if (!zipData) {
    return (
      <div style={{ minHeight: '100vh', backgroundColor: '#f5f7fa', padding: '60px 30px' }}>
        <div style={{ textAlign: 'center' }}>
          <p style={{ fontSize: '16px', color: '#c33' }}>Failed to load ZIP data</p>
        </div>
      </div>
    )
  }

  const extensionStats = zipData.statistics.by_extension
  const totalFiles = Object.values(extensionStats).reduce((sum, stats) => sum + stats.count, 0)
  const selectedFiles = Array.from(selectedExtensions).reduce((sum, ext) =>
    sum + (extensionStats[ext]?.count || 0), 0
  )

  return (
    <div style={{ minHeight: '100vh', backgroundColor: '#f5f7fa' }}>
      {/* Header */}
      <div style={{
        backgroundColor: '#fff',
        borderBottom: '1px solid #e0e0e0',
        padding: '20px 30px',
        boxShadow: '0 2px 4px rgba(0,0,0,0.05)'
      }}>
        <div style={{ maxWidth: '1400px', margin: '0 auto' }}>
          <button
            onClick={() => navigate('/upload')}
            disabled={analyzing}
            style={{
              padding: '8px 16px',
              fontSize: '14px',
              backgroundColor: '#fff',
              border: '1px solid #ddd',
              borderRadius: '6px',
              cursor: analyzing ? 'not-allowed' : 'pointer',
              display: 'flex',
              alignItems: 'center',
              gap: '8px',
              color: '#666',
              marginBottom: '15px',
              transition: 'all 0.2s',
              opacity: analyzing ? 0.5 : 1
            }}
            onMouseEnter={(e) => {
              if (!analyzing) {
                e.currentTarget.style.backgroundColor = '#f8f9fa'
                e.currentTarget.style.borderColor = '#667eea'
              }
            }}
            onMouseLeave={(e) => {
              if (!analyzing) {
                e.currentTarget.style.backgroundColor = '#fff'
                e.currentTarget.style.borderColor = '#ddd'
              }
            }}
          >
            <ArrowLeft size={16} />
            Back to Upload
          </button>

          <h1 style={{ margin: 0, fontSize: '28px', color: '#333' }}>
            Select File Types to Analyze
          </h1>
          <p style={{ margin: '5px 0 0 0', fontSize: '14px', color: '#666' }}>
            Choose which file extensions you want to include in the analysis
          </p>
        </div>
      </div>

      {/* Main Content */}
      <main style={{
        maxWidth: '1400px',
        margin: '0 auto',
        padding: '40px 30px'
      }}>
        {error && (
          <div style={{
            padding: '15px',
            backgroundColor: '#fee',
            border: '1px solid #fcc',
            borderRadius: '8px',
            marginBottom: '20px',
            color: '#c33',
            display: 'flex',
            alignItems: 'center',
            gap: '10px'
          }}>
            <AlertCircle size={20} />
            {error}
          </div>
        )}

        {/* Summary Stats */}
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
          gap: '20px',
          marginBottom: '30px'
        }}>
          <div style={{
            backgroundColor: '#fff',
            borderRadius: '12px',
            padding: '20px',
            boxShadow: '0 2px 8px rgba(0,0,0,0.05)'
          }}>
            <h3 style={{ margin: '0 0 10px 0', fontSize: '14px', color: '#666', fontWeight: '500' }}>
              Total Files in ZIP
            </h3>
            <div style={{ fontSize: '32px', fontWeight: 'bold', color: '#667eea' }}>
              {totalFiles}
            </div>
          </div>

          <div style={{
            backgroundColor: '#fff',
            borderRadius: '12px',
            padding: '20px',
            boxShadow: '0 2px 8px rgba(0,0,0,0.05)'
          }}>
            <h3 style={{ margin: '0 0 10px 0', fontSize: '14px', color: '#666', fontWeight: '500' }}>
              Selected Files
            </h3>
            <div style={{ fontSize: '32px', fontWeight: 'bold', color: '#2e7d32' }}>
              {selectedFiles}
            </div>
          </div>

          <div style={{
            backgroundColor: '#fff',
            borderRadius: '12px',
            padding: '20px',
            boxShadow: '0 2px 8px rgba(0,0,0,0.05)'
          }}>
            <h3 style={{ margin: '0 0 10px 0', fontSize: '14px', color: '#666', fontWeight: '500' }}>
              File Types
            </h3>
            <div style={{ fontSize: '32px', fontWeight: 'bold', color: '#764ba2' }}>
              {selectedExtensions.size} / {Object.keys(extensionStats).length}
            </div>
          </div>
        </div>

        {/* Extension Selection */}
        <div style={{
          backgroundColor: '#fff',
          borderRadius: '12px',
          padding: '30px',
          boxShadow: '0 2px 8px rgba(0,0,0,0.05)'
        }}>
          <div style={{
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            marginBottom: '20px'
          }}>
            <h3 style={{ margin: 0, fontSize: '18px', color: '#333' }}>
              Available File Extensions
            </h3>
            <button
              onClick={toggleAll}
              disabled={analyzing}
              style={{
                padding: '8px 16px',
                fontSize: '14px',
                backgroundColor: '#f8f9fa',
                border: '1px solid #ddd',
                borderRadius: '6px',
                cursor: analyzing ? 'not-allowed' : 'pointer',
                color: '#667eea',
                fontWeight: '500',
                transition: 'all 0.2s',
                opacity: analyzing ? 0.5 : 1
              }}
              onMouseEnter={(e) => {
                if (!analyzing) {
                  e.currentTarget.style.backgroundColor = '#e9ecef'
                }
              }}
              onMouseLeave={(e) => {
                if (!analyzing) {
                  e.currentTarget.style.backgroundColor = '#f8f9fa'
                }
              }}
            >
              {selectedExtensions.size === Object.keys(extensionStats).length ? 'Deselect All' : 'Select All'}
            </button>
          </div>

          <div style={{
            display: 'grid',
            gridTemplateColumns: 'repeat(auto-fill, minmax(300px, 1fr))',
            gap: '12px'
          }}>
            {Object.entries(extensionStats)
              .sort(([a], [b]) => a.localeCompare(b))
              .map(([extension, stats]) => {
                const isSelected = selectedExtensions.has(extension)
                return (
                  <div
                    key={extension}
                    onClick={() => !analyzing && toggleExtension(extension)}
                    style={{
                      padding: '16px',
                      border: `2px solid ${isSelected ? '#667eea' : '#e5e7eb'}`,
                      borderRadius: '8px',
                      cursor: analyzing ? 'not-allowed' : 'pointer',
                      backgroundColor: isSelected ? '#f0f3ff' : '#fff',
                      transition: 'all 0.2s',
                      display: 'flex',
                      alignItems: 'center',
                      gap: '12px',
                      opacity: analyzing ? 0.5 : 1
                    }}
                    onMouseEnter={(e) => {
                      if (!analyzing) {
                        e.currentTarget.style.transform = 'translateY(-2px)'
                        e.currentTarget.style.boxShadow = '0 4px 8px rgba(0,0,0,0.1)'
                      }
                    }}
                    onMouseLeave={(e) => {
                      if (!analyzing) {
                        e.currentTarget.style.transform = 'translateY(0)'
                        e.currentTarget.style.boxShadow = 'none'
                      }
                    }}
                  >
                    {isSelected ? (
                      <CheckSquare size={20} color="#667eea" />
                    ) : (
                      <Square size={20} color="#9ca3af" />
                    )}
                    <div style={{ flex: 1 }}>
                      <div style={{
                        fontFamily: 'monospace',
                        fontWeight: 'bold',
                        fontSize: '14px',
                        color: '#333',
                        marginBottom: '4px'
                      }}>
                        {extension}
                      </div>
                      <div style={{ fontSize: '12px', color: '#666' }}>
                        {stats.count} file{stats.count !== 1 ? 's' : ''} • {stats.loc.toLocaleString()} LOC
                      </div>
                    </div>
                  </div>
                )
              })}
          </div>
        </div>

        {/* Action Buttons */}
        <div style={{
          marginTop: '30px',
          display: 'flex',
          justifyContent: 'flex-end',
          gap: '15px'
        }}>
          <button
            onClick={() => navigate('/upload')}
            disabled={analyzing}
            style={{
              padding: '12px 24px',
              fontSize: '14px',
              fontWeight: 'bold',
              color: '#666',
              backgroundColor: '#fff',
              border: '1px solid #ddd',
              borderRadius: '6px',
              cursor: analyzing ? 'not-allowed' : 'pointer',
              transition: 'all 0.2s',
              opacity: analyzing ? 0.5 : 1
            }}
            onMouseEnter={(e) => {
              if (!analyzing) {
                e.currentTarget.style.backgroundColor = '#f8f9fa'
              }
            }}
            onMouseLeave={(e) => {
              if (!analyzing) {
                e.currentTarget.style.backgroundColor = '#fff'
              }
            }}
          >
            Cancel
          </button>

          <button
            onClick={handleProceed}
            disabled={analyzing || selectedExtensions.size === 0}
            style={{
              padding: '12px 32px',
              fontSize: '14px',
              fontWeight: 'bold',
              color: '#fff',
              background: analyzing || selectedExtensions.size === 0
                ? '#ccc'
                : 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
              border: 'none',
              borderRadius: '6px',
              cursor: analyzing || selectedExtensions.size === 0 ? 'not-allowed' : 'pointer',
              transition: 'all 0.2s',
              boxShadow: analyzing || selectedExtensions.size === 0
                ? 'none'
                : '0 4px 12px rgba(102, 126, 234, 0.4)',
              display: 'flex',
              alignItems: 'center',
              gap: '8px'
            }}
            onMouseEnter={(e) => {
              if (!analyzing && selectedExtensions.size > 0) {
                e.currentTarget.style.transform = 'translateY(-2px)'
                e.currentTarget.style.boxShadow = '0 6px 16px rgba(102, 126, 234, 0.5)'
              }
            }}
            onMouseLeave={(e) => {
              if (!analyzing && selectedExtensions.size > 0) {
                e.currentTarget.style.transform = 'translateY(0)'
                e.currentTarget.style.boxShadow = '0 4px 12px rgba(102, 126, 234, 0.4)'
              }
            }}
          >
            {analyzing ? (
              <>
                <Loader className="loading-spinner" size={16} />
                Starting Analysis...
              </>
            ) : (
              <>
                <FileCode size={16} />
                Proceed with Analysis ({selectedFiles} files)
              </>
            )}
          </button>
        </div>
      </main>
    </div>
  )
}

export default ExtensionSelectionPage
