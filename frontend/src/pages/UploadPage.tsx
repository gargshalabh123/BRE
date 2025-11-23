import React, { useState, useCallback, useEffect } from 'react'
import { useNavigate } from 'react-router-dom'
import { useDropzone } from 'react-dropzone'
import { Upload, FileCode, Loader, ArrowLeft } from 'lucide-react'
import api, { ZipUploadResponse } from '../services/api'
import FileExtensionSelector from '../components/FileExtensionSelector'

const UploadPage: React.FC = () => {
  const [uploading, setUploading] = useState(false)
  const [analyzing, setAnalyzing] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [uploadData, setUploadData] = useState<ZipUploadResponse | null>(null)
  const [showExtensionSelector, setShowExtensionSelector] = useState(false)
  const navigate = useNavigate()

  // Debug: Log when showExtensionSelector changes
  useEffect(() => {
    console.log('[UploadPage] showExtensionSelector changed to:', showExtensionSelector)
    console.log('[UploadPage] uploadData:', uploadData)
  }, [showExtensionSelector, uploadData])

  useEffect(() => {
    // Check authentication
    const isAuth = sessionStorage.getItem('isAuthenticated')
    if (!isAuth) {
      navigate('/login')
    }
  }, [navigate])

  const onDrop = useCallback(async (acceptedFiles: File[]) => {
    if (acceptedFiles.length === 0) return

    const file = acceptedFiles[0]
    setUploading(true)
    setError(null)

    try {
      // Use ZIP upload endpoint for ZIP files
      if (file.name.endsWith('.zip')) {
        const response = await api.uploadZip(file)
        console.log('[UploadPage] ZIP upload response:', response)
        console.log('[UploadPage] Available extensions:', response.available_extensions)
        console.log('[UploadPage] Extension count:', response.available_extensions ? Object.keys(response.available_extensions).length : 0)

        setUploadData(response)
        setUploading(false)

        // Show extension selector if we have available extensions
        if (response.available_extensions && Object.keys(response.available_extensions).length > 0) {
          console.log('[UploadPage] Showing extension selector modal')
          setShowExtensionSelector(true)
        } else {
          // No extensions found, navigate directly
          console.log('[UploadPage] No extensions found, navigating directly')
          navigate(`/analysis/${response.upload_id}`)
        }
      } else {
        const response = await api.uploadFile(file)
        navigate(`/analysis/${response.upload_id}`)
      }
    } catch (err: any) {
      setError(err.response?.data?.detail || 'Upload failed. Please try again.')
      setUploading(false)
    }
  }, [navigate])

  const handleExtensionSelection = async (selectedExtensions: string[]) => {
    if (!uploadData) return

    setAnalyzing(true)
    setError(null)

    try {
      // Store selected extensions in sessionStorage for AnalysisPage to use
      sessionStorage.setItem(`selected_extensions_${uploadData.upload_id}`, JSON.stringify(selectedExtensions))

      // Navigate to analysis page - let AnalysisPage do the actual analysis
      navigate(`/analysis/${uploadData.upload_id}`)
    } catch (err: any) {
      setError(err.response?.data?.detail || 'Analysis failed. Please try again.')
      setAnalyzing(false)
      setShowExtensionSelector(false)
    }
  }

  const handleCancelExtensionSelector = () => {
    setShowExtensionSelector(false)
    setUploadData(null)
  }

  const { getRootProps, getInputProps, isDragActive } = useDropzone({
    onDrop,
    accept: {
      'application/zip': ['.zip'],
      'text/x-cobol': ['.cbl', '.cob', '.cobol', '.cpy'],
      'text/x-sql': ['.sql', '.ddl', '.dml'],
      'text/x-rpg': ['.rpg', '.rpgle', '.rpglec', '.sqlrpgle', '.rpg4', '.rpgiv'],
      'application/x-as400': ['.dspf', '.prtf', '.lf', '.pf']
    },
    multiple: false,
    disabled: uploading
  })

  return (
    <div className="container">
      <div className="header">
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '10px' }}>
          <div>
            <h1>Business Rules Extraction Framework</h1>
          </div>
          <button
            className="button button-secondary"
            onClick={() => navigate('/home')}
            style={{ fontSize: '14px', padding: '8px 16px' }}
          >
            <ArrowLeft size={16} style={{ marginRight: '5px' }} />
            Back to Home
          </button>
        </div>
        <p>Upload your legacy codebase for comprehensive analysis and business rules extraction</p>
      </div>

      {error && (
        <div className="error">
          <strong>Error:</strong> {error}
        </div>
      )}

      <div className="card">
        <div
          {...getRootProps()}
          className={`dropzone ${isDragActive ? 'active' : ''}`}
        >
          <input {...getInputProps()} />
          {uploading || analyzing ? (
            <>
              <Loader size={48} className="loading-spinner" />
              <p style={{ marginTop: '20px', fontSize: '18px' }}>
                {uploading ? 'Uploading and extracting...' : 'Analyzing selected files...'}
              </p>
            </>
          ) : (
            <>
              <Upload size={48} color="#667eea" />
              <h2 style={{ marginTop: '20px', marginBottom: '10px' }}>
                {isDragActive ? 'Drop your file here' : 'Drag & drop a file or ZIP archive'}
              </h2>
              <p style={{ color: '#666', marginBottom: '20px' }}>
                or click to browse your files
              </p>
              <button className="button">
                Select File
              </button>
            </>
          )}
        </div>
      </div>

      <div className="card">
        <h3 style={{ marginBottom: '15px' }}>Supported Features</h3>
        <div className="grid">
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>Code Analysis</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              LOC counting, file type detection, and complexity metrics
            </p>
          </div>
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>Dependencies</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              Extract and visualize code dependencies and imports
            </p>
          </div>
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>Database Operations</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              Identify SQL queries and database interactions
            </p>
          </div>
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>Business Rules</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              Extract business logic using pattern matching
            </p>
          </div>
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>AI Explanations</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              Generate natural language explanations using GenAI
            </p>
          </div>
          <div style={{ padding: '15px' }}>
            <FileCode size={32} color="#667eea" style={{ marginBottom: '10px' }} />
            <h4>Visualizations</h4>
            <p style={{ color: '#666', marginTop: '8px' }}>
              Interactive charts and dependency graphs
            </p>
          </div>
        </div>
      </div>

      <div className="card">
        <h3 style={{ marginBottom: '15px' }}>Supported Languages</h3>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '10px' }}>
          {['COBOL', 'SQL', 'AS400/RPG'].map(lang => (
            <span key={lang} className="badge badge-primary">{lang}</span>
          ))}
        </div>
        <p style={{ marginTop: '15px', color: '#666', fontSize: '14px' }}>
          Supported file types: .cbl, .cob, .cobol, .cpy, .sql, .ddl, .dml, .rpg, .rpgle, .rpglec, .sqlrpgle, .rpg4, .rpgiv, .dspf, .prtf, .lf, .pf
        </p>
      </div>

      {/* File Extension Selector Modal */}
      {(() => {
        const shouldShow = showExtensionSelector && uploadData && uploadData.available_extensions
        console.log('[UploadPage] Modal render check:', {
          showExtensionSelector,
          hasUploadData: !!uploadData,
          hasAvailableExtensions: uploadData?.available_extensions ? Object.keys(uploadData.available_extensions).length : 0,
          shouldShow
        })
        return shouldShow && (
          <FileExtensionSelector
            availableExtensions={uploadData.available_extensions!}
            recommendedExtensions={uploadData.recommended_extensions || []}
            onSelectionChange={handleExtensionSelection}
            onClose={handleCancelExtensionSelector}
          />
        )
      })()}
    </div>
  )
}

export default UploadPage
