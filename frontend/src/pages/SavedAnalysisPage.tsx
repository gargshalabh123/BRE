import React, { useState, useEffect } from 'react'
import { useNavigate } from 'react-router-dom'
import { ArrowLeft, Calendar, FileText, BarChart2, Database, AlertCircle } from 'lucide-react'

interface SavedAnalysis {
  upload_id: string
  upload_filename: string
  analysis_date: string
  total_files: number
  total_loc: number
  status: string
}

const SavedAnalysisPage: React.FC = () => {
  const navigate = useNavigate()
  const [analyses, setAnalyses] = useState<SavedAnalysis[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    // Check authentication
    const isAuth = sessionStorage.getItem('isAuthenticated')
    if (!isAuth) {
      navigate('/login')
      return
    }

    // TODO: Load saved analyses from database
    // For now, show mock data or empty state
    loadSavedAnalyses()
  }, [navigate])

  const loadSavedAnalyses = async () => {
    try {
      setLoading(true)
      setError(null)

      const response = await fetch('http://localhost:8000/api/analysis/saved')

      if (!response.ok) {
        throw new Error(`Failed to load saved analyses: ${response.statusText}`)
      }

      const data = await response.json()

      // Transform API data to match our interface
      const transformedAnalyses = data.analyses.map((analysis: any) => ({
        upload_id: analysis.upload_id,
        upload_filename: analysis.upload_filename || 'Unknown',
        analysis_date: analysis.analysis_date,
        total_files: analysis.total_files || 0,
        total_loc: analysis.total_loc || 0,
        status: analysis.status || 'unknown'
      }))

      setAnalyses(transformedAnalyses)
      setLoading(false)
    } catch (err) {
      console.error('Failed to load saved analyses:', err)
      setError('Failed to load saved analyses. Please try again.')
      setLoading(false)
    }
  }

  const handleViewAnalysis = (uploadId: string) => {
    navigate(`/analysis/${uploadId}`)
  }

  if (loading) {
    return (
      <div style={{ minHeight: '100vh', backgroundColor: '#f5f7fa', padding: '60px 30px' }}>
        <div style={{ textAlign: 'center' }}>
          <p style={{ fontSize: '16px', color: '#666' }}>Loading saved analyses...</p>
        </div>
      </div>
    )
  }

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
            onClick={() => navigate('/home')}
            style={{
              padding: '8px 16px',
              fontSize: '14px',
              backgroundColor: '#fff',
              border: '1px solid #ddd',
              borderRadius: '6px',
              cursor: 'pointer',
              display: 'flex',
              alignItems: 'center',
              gap: '8px',
              color: '#666',
              marginBottom: '15px',
              transition: 'all 0.2s'
            }}
            onMouseEnter={(e) => {
              e.currentTarget.style.backgroundColor = '#f8f9fa'
              e.currentTarget.style.borderColor = '#667eea'
            }}
            onMouseLeave={(e) => {
              e.currentTarget.style.backgroundColor = '#fff'
              e.currentTarget.style.borderColor = '#ddd'
            }}
          >
            <ArrowLeft size={16} />
            Back to Home
          </button>

          <h1 style={{ margin: 0, fontSize: '28px', color: '#333' }}>
            Saved Analysis
          </h1>
          <p style={{ margin: '5px 0 0 0', fontSize: '14px', color: '#666' }}>
            Browse and manage your previously analyzed codebases
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

        {analyses.length === 0 ? (
          // Empty State
          <div style={{
            backgroundColor: '#fff',
            borderRadius: '12px',
            padding: '80px 40px',
            textAlign: 'center',
            boxShadow: '0 2px 8px rgba(0,0,0,0.05)'
          }}>
            <div style={{
              width: '100px',
              height: '100px',
              margin: '0 auto 30px',
              backgroundColor: '#f5f7fa',
              borderRadius: '50%',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center'
            }}>
              <Database size={50} color="#ccc" />
            </div>

            <h2 style={{
              margin: '0 0 15px 0',
              fontSize: '24px',
              color: '#333'
            }}>
              No Saved Analysis Found
            </h2>

            <p style={{
              margin: '0 auto 30px',
              fontSize: '14px',
              color: '#666',
              maxWidth: '500px'
            }}>
              You haven't analyzed any codebases yet. Upload a COBOL ZIP file to get started with comprehensive code analysis.
            </p>

            <button
              onClick={() => navigate('/upload')}
              style={{
                padding: '12px 24px',
                fontSize: '14px',
                fontWeight: 'bold',
                color: '#fff',
                background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
                border: 'none',
                borderRadius: '6px',
                cursor: 'pointer',
                transition: 'transform 0.2s, box-shadow 0.2s',
                boxShadow: '0 4px 12px rgba(102, 126, 234, 0.4)'
              }}
              onMouseEnter={(e) => {
                e.currentTarget.style.transform = 'translateY(-2px)'
                e.currentTarget.style.boxShadow = '0 6px 16px rgba(102, 126, 234, 0.5)'
              }}
              onMouseLeave={(e) => {
                e.currentTarget.style.transform = 'translateY(0)'
                e.currentTarget.style.boxShadow = '0 4px 12px rgba(102, 126, 234, 0.4)'
              }}
            >
              Create New Analysis
            </button>
          </div>
        ) : (
          // Analysis List
          <div style={{
            display: 'grid',
            gap: '20px'
          }}>
            {analyses.map((analysis, index) => (
              <div
                key={index}
                onClick={() => handleViewAnalysis(analysis.upload_id)}
                style={{
                  backgroundColor: '#fff',
                  borderRadius: '12px',
                  padding: '30px',
                  boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
                  cursor: 'pointer',
                  transition: 'all 0.2s',
                  border: '2px solid transparent'
                }}
                onMouseEnter={(e) => {
                  e.currentTarget.style.transform = 'translateY(-4px)'
                  e.currentTarget.style.boxShadow = '0 8px 16px rgba(0,0,0,0.1)'
                  e.currentTarget.style.borderColor = '#667eea'
                }}
                onMouseLeave={(e) => {
                  e.currentTarget.style.transform = 'translateY(0)'
                  e.currentTarget.style.boxShadow = '0 2px 8px rgba(0,0,0,0.05)'
                  e.currentTarget.style.borderColor = 'transparent'
                }}
              >
                <div style={{
                  display: 'grid',
                  gridTemplateColumns: '1fr auto',
                  gap: '30px',
                  alignItems: 'center'
                }}>
                  <div>
                    <h3 style={{
                      margin: '0 0 10px 0',
                      fontSize: '20px',
                      color: '#333',
                      fontWeight: '600'
                    }}>
                      {analysis.upload_filename}
                    </h3>

                    <div style={{
                      display: 'flex',
                      gap: '20px',
                      marginBottom: '15px'
                    }}>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '13px', color: '#666' }}>
                        <Calendar size={14} />
                        {new Date(analysis.analysis_date).toLocaleDateString()}
                      </div>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '13px', color: '#666' }}>
                        <FileText size={14} />
                        {analysis.total_files} files
                      </div>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '13px', color: '#666' }}>
                        <BarChart2 size={14} />
                        {analysis.total_loc.toLocaleString()} LOC
                      </div>
                    </div>

                    <span style={{
                      padding: '4px 12px',
                      backgroundColor: analysis.status === 'completed' ? '#e6f4ea' : '#fff3e0',
                      color: analysis.status === 'completed' ? '#188038' : '#e65100',
                      borderRadius: '12px',
                      fontSize: '12px',
                      fontWeight: 'bold'
                    }}>
                      {analysis.status}
                    </span>
                  </div>

                  <div>
                    <button
                      style={{
                        padding: '10px 20px',
                        fontSize: '14px',
                        fontWeight: 'bold',
                        color: '#667eea',
                        backgroundColor: '#f0f3ff',
                        border: 'none',
                        borderRadius: '6px',
                        cursor: 'pointer'
                      }}
                    >
                      View Details →
                    </button>
                  </div>
                </div>
              </div>
            ))}
          </div>
        )}
      </main>
    </div>
  )
}

export default SavedAnalysisPage
