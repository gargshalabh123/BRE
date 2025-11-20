import React, { useState, useEffect } from 'react'
import { useNavigate } from 'react-router-dom'
import { Upload, FileSearch, LogOut, User, FolderOpen, History, BarChart2, Database, FileCode, Sparkles, GitBranch, AlertCircle } from 'lucide-react'

const HomePage: React.FC = () => {
  const navigate = useNavigate()
  const [username, setUsername] = useState('')

  useEffect(() => {
    // Check authentication
    const isAuth = sessionStorage.getItem('isAuthenticated')
    if (!isAuth) {
      navigate('/login')
      return
    }

    const storedUsername = sessionStorage.getItem('username')
    setUsername(storedUsername || 'User')
  }, [navigate])

  const handleLogout = () => {
    sessionStorage.clear()
    navigate('/login')
  }

  const handleCreateAnalysis = () => {
    navigate('/upload')
  }

  const handleViewAnalysis = () => {
    navigate('/saved-analysis')
  }

  return (
    <div style={{ minHeight: '100vh', backgroundColor: '#f5f7fa' }}>
      {/* Header */}
      <header style={{
        backgroundColor: '#fff',
        borderBottom: '1px solid #e0e0e0',
        padding: '15px 30px',
        boxShadow: '0 2px 4px rgba(0,0,0,0.05)'
      }}>
        <div style={{
          maxWidth: '1400px',
          margin: '0 auto',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center'
        }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: '15px' }}>
            <div style={{
              width: '50px',
              height: '50px',
              background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
              borderRadius: '10px',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center'
            }}>
              <FolderOpen size={28} color="#fff" />
            </div>
            <div>
              <h1 style={{ margin: 0, fontSize: '24px', color: '#333' }}>
                Business Rules Extraction
              </h1>
              <p style={{ margin: 0, fontSize: '13px', color: '#666' }}>
                Mainframe Code Analysis Platform
              </p>
            </div>
          </div>

          <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
              <User size={20} color="#667eea" />
              <span style={{ fontSize: '14px', color: '#333', fontWeight: 'bold' }}>
                {username}
              </span>
            </div>
            <button
              onClick={handleLogout}
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
                transition: 'all 0.2s'
              }}
              onMouseEnter={(e) => {
                e.currentTarget.style.backgroundColor = '#f8f9fa'
                e.currentTarget.style.borderColor = '#667eea'
                e.currentTarget.style.color = '#667eea'
              }}
              onMouseLeave={(e) => {
                e.currentTarget.style.backgroundColor = '#fff'
                e.currentTarget.style.borderColor = '#ddd'
                e.currentTarget.style.color = '#666'
              }}
            >
              <LogOut size={16} />
              Logout
            </button>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main style={{
        maxWidth: '1400px',
        margin: '0 auto',
        padding: '60px 30px'
      }}>
        {/* Welcome Section */}
        <div style={{ marginBottom: '50px', textAlign: 'center' }}>
          <h2 style={{
            fontSize: '32px',
            margin: '0 0 15px 0',
            color: '#333',
            fontWeight: '600'
          }}>
            Welcome back, {username}!
          </h2>
          <p style={{
            fontSize: '16px',
            color: '#666',
            margin: 0
          }}>
            What would you like to do today?
          </p>
        </div>

        {/* Action Tiles */}
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(400px, 1fr))',
          gap: '30px',
          maxWidth: '1000px',
          margin: '0 auto'
        }}>
          {/* Create New Analysis Tile */}
          <div
            onClick={handleCreateAnalysis}
            style={{
              backgroundColor: '#fff',
              borderRadius: '16px',
              padding: '40px',
              boxShadow: '0 4px 12px rgba(0,0,0,0.08)',
              cursor: 'pointer',
              transition: 'all 0.3s ease',
              border: '2px solid transparent',
              position: 'relative',
              overflow: 'hidden'
            }}
            onMouseEnter={(e) => {
              e.currentTarget.style.transform = 'translateY(-8px)'
              e.currentTarget.style.boxShadow = '0 12px 24px rgba(102, 126, 234, 0.2)'
              e.currentTarget.style.borderColor = '#667eea'
            }}
            onMouseLeave={(e) => {
              e.currentTarget.style.transform = 'translateY(0)'
              e.currentTarget.style.boxShadow = '0 4px 12px rgba(0,0,0,0.08)'
              e.currentTarget.style.borderColor = 'transparent'
            }}
          >
            {/* Gradient Background */}
            <div style={{
              position: 'absolute',
              top: 0,
              right: 0,
              width: '150px',
              height: '150px',
              background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
              opacity: 0.1,
              borderRadius: '0 16px 0 100%'
            }} />

            <div style={{ position: 'relative', zIndex: 1 }}>
              <div style={{
                width: '80px',
                height: '80px',
                background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
                borderRadius: '16px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '20px',
                boxShadow: '0 8px 16px rgba(102, 126, 234, 0.3)'
              }}>
                <Upload size={40} color="#fff" />
              </div>

              <h3 style={{
                margin: '0 0 12px 0',
                fontSize: '24px',
                color: '#333',
                fontWeight: '600'
              }}>
                Create New Analysis
              </h3>

              <p style={{
                margin: '0 0 20px 0',
                fontSize: '14px',
                color: '#666',
                lineHeight: '1.6'
              }}>
                Upload a COBOL codebase (ZIP file) and perform comprehensive analysis including dependencies, business rules, and database operations.
              </p>

              <div style={{
                display: 'flex',
                gap: '10px',
                flexWrap: 'wrap'
              }}>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#e8f0fe',
                  color: '#1967d2',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  Code Metrics
                </span>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#fce8e6',
                  color: '#d93025',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  Dependencies
                </span>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#e6f4ea',
                  color: '#188038',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  Business Rules
                </span>
              </div>
            </div>
          </div>

          {/* View Saved Analysis Tile */}
          <div
            onClick={handleViewAnalysis}
            style={{
              backgroundColor: '#fff',
              borderRadius: '16px',
              padding: '40px',
              boxShadow: '0 4px 12px rgba(0,0,0,0.08)',
              cursor: 'pointer',
              transition: 'all 0.3s ease',
              border: '2px solid transparent',
              position: 'relative',
              overflow: 'hidden'
            }}
            onMouseEnter={(e) => {
              e.currentTarget.style.transform = 'translateY(-8px)'
              e.currentTarget.style.boxShadow = '0 12px 24px rgba(118, 75, 162, 0.2)'
              e.currentTarget.style.borderColor = '#764ba2'
            }}
            onMouseLeave={(e) => {
              e.currentTarget.style.transform = 'translateY(0)'
              e.currentTarget.style.boxShadow = '0 4px 12px rgba(0,0,0,0.08)'
              e.currentTarget.style.borderColor = 'transparent'
            }}
          >
            {/* Gradient Background */}
            <div style={{
              position: 'absolute',
              top: 0,
              right: 0,
              width: '150px',
              height: '150px',
              background: 'linear-gradient(135deg, #764ba2 0%, #667eea 100%)',
              opacity: 0.1,
              borderRadius: '0 16px 0 100%'
            }} />

            <div style={{ position: 'relative', zIndex: 1 }}>
              <div style={{
                width: '80px',
                height: '80px',
                background: 'linear-gradient(135deg, #764ba2 0%, #667eea 100%)',
                borderRadius: '16px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '20px',
                boxShadow: '0 8px 16px rgba(118, 75, 162, 0.3)'
              }}>
                <FileSearch size={40} color="#fff" />
              </div>

              <h3 style={{
                margin: '0 0 12px 0',
                fontSize: '24px',
                color: '#333',
                fontWeight: '600'
              }}>
                View Saved Analysis
              </h3>

              <p style={{
                margin: '0 0 20px 0',
                fontSize: '14px',
                color: '#666',
                lineHeight: '1.6'
              }}>
                Browse previously analyzed codebases, compare different versions, and export results. View detailed metrics, dependencies, and insights.
              </p>

              <div style={{
                display: 'flex',
                gap: '10px',
                flexWrap: 'wrap'
              }}>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#f3e8ff',
                  color: '#7b1fa2',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  <History size={12} style={{ display: 'inline', marginRight: '4px' }} />
                  History
                </span>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#fff3e0',
                  color: '#e65100',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  Compare
                </span>
                <span style={{
                  padding: '4px 12px',
                  backgroundColor: '#e0f7fa',
                  color: '#006064',
                  borderRadius: '12px',
                  fontSize: '12px',
                  fontWeight: 'bold'
                }}>
                  Export
                </span>
              </div>
            </div>
          </div>
        </div>

        {/* Platform Features Section */}
        <div style={{
          marginTop: '60px',
          maxWidth: '1000px',
          margin: '60px auto 0'
        }}>
          <h3 style={{
            fontSize: '28px',
            margin: '0 0 30px 0',
            color: '#333',
            fontWeight: '600',
            textAlign: 'center'
          }}>
            Comprehensive Code Analysis
          </h3>

          <div style={{
            display: 'grid',
            gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))',
            gap: '20px'
          }}>
            {/* Static Code Analysis */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#e8f0fe',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <BarChart2 size={28} color="#1967d2" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                Static Code Analysis
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                LOC counting, complexity metrics, file type detection, and SLOC analysis for comprehensive code quality assessment.
              </p>
            </div>

            {/* Dependency Extraction */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#fce8e6',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <GitBranch size={28} color="#d93025" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                Dependency Extraction
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                Extract and visualize program calls, copybooks, CICS, IMS, and BMS dependencies with detailed parameter tracking.
              </p>
            </div>

            {/* Database Operations */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#e6f4ea',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <Database size={28} color="#188038" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                Database Operations
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                Identify SQL queries, IMS DL/I operations, CICS commands, and track database access patterns with parameter extraction.
              </p>
            </div>

            {/* Business Rules */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#fff3e0',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <AlertCircle size={28} color="#e65100" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                Business Rules
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                Extract 88-level conditions, IF-THEN logic, EVALUATE statements, and identify business validation rules.
              </p>
            </div>

            {/* AI-Powered Insights */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#f3e8ff',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <Sparkles size={28} color="#7b1fa2" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                AI-Powered Insights
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                Generate code explanations, architectural insights, and modernization suggestions using advanced AI models.
              </p>
            </div>

            {/* Code Visualization */}
            <div style={{
              backgroundColor: '#fff',
              borderRadius: '12px',
              padding: '25px',
              boxShadow: '0 2px 8px rgba(0,0,0,0.05)',
              border: '1px solid #e0e0e0'
            }}>
              <div style={{
                width: '50px',
                height: '50px',
                backgroundColor: '#e0f7fa',
                borderRadius: '10px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                marginBottom: '15px'
              }}>
                <FileCode size={28} color="#006064" />
              </div>
              <h4 style={{ margin: '0 0 10px 0', fontSize: '18px', color: '#333', fontWeight: '600' }}>
                Code Visualization
              </h4>
              <p style={{ margin: 0, fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
                Interactive charts, dependency graphs, file tree viewer, and syntax-highlighted code display with copy and export.
              </p>
            </div>
          </div>

          {/* Technology Stack Info */}
          <div style={{
            marginTop: '40px',
            padding: '25px',
            backgroundColor: '#f8f9fa',
            borderRadius: '12px',
            border: '1px solid #e0e0e0'
          }}>
            <div style={{
              display: 'grid',
              gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
              gap: '20px',
              textAlign: 'center'
            }}>
              <div>
                <div style={{ fontSize: '14px', color: '#666', marginBottom: '8px', fontWeight: '500' }}>
                  Supported Languages
                </div>
                <div style={{ fontSize: '18px', color: '#667eea', fontWeight: 'bold' }}>
                  COBOL, SQL, JCL, RPG
                </div>
              </div>

              <div>
                <div style={{ fontSize: '14px', color: '#666', marginBottom: '8px', fontWeight: '500' }}>
                  Database Storage
                </div>
                <div style={{ fontSize: '18px', color: '#764ba2', fontWeight: 'bold' }}>
                  SQLite with Caching
                </div>
              </div>

              <div>
                <div style={{ fontSize: '14px', color: '#666', marginBottom: '8px', fontWeight: '500' }}>
                  Export Formats
                </div>
                <div style={{ fontSize: '18px', color: '#00897b', fontWeight: 'bold' }}>
                  JSON, CSV
                </div>
              </div>

              <div>
                <div style={{ fontSize: '14px', color: '#666', marginBottom: '8px', fontWeight: '500' }}>
                  Analysis Speed
                </div>
                <div style={{ fontSize: '18px', color: '#e65100', fontWeight: 'bold' }}>
                  10-20x Faster (Cached)
                </div>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  )
}

export default HomePage
