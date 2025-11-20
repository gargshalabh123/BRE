import React, { useState, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { ArrowLeft, Download, Loader } from 'lucide-react'
import api, { AnalysisResults } from '../services/api'
import OverviewTab from '../components/OverviewTab'
import MetricsTab from '../components/MetricsTab'
import DependenciesTab from '../components/DependenciesTab'
import DatabaseTab from '../components/DatabaseTab'
import BusinessRulesTab from '../components/BusinessRulesTab'
import AIInsightsTab from '../components/AIInsightsTab'

type TabType = 'overview' | 'metrics' | 'dependencies' | 'database' | 'rules' | 'ai'

const AnalysisPage: React.FC = () => {
  const { uploadId } = useParams<{ uploadId: string }>()
  const navigate = useNavigate()
  const [activeTab, setActiveTab] = useState<TabType>('overview')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [analysisData, setAnalysisData] = useState<AnalysisResults | null>(null)

  useEffect(() => {
    // Check authentication first
    const isAuth = sessionStorage.getItem('isAuthenticated')
    if (!isAuth) {
      console.log('[AnalysisPage] Not authenticated, redirecting to login')
      navigate('/login')
      return
    }

    if (!uploadId) {
      console.log('[AnalysisPage] No uploadId provided')
      return
    }

    const loadAnalysis = async () => {
      try {
        console.log('[AnalysisPage] Loading analysis for uploadId:', uploadId)
        setLoading(true)
        const data = await api.analyzeFullCodebase(uploadId)
        console.log('[AnalysisPage] Analysis data loaded successfully')
        setAnalysisData(data)
        setError(null)
      } catch (err: any) {
        console.error('[AnalysisPage] Analysis failed:', err)
        setError(err.response?.data?.detail || 'Failed to analyze codebase')
      } finally {
        setLoading(false)
      }
    }

    loadAnalysis()
  }, [uploadId, navigate])

  const handleExport = () => {
    if (!analysisData) return

    const dataStr = JSON.stringify(analysisData, null, 2)
    const dataBlob = new Blob([dataStr], { type: 'application/json' })
    const url = URL.createObjectURL(dataBlob)
    const link = document.createElement('a')
    link.href = url
    link.download = `analysis-${uploadId}.json`
    link.click()
    URL.revokeObjectURL(url)
  }

  // Allow ZIP Explorer tab even while main analysis is loading
  const canShowTabs = uploadId !== undefined

  return (
    <div className="container">
      <div className="header">
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
          <div>
            <h1>Code Analysis Results</h1>
          </div>
          <div style={{ display: 'flex', gap: '10px' }}>
            <button className="button button-secondary" onClick={() => navigate('/home')}>
              <ArrowLeft size={16} /> Back to Home
            </button>
            <button className="button" onClick={handleExport}>
              <Download size={16} /> Export JSON
            </button>
          </div>
        </div>
      </div>

      <div className="card">
        <div className="tabs">
          <button
            className={`tab ${activeTab === 'overview' ? 'active' : ''}`}
            onClick={() => setActiveTab('overview')}
          >
            Overview
          </button>
          <button
            className={`tab ${activeTab === 'metrics' ? 'active' : ''}`}
            onClick={() => setActiveTab('metrics')}
          >
            Metrics
          </button>
          <button
            className={`tab ${activeTab === 'dependencies' ? 'active' : ''}`}
            onClick={() => setActiveTab('dependencies')}
          >
            Dependencies
          </button>
          <button
            className={`tab ${activeTab === 'database' ? 'active' : ''}`}
            onClick={() => setActiveTab('database')}
          >
            Database
          </button>
          <button
            className={`tab ${activeTab === 'rules' ? 'active' : ''}`}
            onClick={() => setActiveTab('rules')}
          >
            Business Rules
          </button>
          <button
            className={`tab ${activeTab === 'ai' ? 'active' : ''}`}
            onClick={() => setActiveTab('ai')}
          >
            AI Insights
          </button>
        </div>

        {activeTab === 'overview' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <OverviewTab data={analysisData} uploadId={uploadId!} />
          ) : null
        )}
        {activeTab === 'metrics' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <MetricsTab data={analysisData} />
          ) : null
        )}
        {activeTab === 'dependencies' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <DependenciesTab data={analysisData} uploadId={uploadId!} />
          ) : null
        )}
        {activeTab === 'database' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <DatabaseTab data={analysisData} />
          ) : null
        )}
        {activeTab === 'rules' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <BusinessRulesTab data={analysisData} />
          ) : null
        )}
        {activeTab === 'ai' && (
          loading ? (
            <div style={{ padding: '60px', textAlign: 'center' }}>
              <Loader className="loading-spinner" size={40} />
              <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
            </div>
          ) : error ? (
            <div className="error">{error}</div>
          ) : analysisData ? (
            <AIInsightsTab uploadId={uploadId!} data={analysisData} />
          ) : null
        )}
      </div>
    </div>
  )
}

export default AnalysisPage
