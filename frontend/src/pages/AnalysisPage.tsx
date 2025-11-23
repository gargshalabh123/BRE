import React, { useState, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { ArrowLeft, Download, Loader } from 'lucide-react'
import api, { AnalysisResults } from '../services/api'
import OverviewTab from '../components/OverviewTab'
import MetricsTab from '../components/MetricsTab'
import AgenticAnalysisTab from '../components/AgenticAnalysisTab'
import EnhancedDependenciesTab from '../components/EnhancedDependenciesTab'

type TabType = 'overview' | 'metrics' | 'ai' | 'enhanced'

const AnalysisPage: React.FC = () => {
  const { uploadId } = useParams<{ uploadId: string }>()
  const navigate = useNavigate()
  const [activeTab, setActiveTab] = useState<TabType>('overview')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [analysisData, setAnalysisData] = useState<AnalysisResults | null>(null)

  // Track if we're currently loading to prevent duplicate requests
  const loadingRef = React.useRef(false)
  const loadedUploadIdRef = React.useRef<string | null>(null)

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

    // Skip if already loaded this upload or currently loading
    if (loadingRef.current || loadedUploadIdRef.current === uploadId) {
      console.log('[AnalysisPage] Skipping duplicate load for uploadId:', uploadId)
      return
    }

    const loadAnalysis = async () => {
      try {
        loadingRef.current = true
        console.log('[AnalysisPage] Loading analysis for uploadId:', uploadId)
        setLoading(true)

        // Get selected extensions from sessionStorage if available
        const selectedExtensionsJson = sessionStorage.getItem(`selected_extensions_${uploadId}`)
        const selectedExtensions = selectedExtensionsJson ? JSON.parse(selectedExtensionsJson) : undefined

        console.log('[AnalysisPage] Selected extensions:', selectedExtensions)
        const data = await api.analyzeFullCodebase(uploadId, selectedExtensions)
        console.log('[AnalysisPage] Analysis data loaded successfully')
        setAnalysisData(data)
        setError(null)
        loadedUploadIdRef.current = uploadId
      } catch (err: any) {
        console.error('[AnalysisPage] Analysis failed:', err)
        setError(err.response?.data?.detail || 'Failed to analyze codebase')
      } finally {
        setLoading(false)
        loadingRef.current = false
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
            Details
          </button>
          <button
            className={`tab ${activeTab === 'enhanced' ? 'active' : ''}`}
            onClick={() => setActiveTab('enhanced')}
          >
            Analysis
          </button>
          <button
            className={`tab ${activeTab === 'ai' ? 'active' : ''}`}
            onClick={() => setActiveTab('ai')}
          >
            AI Insights
          </button>
        </div>

        {loading ? (
          <div style={{ padding: '60px', textAlign: 'center' }}>
            <Loader className="loading-spinner" size={40} />
            <p style={{ marginTop: '20px' }}>Analyzing codebase...</p>
          </div>
        ) : error ? (
          <div className="error">{error}</div>
        ) : analysisData ? (
          <>
            <div style={{ display: activeTab === 'overview' ? 'block' : 'none' }}>
              <OverviewTab data={analysisData} uploadId={uploadId!} />
            </div>
            <div style={{ display: activeTab === 'metrics' ? 'block' : 'none' }}>
              <MetricsTab data={analysisData} />
            </div>
            <div style={{ display: activeTab === 'enhanced' ? 'block' : 'none' }}>
              <EnhancedDependenciesTab uploadId={uploadId!} />
            </div>
            <div style={{ display: activeTab === 'ai' ? 'block' : 'none' }}>
              <AgenticAnalysisTab uploadId={uploadId!} data={analysisData} />
            </div>
          </>
        ) : null}
      </div>
    </div>
  )
}

export default AnalysisPage
