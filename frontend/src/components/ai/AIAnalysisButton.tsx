import React, { useState } from 'react'
import { Sparkles } from 'lucide-react'
import api, { AIAnalysisResponse } from '../../services/api'
import AIAnalysisModal from './AIAnalysisModal'
import { useAIProvider } from '../../contexts/AIProviderContext'

interface AIAnalysisButtonProps {
  uploadId: string
  itemType: 'file' | 'rule' | 'dependency' | 'query'
  itemData: any
  variant?: 'button' | 'icon'
  className?: string
}

const AIAnalysisButton: React.FC<AIAnalysisButtonProps> = ({
  uploadId,
  itemType,
  itemData,
  variant = 'button',
  className = ''
}) => {
  const { selectedProvider } = useAIProvider()
  const [loading, setLoading] = useState(false)
  const [analysis, setAnalysis] = useState<AIAnalysisResponse | null>(null)
  const [modalOpen, setModalOpen] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const handleAnalyze = async (e: React.MouseEvent) => {
    e.stopPropagation() // Prevent row click if in table

    setLoading(true)
    setError(null)

    try {
      let response: AIAnalysisResponse

      switch (itemType) {
        case 'file':
          response = await api.analyzeFile({
            upload_id: uploadId,
            file: itemData.file,
            loc: itemData.loc,
            sloc: itemData.sloc,
            complexity: itemData.complexity,
            comments: itemData.comments,
            blank: itemData.blank,
            functions: itemData.functions,
            provider: selectedProvider
          })
          break

        case 'rule':
          response = await api.analyzeRule({
            upload_id: uploadId,
            file: itemData.file,
            line: itemData.line,
            type: itemData.type,
            code: itemData.code || '',
            description: itemData.description,
            provider: selectedProvider
          })
          break

        case 'dependency':
          response = await api.analyzeDependency({
            upload_id: uploadId,
            file: itemData.file,
            type: itemData.type,
            target: itemData.target,
            line: itemData.line,
            lines: itemData.lines,
            signature: itemData.signature,
            parameters: itemData.parameters,
            description: itemData.description,
            provider: selectedProvider
          })
          break

        case 'query':
          response = await api.analyzeQuery({
            upload_id: uploadId,
            file: itemData.file,
            line: itemData.line,
            type: itemData.type,
            query: itemData.query,
            category: itemData.category,
            provider: selectedProvider
          })
          break

        default:
          throw new Error(`Unknown item type: ${itemType}`)
      }

      setAnalysis(response)
      setModalOpen(true)
    } catch (err: any) {
      console.error('Analysis failed:', err)

      // Handle different error formats
      let errorMessage = 'Analysis failed'
      if (err.response?.data?.detail) {
        // If detail is an array or object, stringify it
        if (typeof err.response.data.detail === 'string') {
          errorMessage = err.response.data.detail
        } else {
          errorMessage = JSON.stringify(err.response.data.detail)
        }
      } else if (err.message) {
        errorMessage = err.message
      }

      setError(errorMessage)
    } finally {
      setLoading(false)
    }
  }

  if (variant === 'icon') {
    return (
      <>
        <button
          onClick={handleAnalyze}
          disabled={loading}
          className={`inline-flex items-center justify-center w-8 h-8 rounded hover:bg-gray-100 transition-colors ${
            loading ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'
          } ${className}`}
          title="AI Analysis"
        >
          {loading ? (
            <svg className="animate-spin h-4 w-4 text-blue-600" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
              <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
              <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
          ) : (
            <Sparkles className="w-4 h-4 text-blue-600" />
          )}
        </button>

        {error && (
          <div className="absolute z-10 mt-1 p-2 bg-red-100 border border-red-300 rounded text-xs text-red-700">
            {error}
          </div>
        )}

        {analysis && (
          <AIAnalysisModal
            open={modalOpen}
            onClose={() => setModalOpen(false)}
            analysis={analysis}
            itemType={itemType}
            itemData={itemData}
          />
        )}
      </>
    )
  }

  return (
    <>
      <button
        onClick={handleAnalyze}
        disabled={loading}
        className="button"
        style={{
          padding: '6px 12px',
          display: 'flex',
          alignItems: 'center',
          gap: '6px',
          backgroundColor: loading ? '#9ca3af' : '#667eea',
          color: '#fff',
          border: 'none',
          fontSize: '0.85em'
        }}
      >
        {loading ? (
          <>
            <svg
              style={{
                animation: 'spin 1s linear infinite',
                width: '14px',
                height: '14px'
              }}
              xmlns="http://www.w3.org/2000/svg"
              fill="none"
              viewBox="0 0 24 24"
            >
              <circle
                style={{ opacity: 0.25 }}
                cx="12"
                cy="12"
                r="10"
                stroke="currentColor"
                strokeWidth="4"
              />
              <path
                style={{ opacity: 0.75 }}
                fill="currentColor"
                d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
              />
            </svg>
            <style>{`
              @keyframes spin {
                from { transform: rotate(0deg); }
                to { transform: rotate(360deg); }
              }
            `}</style>
            AI
          </>
        ) : (
          <>
            <Sparkles size={14} /> AI
          </>
        )}
      </button>

      {error && (
        <div className="mt-2 p-2 bg-red-100 border border-red-300 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {analysis && (
        <AIAnalysisModal
          open={modalOpen}
          onClose={() => setModalOpen(false)}
          analysis={analysis}
          itemType={itemType}
          itemData={itemData}
        />
      )}
    </>
  )
}

export default AIAnalysisButton
