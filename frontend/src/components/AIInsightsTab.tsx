import React, { useState } from 'react'
import ReactMarkdown from 'react-markdown'
import { Loader, Sparkles } from 'lucide-react'
import api, { AnalysisResults } from '../services/api'

interface Props {
  uploadId: string
  data: AnalysisResults
}

const AIInsightsTab: React.FC<Props> = ({ uploadId, data }) => {
  const [loading, setLoading] = useState(false)
  const [summary, setSummary] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [provider, setProvider] = useState<'openai' | 'anthropic'>('anthropic')

  const generateSummary = async () => {
    setLoading(true)
    setError(null)

    try {
      const response = await api.summarizeCodebase(uploadId, provider)
      setSummary(response.summary)
    } catch (err: any) {
      setError(err.response?.data?.detail || 'Failed to generate AI summary')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>AI-Powered Insights</h2>

      <div className="card" style={{ background: '#f8f9ff', border: '1px solid #667eea' }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginBottom: '15px' }}>
          <Sparkles size={24} color="#667eea" />
          <h3>Generate Codebase Summary</h3>
        </div>
        <p style={{ color: '#666', marginBottom: '15px' }}>
          Use AI to generate a comprehensive analysis of your codebase, including architecture assessment,
          technology stack identification, and modernization recommendations.
        </p>

        <div style={{ display: 'flex', gap: '10px', alignItems: 'center' }}>
          <select
            value={provider}
            onChange={(e) => setProvider(e.target.value as 'openai' | 'anthropic')}
            style={{
              padding: '10px',
              borderRadius: '4px',
              border: '1px solid #ddd'
            }}
          >
            <option value="anthropic">Anthropic Claude</option>
            <option value="openai">OpenAI GPT-4</option>
          </select>

          <button
            className="button"
            onClick={generateSummary}
            disabled={loading}
          >
            {loading ? (
              <>
                <Loader size={16} className="loading-spinner" /> Generating...
              </>
            ) : (
              <>
                <Sparkles size={16} /> Generate Summary
              </>
            )}
          </button>
        </div>
      </div>

      {error && (
        <div className="error" style={{ marginTop: '20px' }}>
          <strong>Error:</strong> {error}
        </div>
      )}

      {summary && (
        <div className="card" style={{ marginTop: '20px' }}>
          <h3 style={{ marginBottom: '15px', display: 'flex', alignItems: 'center', gap: '10px' }}>
            <Sparkles size={20} color="#667eea" />
            AI-Generated Analysis
          </h3>
          <div style={{
            background: '#fff',
            padding: '20px',
            borderRadius: '6px',
            border: '1px solid #e0e0e0',
            lineHeight: '1.8'
          }}>
            <ReactMarkdown>{summary}</ReactMarkdown>
          </div>
        </div>
      )}

      <div className="card" style={{ marginTop: '20px' }}>
        <h3 style={{ marginBottom: '15px' }}>Quick Statistics for AI Context</h3>
        <div style={{ background: '#f9f9f9', padding: '15px', borderRadius: '6px' }}>
          <pre style={{ margin: 0, fontSize: '0.9em', lineHeight: '1.6' }}>
            {JSON.stringify({
              total_files: data.summary?.total_files || data.metrics?.by_file?.length || 0,
              file_types: data.summary?.file_types || {},
              total_loc: data.metrics?.total_loc || 0,
              total_sloc: data.metrics?.total_sloc || 0,
              database_operations: data.database_operations?.total_count || 0,
              business_rules: data.business_rules?.length || 0,
              dependencies_count: Object.keys(data.dependencies || {}).length
            }, null, 2)}
          </pre>
        </div>
      </div>
    </div>
  )
}

export default AIInsightsTab
