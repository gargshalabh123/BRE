import React, { useState, useEffect } from 'react'
import ReactMarkdown from 'react-markdown'
import { Loader, Brain, ChevronDown, ChevronRight, Play, CheckCircle, AlertCircle } from 'lucide-react'
import api, { AnalysisResults, AgenticAnalysisResponse } from '../services/api'
import { useAIProvider } from '../contexts/AIProviderContext'

interface Props {
  uploadId: string
  data: AnalysisResults
}

const AgenticAnalysisTab: React.FC<Props> = ({ uploadId, data }) => {
  const [query, setQuery] = useState('')
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState<AgenticAnalysisResponse | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [selectedAgent, setSelectedAgent] = useState<string>('auto')
  const [expandedIterations, setExpandedIterations] = useState<Set<number>>(new Set())
  const [availableAgents, setAvailableAgents] = useState<any>(null)
  const [showReasoningTrace, setShowReasoningTrace] = useState(false)
  const { selectedProvider, setSelectedProvider, availableProviders, setAvailableProviders } = useAIProvider()
  const [loadingProviders, setLoadingProviders] = useState(true)

  // Predefined queries
  const exampleQueries = [
    { label: "Security Analysis", query: "Find all security vulnerabilities in this codebase", agent: "security" },
    { label: "Architecture Overview", query: "Explain the architecture of this system", agent: "architecture" },
    { label: "SQL Injection Check", query: "Check for SQL injection vulnerabilities", agent: "security" },
    { label: "Complexity Hotspots", query: "Find the most complex files and explain why", agent: "general" },
    { label: "Business Logic", query: "Identify and explain the main business logic", agent: "general" }
  ]

  // Fetch available providers on mount
  useEffect(() => {
    const fetchProviders = async () => {
      try {
        const data = await api.getAvailableProviders()
        setAvailableProviders(data)
      } catch (err) {
        console.error('Failed to fetch AI providers:', err)
      } finally {
        setLoadingProviders(false)
      }
    }
    fetchProviders()
  }, [])

  // Load available agents
  useEffect(() => {
    const fetchAgents = async () => {
      try {
        const agents = await api.getAvailableAgents()
        setAvailableAgents(agents)
      } catch (err) {
        console.error('Failed to fetch agents:', err)
      }
    }
    fetchAgents()
  }, [])

  const handleAnalyze = async () => {
    if (!query.trim()) {
      setError('Please enter a query')
      return
    }

    setLoading(true)
    setError(null)
    setResult(null)
    setExpandedIterations(new Set())

    try {
      const agentType = selectedAgent === 'auto' ? undefined : selectedAgent
      const response = await api.agenticAnalyze(uploadId, query, agentType, selectedProvider)
      setResult(response)

      // Keep reasoning trace collapsed by default
      setExpandedIterations(new Set())
      setShowReasoningTrace(false)
    } catch (err: any) {
      setError(err.response?.data?.detail || 'Analysis failed')
    } finally {
      setLoading(false)
    }
  }

  const toggleIteration = (iteration: number) => {
    const newExpanded = new Set(expandedIterations)
    if (newExpanded.has(iteration)) {
      newExpanded.delete(iteration)
    } else {
      newExpanded.add(iteration)
    }
    setExpandedIterations(newExpanded)
  }

  const useExampleQuery = (exampleQuery: string, agent: string) => {
    setQuery(exampleQuery)
    setSelectedAgent(agent)
  }

  return (
    <div>
      <h2 style={{ marginBottom: '20px' }}>AI-Powered Insights</h2>

      <div className="card" style={{ background: '#f0f7ff', border: '2px solid #667eea', marginBottom: '20px' }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginBottom: '15px' }}>
          <Brain size={28} color="#667eea" />
          <div>
            <h3 style={{ margin: 0 }}>AI Agents with Reasoning</h3>
            <p style={{ margin: '5px 0 0 0', fontSize: '0.9em', color: '#666' }}>
              Agents use tools to explore your codebase systematically and answer questions
            </p>
          </div>
        </div>

        {/* Example Queries */}
        <div style={{ marginBottom: '15px' }}>
          <label style={{ display: 'block', marginBottom: '8px', fontWeight: 'bold' }}>
            Try an example:
          </label>
          <div style={{ display: 'flex', gap: '8px', flexWrap: 'wrap' }}>
            {exampleQueries.map((example, idx) => (
              <button
                key={idx}
                className="button"
                onClick={() => useExampleQuery(example.query, example.agent)}
                style={{
                  padding: '6px 12px',
                  fontSize: '0.85em',
                  background: '#fff',
                  border: '1px solid #667eea',
                  color: '#667eea'
                }}
              >
                {example.label}
              </button>
            ))}
          </div>
        </div>

        {/* Query Input */}
        <div style={{ marginBottom: '15px' }}>
          <label htmlFor="query" style={{ display: 'block', marginBottom: '8px', fontWeight: 'bold' }}>
            Your Question:
          </label>
          <textarea
            id="query"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            placeholder="Ask anything about the codebase (e.g., 'Find security vulnerabilities', 'Explain the architecture', 'What business rules are implemented?')"
            rows={3}
            style={{
              width: '100%',
              padding: '10px',
              borderRadius: '4px',
              border: '1px solid #ddd',
              fontFamily: 'inherit',
              fontSize: '0.95em'
            }}
          />
        </div>

        {/* Agent and Model Selection */}
        <div style={{ display: 'flex', gap: '15px', alignItems: 'flex-end', marginBottom: '15px' }}>
          <div style={{ flex: 1 }}>
            <label style={{ display: 'block', marginBottom: '5px', fontSize: '0.9em', fontWeight: 'bold' }}>
              Agent Type:
            </label>
            <select
              value={selectedAgent}
              onChange={(e) => setSelectedAgent(e.target.value)}
              style={{
                padding: '8px',
                borderRadius: '4px',
                border: '1px solid #ddd',
                width: '100%'
              }}
            >
              <option value="auto">Auto (Smart Routing)</option>
              <option value="security">Security Agent</option>
              <option value="architecture">Architecture Agent</option>
              <option value="general">General Agent</option>
            </select>
          </div>

          <div style={{ flex: 1 }}>
            <label style={{ display: 'block', marginBottom: '5px', fontSize: '0.9em', fontWeight: 'bold' }}>
              AI Model:
            </label>
            <select
              value={selectedProvider}
              onChange={(e) => setSelectedProvider(e.target.value)}
              disabled={loadingProviders}
              style={{
                padding: '8px',
                borderRadius: '4px',
                border: '1px solid #ddd',
                width: '100%'
              }}
            >
              <option value="gemini">
                {availableProviders?.providers?.gemini?.available ? '✓ ' : '✗ '}
                Google Gemini
              </option>
              <option value="anthropic">
                {availableProviders?.providers?.anthropic?.available ? '✓ ' : '✗ '}
                Anthropic Claude
              </option>
              <option value="openai">
                {availableProviders?.providers?.openai?.available ? '✓ ' : '✗ '}
                OpenAI GPT-4
              </option>
            </select>
          </div>

          <button
            className="button"
            onClick={handleAnalyze}
            disabled={loading}
            style={{
              padding: '10px 24px'
            }}
          >
            {loading ? (
              <>
                <Loader size={16} className="loading-spinner" /> Analyzing...
              </>
            ) : (
              <>
                <Play size={16} /> Analyze
              </>
            )}
          </button>
        </div>

        {availableAgents && (
          <div style={{ fontSize: '0.85em', color: '#666', marginTop: '10px' }}>
            <strong>Available Agents:</strong>{' '}
            {Object.entries(availableAgents.agents || {}).map(([key, agent]: [string, any]) => agent.name).join(', ')}
          </div>
        )}
      </div>

      {/* Error Display */}
      {error && (
        <div className="error" style={{ marginBottom: '20px' }}>
          <AlertCircle size={18} /> <strong>Error:</strong> {error}
        </div>
      )}

      {/* Results */}
      {result && (
        <div>
          {/* Final Answer */}
          <div className="card" style={{ marginBottom: '20px', background: '#f8fff8', border: '2px solid #48bb78' }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginBottom: '15px' }}>
              <CheckCircle size={24} color="#48bb78" />
              <h3 style={{ margin: 0 }}>Analysis Result</h3>
            </div>

            <div style={{ background: '#fff', padding: '20px', borderRadius: '6px', border: '1px solid #e0e0e0' }}>
              <ReactMarkdown>{result.answer}</ReactMarkdown>
            </div>

            <div style={{ marginTop: '15px', fontSize: '0.85em', color: '#666' }}>
              <strong>Agent:</strong> {result.agent_type || 'Auto-routed'} |
              <strong> Model:</strong> {result.provider} ({result.model}) |
              <strong> Iterations:</strong> {result.iterations_used} |
              <strong> Status:</strong> {result.status}
            </div>
          </div>

          {/* Reasoning Trace - Collapsible */}
          <div className="card">
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
              <div>
                <h3 style={{ margin: 0 }}>Reasoning Trace</h3>
                <p style={{ color: '#666', fontSize: '0.85em', margin: '5px 0 0 0' }}>
                  {showReasoningTrace ? 'Hide' : 'Show'} how the agent reasoned step-by-step
                </p>
              </div>
              <button
                className="button button-secondary"
                onClick={() => setShowReasoningTrace(!showReasoningTrace)}
                style={{ padding: '8px 16px' }}
              >
                {showReasoningTrace ? <ChevronDown size={16} /> : <ChevronRight size={16} />}
                {showReasoningTrace ? 'Hide Trace' : 'Show Trace'}
              </button>
            </div>

            {showReasoningTrace && result.reasoning_trace.map((step, idx) => {
              if (step.iteration) {
                const isExpanded = expandedIterations.has(step.iteration)
                return (
                  <div key={idx} style={{ marginBottom: '10px', border: '1px solid #ddd', borderRadius: '6px' }}>
                    <div
                      onClick={() => toggleIteration(step.iteration!)}
                      style={{
                        padding: '12px',
                        background: '#f9f9f9',
                        cursor: 'pointer',
                        display: 'flex',
                        alignItems: 'center',
                        gap: '10px',
                        borderRadius: '6px 6px 0 0'
                      }}
                    >
                      {isExpanded ? <ChevronDown size={18} /> : <ChevronRight size={18} />}
                      <strong>Iteration {step.iteration}</strong>
                      {step.thought && (
                        <span style={{ color: '#666', fontSize: '0.9em', marginLeft: '10px' }}>
                          {step.thought.substring(0, 80)}...
                        </span>
                      )}
                    </div>

                    {isExpanded && (
                      <div style={{ padding: '15px', background: '#fff' }}>
                        {step.thought && (
                          <div style={{ marginBottom: '12px' }}>
                            <strong style={{ color: '#667eea' }}>💭 Thought:</strong>
                            <p style={{ marginTop: '5px', marginBottom: '0', paddingLeft: '20px' }}>
                              {step.thought}
                            </p>
                          </div>
                        )}

                        {step.action && (
                          <div style={{ marginBottom: '12px' }}>
                            <strong style={{ color: '#48bb78' }}>⚡ Action:</strong>
                            <pre style={{
                              marginTop: '5px',
                              padding: '10px',
                              background: '#f5f5f5',
                              borderRadius: '4px',
                              fontSize: '0.85em',
                              overflow: 'auto'
                            }}>
                              {step.action.type === 'tool'
                                ? `${step.action.name}(${step.action.params_str || ''})`
                                : step.action.type === 'FINISH'
                                ? 'FINISH'
                                : JSON.stringify(step.action, null, 2)}
                            </pre>
                          </div>
                        )}

                        {/* Next observation */}
                        {result.reasoning_trace[idx + 1]?.observation && (
                          <div>
                            <strong style={{ color: '#f59e0b' }}>👁️ Observation:</strong>
                            <pre style={{
                              marginTop: '5px',
                              padding: '10px',
                              background: '#fffbeb',
                              borderRadius: '4px',
                              fontSize: '0.85em',
                              overflow: 'auto',
                              maxHeight: '300px'
                            }}>
                              {typeof result.reasoning_trace[idx + 1].observation === 'string'
                                ? result.reasoning_trace[idx + 1].observation
                                : JSON.stringify(result.reasoning_trace[idx + 1].observation, null, 2)}
                            </pre>
                          </div>
                        )}
                      </div>
                    )}
                  </div>
                )
              }
              return null
            })}

            {!result.reasoning_trace.some((step: any) => step.iteration) && (
              <p style={{ color: '#999', fontStyle: 'italic', textAlign: 'center', padding: '20px' }}>
                No reasoning trace available
              </p>
            )}
          </div>
        </div>
      )}

      {/* Info Card */}
      <div className="card" style={{ marginTop: '20px', background: '#fef3c7' }}>
        <h4>How Agentic Analysis Works:</h4>
        <ul style={{ marginLeft: '20px', lineHeight: '1.8' }}>
          <li><strong>Reasoning:</strong> Agent thinks step-by-step about what to do</li>
          <li><strong>Tool Use:</strong> Agent uses specialized tools to explore your code</li>
          <li><strong>Iteration:</strong> Agent refines its understanding through multiple steps</li>
          <li><strong>Context-Aware:</strong> Agent only accesses files that were analyzed</li>
        </ul>
      </div>
    </div>
  )
}

export default AgenticAnalysisTab
