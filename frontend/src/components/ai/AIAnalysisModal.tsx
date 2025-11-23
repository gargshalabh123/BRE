import React, { useState, useEffect } from 'react'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { Copy, Check, Download, X } from 'lucide-react'
import { AIAnalysisResponse } from '../../services/api'

interface AIAnalysisModalProps {
  open: boolean
  onClose: () => void
  analysis: AIAnalysisResponse
  itemType: string
  itemData: any
}

const AIAnalysisModal: React.FC<AIAnalysisModalProps> = ({
  open,
  onClose,
  analysis,
  itemType,
  itemData
}) => {
  const [copied, setCopied] = useState(false)
  const [saved, setSaved] = useState(false)

  useEffect(() => {
    if (open) {
      document.body.style.overflow = 'hidden'
    } else {
      document.body.style.overflow = 'unset'
    }
    return () => {
      document.body.style.overflow = 'unset'
    }
  }, [open])

  if (!open) return null

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(analysis.analysis)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (err) {
      console.error('Failed to copy:', err)
    }
  }

  const handleSave = () => {
    // Save analysis to local storage as a simple implementation
    const savedAnalyses = JSON.parse(localStorage.getItem('saved_ai_analyses') || '[]')
    const newAnalysis = {
      id: Date.now(),
      timestamp: new Date().toISOString(),
      itemType,
      itemData: itemData.file || itemData.target || 'Unknown',
      analysis: analysis.analysis,
      provider: analysis.provider,
      model: analysis.model
    }
    savedAnalyses.push(newAnalysis)
    localStorage.setItem('saved_ai_analyses', JSON.stringify(savedAnalyses))

    setSaved(true)
    setTimeout(() => setSaved(false), 2000)
  }

  const getItemContext = () => {
    switch (itemType) {
      case 'file':
        return itemData.file
      case 'rule':
        return `${itemData.file}:${itemData.line}`
      case 'dependency':
        return `${itemData.target} (${itemData.type})`
      case 'query':
        return `${itemData.file}:${itemData.line} - ${itemData.type}`
      default:
        return 'Unknown'
    }
  }

  const getItemTypeLabel = () => {
    switch (itemType) {
      case 'file':
        return 'File Analysis'
      case 'rule':
        return 'Business Rule Analysis'
      case 'dependency':
        return 'Dependency Analysis'
      case 'query':
        return 'Database Query Analysis'
      default:
        return 'AI Analysis'
    }
  }

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: 'rgba(0, 0, 0, 0.5)',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        zIndex: 10000,
        padding: '20px'
      }}
      onClick={onClose}
    >
      <div
        style={{
          backgroundColor: 'white',
          borderRadius: '8px',
          maxWidth: '900px',
          width: '100%',
          maxHeight: '90vh',
          display: 'flex',
          flexDirection: 'column',
          boxShadow: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)'
        }}
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div style={{
          padding: '20px 24px',
          borderBottom: '1px solid #e5e7eb',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center'
        }}>
          <h3 style={{ margin: 0, fontSize: '18px', fontWeight: '500', color: '#111827' }}>
            AI Analysis
          </h3>
          <button
            onClick={onClose}
            style={{
              background: 'none',
              border: 'none',
              cursor: 'pointer',
              padding: '4px',
              color: '#9ca3af',
              display: 'flex',
              alignItems: 'center'
            }}
          >
            <X size={24} />
          </button>
        </div>

        {/* Debug: Show Prompt */}
        {analysis.prompt && (
          <details style={{
            backgroundColor: '#fef3c7',
            padding: '12px 24px',
            borderBottom: '1px solid #fde68a'
          }}>
            <summary style={{
              cursor: 'pointer',
              fontSize: '14px',
              fontWeight: '500',
              color: '#92400e'
            }}>
              🐛 Debug: View Prompt Sent to AI
            </summary>
            <pre style={{
              marginTop: '12px',
              padding: '12px',
              backgroundColor: 'white',
              border: '1px solid #fde68a',
              borderRadius: '4px',
              fontSize: '12px',
              overflowX: 'auto',
              whiteSpace: 'pre-wrap'
            }}>
              {analysis.prompt}
            </pre>
          </details>
        )}

        {/* Content */}
        <div style={{
          padding: '24px',
          overflowY: 'auto',
          flex: 1,
          backgroundColor: '#fafbfc',
          borderTop: '1px solid #e5e7eb',
          borderBottom: '1px solid #e5e7eb'
        }}>
          <div style={{
            maxWidth: '100%',
            backgroundColor: 'white',
            padding: '20px',
            borderRadius: '8px',
            border: '1px solid #e5e7eb',
            boxShadow: '0 1px 3px rgba(0, 0, 0, 0.1)'
          }}>
            <ReactMarkdown
              remarkPlugins={[remarkGfm]}
              components={{
                h1: ({ node, ...props }) => (
                  <div style={{ marginTop: '20px', marginBottom: '12px' }}>
                    <h1 style={{ fontSize: '20px', fontWeight: 'bold', color: '#111827', margin: 0, display: 'block', borderBottom: '2px solid #667eea', paddingBottom: '8px' }} {...props} />
                  </div>
                ),
                h2: ({ node, ...props }) => (
                  <div style={{ marginTop: '18px', marginBottom: '10px' }}>
                    <h2 style={{ fontSize: '18px', fontWeight: 'bold', color: '#1f2937', margin: 0, display: 'block', borderBottom: '1px solid #e5e7eb', paddingBottom: '6px' }} {...props} />
                  </div>
                ),
                h3: ({ node, ...props }) => (
                  <div style={{ marginTop: '16px', marginBottom: '8px' }}>
                    <h3 style={{ fontSize: '16px', fontWeight: '600', color: '#1f2937', margin: 0, display: 'block' }} {...props} />
                  </div>
                ),
                p: ({ node, ...props }) => <span style={{ color: '#374151', lineHeight: '1.8' }} {...props} />,
                ul: ({ node, ...props }) => <span style={{ display: 'inline' }} {...props} />,
                ol: ({ node, ...props }) => <span style={{ display: 'inline' }} {...props} />,
                li: ({ node, ...props }) => <span style={{ color: '#374151' }}> • {props.children} </span>,
                code: ({ node, inline, ...props }: any) => (
                  <span style={{ color: '#1f2937', fontFamily: 'monospace' }} {...props} />
                ),
                blockquote: ({ node, ...props }) => (
                  <span style={{ color: '#374151' }} {...props} />
                ),
                strong: ({ node, ...props }) => (
                  <strong style={{ fontWeight: 'bold' }} {...props} />
                ),
                em: ({ node, ...props }) => (
                  <em style={{ fontStyle: 'italic' }} {...props} />
                ),
                table: ({ node, ...props }) => null,
                th: ({ node, ...props }) => null,
                td: ({ node, ...props }) => null,
                br: () => <span> </span>
              }}
            >
              {analysis.analysis}
            </ReactMarkdown>
          </div>
        </div>

        {/* Footer */}
        <div style={{
          padding: '16px 24px',
          borderTop: '1px solid #e5e7eb',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          backgroundColor: 'white'
        }}>
          <div style={{ display: 'flex', gap: '12px' }}>
            <button
              onClick={handleCopy}
              style={{
                padding: '8px 16px',
                border: '1px solid #d1d5db',
                borderRadius: '6px',
                fontSize: '14px',
                fontWeight: '500',
                color: '#374151',
                backgroundColor: 'white',
                cursor: 'pointer',
                display: 'flex',
                alignItems: 'center',
                gap: '8px'
              }}
            >
              {copied ? (
                <>
                  <Check size={16} style={{ color: '#16a34a' }} />
                  Copied!
                </>
              ) : (
                <>
                  <Copy size={16} />
                  Copy Response
                </>
              )}
            </button>
            <button
              onClick={handleSave}
              style={{
                padding: '8px 16px',
                border: '1px solid #d1d5db',
                borderRadius: '6px',
                fontSize: '14px',
                fontWeight: '500',
                color: '#374151',
                backgroundColor: 'white',
                cursor: 'pointer',
                display: 'flex',
                alignItems: 'center',
                gap: '8px'
              }}
            >
              {saved ? (
                <>
                  <Check size={16} style={{ color: '#16a34a' }} />
                  Saved!
                </>
              ) : (
                <>
                  <Download size={16} />
                  Save Analysis
                </>
              )}
            </button>
          </div>
          <button
            onClick={onClose}
            style={{
              padding: '8px 16px',
              border: '1px solid #d1d5db',
              borderRadius: '6px',
              fontSize: '14px',
              fontWeight: '500',
              color: '#374151',
              backgroundColor: 'white',
              cursor: 'pointer'
            }}
          >
            Close
          </button>
        </div>
      </div>
    </div>
  )
}

export default AIAnalysisModal
