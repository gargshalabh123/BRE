import React, { useState, useEffect } from 'react'
import { X, Download, Loader } from 'lucide-react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism'
import api from '../services/api'

interface Props {
  uploadId: string
  filePath: string
  fileName: string
  onClose: () => void
}

const FilePreviewModal: React.FC<Props> = ({ uploadId, filePath, fileName, onClose }) => {
  const [content, setContent] = useState<string>('')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [lineCount, setLineCount] = useState(0)

  useEffect(() => {
    const loadContent = async () => {
      try {
        setLoading(true)
        const response = await api.getZipFileContent(uploadId, filePath)
        setContent(response.content)
        setLineCount(response.lines)
        setError(null)
      } catch (err: any) {
        setError(err.response?.data?.detail || 'Failed to load file content')
      } finally {
        setLoading(false)
      }
    }

    loadContent()
  }, [uploadId, filePath])

  const getLanguage = (fileName: string): string => {
    const ext = fileName.toLowerCase().split('.').pop()
    const languageMap: Record<string, string> = {
      'cbl': 'cobol',
      'cob': 'cobol',
      'cobol': 'cobol',
      'cpy': 'cobol',
      'sql': 'sql',
      'ddl': 'sql',
      'dml': 'sql',
      'rpg': 'text',
      'rpgle': 'text',
      'rpglec': 'text',
      'sqlrpgle': 'sql',
      'js': 'javascript',
      'ts': 'typescript',
      'py': 'python',
      'java': 'java',
      'c': 'c',
      'cpp': 'cpp',
      'txt': 'text',
      'log': 'text'
    }
    return languageMap[ext || ''] || 'text'
  }

  const handleDownload = () => {
    const blob = new Blob([content], { type: 'text/plain' })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = fileName
    link.click()
    URL.revokeObjectURL(url)
  }

  return (
    <div style={{
      position: 'fixed',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
      backgroundColor: 'rgba(0, 0, 0, 0.7)',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      zIndex: 1000,
      padding: '20px'
    }} onClick={onClose}>
      <div
        style={{
          backgroundColor: 'white',
          borderRadius: '12px',
          width: '90%',
          maxWidth: '1200px',
          maxHeight: '90vh',
          display: 'flex',
          flexDirection: 'column',
          overflow: 'hidden',
          boxShadow: '0 20px 60px rgba(0,0,0,0.3)'
        }}
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div style={{
          padding: '20px',
          borderBottom: '1px solid #e0e0e0',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center'
        }}>
          <div>
            <h2 style={{ margin: 0, fontSize: '18px', fontWeight: 600 }}>{fileName}</h2>
            <p style={{ margin: '5px 0 0 0', fontSize: '14px', color: '#666' }}>
              {filePath} {lineCount > 0 && `• ${lineCount.toLocaleString()} lines`}
            </p>
          </div>
          <div style={{ display: 'flex', gap: '10px', alignItems: 'center' }}>
            <button
              onClick={handleDownload}
              disabled={loading}
              style={{
                padding: '8px 16px',
                border: 'none',
                borderRadius: '6px',
                backgroundColor: '#667eea',
                color: 'white',
                cursor: 'pointer',
                fontSize: '14px',
                display: 'flex',
                alignItems: 'center',
                gap: '6px'
              }}
            >
              <Download size={16} />
              Download
            </button>
            <button
              onClick={onClose}
              style={{
                padding: '8px',
                border: 'none',
                borderRadius: '6px',
                backgroundColor: '#f0f0f0',
                cursor: 'pointer',
                display: 'flex',
                alignItems: 'center'
              }}
            >
              <X size={20} />
            </button>
          </div>
        </div>

        {/* Content */}
        <div style={{
          flex: 1,
          overflow: 'auto',
          backgroundColor: '#1e1e1e'
        }}>
          {loading ? (
            <div style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              height: '100%',
              color: 'white'
            }}>
              <Loader className="loading-spinner" size={32} />
              <span style={{ marginLeft: '10px' }}>Loading file content...</span>
            </div>
          ) : error ? (
            <div style={{
              padding: '40px',
              textAlign: 'center',
              color: '#ff4444'
            }}>
              <strong>Error:</strong> {error}
            </div>
          ) : (
            <SyntaxHighlighter
              language={getLanguage(fileName)}
              style={vscDarkPlus}
              showLineNumbers
              wrapLines
              customStyle={{
                margin: 0,
                padding: '20px',
                fontSize: '13px',
                lineHeight: '1.5',
                minHeight: '100%'
              }}
            >
              {content}
            </SyntaxHighlighter>
          )}
        </div>
      </div>
    </div>
  )
}

export default FilePreviewModal
