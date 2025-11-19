import React, { useState, useMemo } from 'react'
import { Search, ArrowUpDown, Eye } from 'lucide-react'
import { FileMetadata } from '../services/api'

interface Props {
  files: FileMetadata[]
  onFileClick?: (file: FileMetadata) => void
}

const FileListTable: React.FC<Props> = ({ files, onFileClick }) => {
  const [searchTerm, setSearchTerm] = useState('')
  const [languageFilter, setLanguageFilter] = useState('all')
  const [extensionFilter, setExtensionFilter] = useState('all')
  const [sortBy, setSortBy] = useState<'name' | 'size' | 'loc'>('name')
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('asc')

  // Get unique languages and extensions
  const languages = useMemo(() => {
    const unique = new Set(files.map(f => f.language))
    return ['all', ...Array.from(unique).sort()]
  }, [files])

  const extensions = useMemo(() => {
    const unique = new Set(files.map(f => f.extension).filter(Boolean))
    return ['all', ...Array.from(unique).sort()]
  }, [files])

  // Filter and sort files
  const filteredFiles = useMemo(() => {
    let result = files.filter(file => {
      const matchesSearch = file.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                          file.path.toLowerCase().includes(searchTerm.toLowerCase())
      const matchesLanguage = languageFilter === 'all' || file.language === languageFilter
      const matchesExtension = extensionFilter === 'all' || file.extension === extensionFilter
      return matchesSearch && matchesLanguage && matchesExtension
    })

    // Sort
    result.sort((a, b) => {
      let comparison = 0
      if (sortBy === 'name') {
        comparison = a.name.localeCompare(b.name)
      } else if (sortBy === 'size') {
        comparison = a.size_bytes - b.size_bytes
      } else if (sortBy === 'loc') {
        comparison = a.estimated_loc - b.estimated_loc
      }
      return sortOrder === 'asc' ? comparison : -comparison
    })

    return result
  }, [files, searchTerm, languageFilter, extensionFilter, sortBy, sortOrder])

  const handleSort = (column: 'name' | 'size' | 'loc') => {
    if (sortBy === column) {
      setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc')
    } else {
      setSortBy(column)
      setSortOrder('asc')
    }
  }

  const getLanguageBadgeColor = (language: string) => {
    const colors: Record<string, string> = {
      'cobol': '#667eea',
      'sql': '#764ba2',
      'as400': '#f093fb',
      'unknown': '#999'
    }
    return colors[language] || '#999'
  }

  return (
    <div>
      {/* Filters */}
      <div style={{
        display: 'flex',
        gap: '10px',
        marginBottom: '20px',
        flexWrap: 'wrap',
        alignItems: 'center'
      }}>
        <div style={{ flex: '1 1 300px', position: 'relative' }}>
          <Search size={18} style={{
            position: 'absolute',
            left: '12px',
            top: '50%',
            transform: 'translateY(-50%)',
            color: '#999'
          }} />
          <input
            type="text"
            placeholder="Search files..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            style={{
              width: '100%',
              padding: '10px 10px 10px 40px',
              border: '1px solid #e0e0e0',
              borderRadius: '6px',
              fontSize: '14px'
            }}
          />
        </div>

        <select
          value={languageFilter}
          onChange={(e) => setLanguageFilter(e.target.value)}
          style={{
            padding: '10px',
            border: '1px solid #e0e0e0',
            borderRadius: '6px',
            fontSize: '14px',
            minWidth: '150px'
          }}
        >
          {languages.map(lang => (
            <option key={lang} value={lang}>
              {lang === 'all' ? 'All Languages' : lang.toUpperCase()}
            </option>
          ))}
        </select>

        <select
          value={extensionFilter}
          onChange={(e) => setExtensionFilter(e.target.value)}
          style={{
            padding: '10px',
            border: '1px solid #e0e0e0',
            borderRadius: '6px',
            fontSize: '14px',
            minWidth: '150px'
          }}
        >
          {extensions.map(ext => (
            <option key={ext} value={ext}>
              {ext === 'all' ? 'All Extensions' : ext}
            </option>
          ))}
        </select>

        <div style={{ fontSize: '14px', color: '#666' }}>
          {filteredFiles.length} of {files.length} files
        </div>
      </div>

      {/* Table */}
      <div style={{
        border: '1px solid #e0e0e0',
        borderRadius: '8px',
        overflow: 'hidden'
      }}>
        <div style={{ overflowX: 'auto' }}>
          <table style={{
            width: '100%',
            borderCollapse: 'collapse',
            fontSize: '14px'
          }}>
            <thead>
              <tr style={{ backgroundColor: '#f8f9fa' }}>
                <th style={{
                  padding: '12px',
                  textAlign: 'left',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0',
                  cursor: 'pointer'
                }} onClick={() => handleSort('name')}>
                  <div style={{ display: 'flex', alignItems: 'center', gap: '5px' }}>
                    Name
                    <ArrowUpDown size={14} />
                  </div>
                </th>
                <th style={{
                  padding: '12px',
                  textAlign: 'left',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0'
                }}>
                  Directory
                </th>
                <th style={{
                  padding: '12px',
                  textAlign: 'left',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0'
                }}>
                  Language
                </th>
                <th style={{
                  padding: '12px',
                  textAlign: 'right',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0',
                  cursor: 'pointer'
                }} onClick={() => handleSort('size')}>
                  <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'flex-end', gap: '5px' }}>
                    Size
                    <ArrowUpDown size={14} />
                  </div>
                </th>
                <th style={{
                  padding: '12px',
                  textAlign: 'right',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0',
                  cursor: 'pointer'
                }} onClick={() => handleSort('loc')}>
                  <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'flex-end', gap: '5px' }}>
                    LOC
                    <ArrowUpDown size={14} />
                  </div>
                </th>
                <th style={{
                  padding: '12px',
                  textAlign: 'center',
                  fontWeight: 600,
                  borderBottom: '2px solid #e0e0e0',
                  width: '80px'
                }}>
                  Action
                </th>
              </tr>
            </thead>
            <tbody>
              {filteredFiles.length === 0 ? (
                <tr>
                  <td colSpan={6} style={{
                    padding: '40px',
                    textAlign: 'center',
                    color: '#999'
                  }}>
                    No files found
                  </td>
                </tr>
              ) : (
                filteredFiles.map((file, index) => (
                  <tr
                    key={`${file.path}-${index}`}
                    style={{
                      borderBottom: '1px solid #f0f0f0',
                      cursor: onFileClick ? 'pointer' : 'default',
                      transition: 'background 0.2s'
                    }}
                    onMouseEnter={(e) => (e.currentTarget.style.background = '#f8f9fa')}
                    onMouseLeave={(e) => (e.currentTarget.style.background = 'transparent')}
                    onClick={() => onFileClick && onFileClick(file)}
                  >
                    <td style={{ padding: '12px' }}>
                      <div style={{ fontWeight: 500 }}>{file.name}</div>
                      <div style={{ fontSize: '12px', color: '#999', marginTop: '2px' }}>
                        {file.extension}
                      </div>
                    </td>
                    <td style={{ padding: '12px', color: '#666' }}>
                      {file.directory || '/'}
                    </td>
                    <td style={{ padding: '12px' }}>
                      <span style={{
                        padding: '4px 8px',
                        borderRadius: '4px',
                        fontSize: '12px',
                        fontWeight: 500,
                        backgroundColor: getLanguageBadgeColor(file.language) + '20',
                        color: getLanguageBadgeColor(file.language)
                      }}>
                        {file.language.toUpperCase()}
                      </span>
                    </td>
                    <td style={{ padding: '12px', textAlign: 'right', color: '#666' }}>
                      {file.size_kb.toFixed(1)} KB
                    </td>
                    <td style={{ padding: '12px', textAlign: 'right', fontWeight: 500 }}>
                      {file.estimated_loc.toLocaleString()}
                    </td>
                    <td style={{ padding: '12px', textAlign: 'center' }}>
                      {onFileClick && file.is_text && (
                        <button
                          onClick={(e) => {
                            e.stopPropagation()
                            onFileClick(file)
                          }}
                          style={{
                            padding: '6px 12px',
                            border: 'none',
                            borderRadius: '4px',
                            backgroundColor: '#667eea',
                            color: 'white',
                            cursor: 'pointer',
                            fontSize: '12px',
                            display: 'inline-flex',
                            alignItems: 'center',
                            gap: '4px'
                          }}
                        >
                          <Eye size={14} />
                          View
                        </button>
                      )}
                    </td>
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

export default FileListTable
