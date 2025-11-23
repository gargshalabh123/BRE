import React, { useState, useEffect } from 'react'
import { CheckSquare, Square, FileCode, Database, File, Archive } from 'lucide-react'

interface ExtensionGroup {
  name: string
  icon: React.ReactNode
  extensions: string[]
  color: string
}

interface FileExtensionSelectorProps {
  availableExtensions: Record<string, number>
  recommendedExtensions: string[]
  onSelectionChange: (selected: string[]) => void
  onClose: () => void
}

const FileExtensionSelector: React.FC<FileExtensionSelectorProps> = ({
  availableExtensions,
  recommendedExtensions,
  onSelectionChange,
  onClose
}) => {
  const [selectedExtensions, setSelectedExtensions] = useState<Set<string>>(new Set(recommendedExtensions))

  // Extension groups for better UX
  const extensionGroups: ExtensionGroup[] = [
    {
      name: 'Legacy Code',
      icon: <FileCode size={20} color="#667eea" />,
      extensions: ['.cbl', '.cob', '.cobol', '.cpy', '.rpg', '.rpgle', '.sqlrpgle', '.rpg4', '.rpgiv'],
      color: '#667eea'
    },
    {
      name: 'Database',
      icon: <Database size={20} color="#43e97b" />,
      extensions: ['.sql', '.ddl', '.dml', '.pls', '.pkb', '.pks'],
      color: '#43e97b'
    },
    {
      name: 'AS400 Files',
      icon: <File size={20} color="#f093fb" />,
      extensions: ['.dspf', '.prtf', '.lf', '.pf', '.cmd', '.clle'],
      color: '#f093fb'
    },
    {
      name: 'Other',
      icon: <Archive size={20} color="#999" />,
      extensions: [],
      color: '#999'
    }
  ]

  // Categorize extensions
  const categorizedExtensions = Object.keys(availableExtensions).reduce((acc, ext) => {
    let found = false
    for (const group of extensionGroups.slice(0, -1)) {
      if (group.extensions.includes(ext)) {
        if (!acc[group.name]) acc[group.name] = []
        acc[group.name].push(ext)
        found = true
        break
      }
    }
    if (!found) {
      if (!acc['Other']) acc['Other'] = []
      acc['Other'].push(ext)
    }
    return acc
  }, {} as Record<string, string[]>)

  const handleToggle = (ext: string) => {
    const newSelected = new Set(selectedExtensions)
    if (newSelected.has(ext)) {
      newSelected.delete(ext)
    } else {
      newSelected.add(ext)
    }
    setSelectedExtensions(newSelected)
  }

  const handleSelectAll = () => {
    setSelectedExtensions(new Set(Object.keys(availableExtensions)))
  }

  const handleDeselectAll = () => {
    setSelectedExtensions(new Set())
  }

  const handleSelectGroup = (groupName: string) => {
    const newSelected = new Set(selectedExtensions)
    const exts = categorizedExtensions[groupName] || []
    exts.forEach(ext => newSelected.add(ext))
    setSelectedExtensions(newSelected)
  }

  const handleContinue = () => {
    onSelectionChange(Array.from(selectedExtensions))
    onClose()
  }

  const totalFiles = Object.values(availableExtensions).reduce((a, b) => a + b, 0)
  const selectedFiles = Array.from(selectedExtensions).reduce((sum, ext) => sum + (availableExtensions[ext] || 0), 0)

  return (
    <div style={{
      position: 'fixed',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
      backgroundColor: 'rgba(0,0,0,0.5)',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      zIndex: 1000,
      padding: '20px'
    }}>
      <div style={{
        backgroundColor: '#fff',
        borderRadius: '12px',
        padding: '30px',
        maxWidth: '800px',
        width: '100%',
        maxHeight: '90vh',
        overflow: 'auto',
        boxShadow: '0 10px 40px rgba(0,0,0,0.2)'
      }}>
        <h2 style={{ margin: '0 0 10px 0', fontSize: '24px', color: '#333' }}>
          Select File Types to Analyze
        </h2>
        <p style={{ margin: '0 0 20px 0', fontSize: '14px', color: '#666', lineHeight: '1.6' }}>
          Choose which file extensions you want to analyze. Only selected files will be processed and stored in the database.
        </p>

        <div style={{
          padding: '15px',
          backgroundColor: '#f5f7fa',
          borderRadius: '8px',
          marginBottom: '20px',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center'
        }}>
          <div>
            <strong>{selectedFiles}</strong> of <strong>{totalFiles}</strong> files selected
          </div>
          <div style={{ display: 'flex', gap: '10px' }}>
            <button
              onClick={handleSelectAll}
              style={{
                padding: '6px 12px',
                fontSize: '13px',
                color: '#667eea',
                backgroundColor: '#f0f3ff',
                border: '1px solid #667eea',
                borderRadius: '4px',
                cursor: 'pointer'
              }}
            >
              Select All
            </button>
            <button
              onClick={handleDeselectAll}
              style={{
                padding: '6px 12px',
                fontSize: '13px',
                color: '#666',
                backgroundColor: '#fff',
                border: '1px solid #ddd',
                borderRadius: '4px',
                cursor: 'pointer'
              }}
            >
              Deselect All
            </button>
          </div>
        </div>

        {/* Extension Groups */}
        {extensionGroups.map((group) => {
          const exts = categorizedExtensions[group.name]
          if (!exts || exts.length === 0) return null

          return (
            <div key={group.name} style={{ marginBottom: '25px' }}>
              <div style={{
                display: 'flex',
                alignItems: 'center',
                gap: '10px',
                marginBottom: '12px',
                paddingBottom: '8px',
                borderBottom: '2px solid #f0f0f0'
              }}>
                {group.icon}
                <h3 style={{ margin: 0, fontSize: '16px', color: '#333', flex: 1 }}>
                  {group.name}
                </h3>
                <button
                  onClick={() => handleSelectGroup(group.name)}
                  style={{
                    padding: '4px 10px',
                    fontSize: '12px',
                    color: group.color,
                    backgroundColor: `${group.color}10`,
                    border: `1px solid ${group.color}`,
                    borderRadius: '4px',
                    cursor: 'pointer'
                  }}
                >
                  Select Group
                </button>
              </div>
              <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(180px, 1fr))', gap: '10px' }}>
                {exts.sort().map((ext) => {
                  const isSelected = selectedExtensions.has(ext)
                  const count = availableExtensions[ext] || 0
                  return (
                    <div
                      key={ext}
                      onClick={() => handleToggle(ext)}
                      style={{
                        padding: '12px',
                        backgroundColor: isSelected ? '#f0f3ff' : '#fff',
                        border: isSelected ? '2px solid #667eea' : '2px solid #e0e0e0',
                        borderRadius: '8px',
                        cursor: 'pointer',
                        display: 'flex',
                        alignItems: 'center',
                        gap: '10px',
                        transition: 'all 0.2s'
                      }}
                      onMouseEnter={(e) => {
                        if (!isSelected) {
                          e.currentTarget.style.borderColor = '#bbb'
                          e.currentTarget.style.backgroundColor = '#f9f9f9'
                        }
                      }}
                      onMouseLeave={(e) => {
                        if (!isSelected) {
                          e.currentTarget.style.borderColor = '#e0e0e0'
                          e.currentTarget.style.backgroundColor = '#fff'
                        }
                      }}
                    >
                      {isSelected ? (
                        <CheckSquare size={20} color="#667eea" />
                      ) : (
                        <Square size={20} color="#ccc" />
                      )}
                      <div style={{ flex: 1 }}>
                        <div style={{ fontWeight: 'bold', fontSize: '13px', fontFamily: 'monospace' }}>{ext}</div>
                        <div style={{ fontSize: '11px', color: '#999' }}>{count} file{count !== 1 ? 's' : ''}</div>
                      </div>
                    </div>
                  )
                })}
              </div>
            </div>
          )
        })}

        {/* Action Buttons */}
        <div style={{
          display: 'flex',
          gap: '12px',
          justifyContent: 'flex-end',
          marginTop: '30px',
          paddingTop: '20px',
          borderTop: '1px solid #e0e0e0'
        }}>
          <button
            onClick={() => onClose()}
            style={{
              padding: '12px 24px',
              fontSize: '14px',
              fontWeight: 'bold',
              color: '#666',
              backgroundColor: '#f5f5f5',
              border: '1px solid #ddd',
              borderRadius: '6px',
              cursor: 'pointer'
            }}
          >
            Cancel
          </button>
          <button
            onClick={handleContinue}
            disabled={selectedExtensions.size === 0}
            style={{
              padding: '12px 24px',
              fontSize: '14px',
              fontWeight: 'bold',
              color: '#fff',
              background: selectedExtensions.size === 0 ? '#ccc' : 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
              border: 'none',
              borderRadius: '6px',
              cursor: selectedExtensions.size === 0 ? 'not-allowed' : 'pointer',
              opacity: selectedExtensions.size === 0 ? 0.6 : 1
            }}
          >
            Continue to Analysis ({selectedFiles} files)
          </button>
        </div>
      </div>
    </div>
  )
}

export default FileExtensionSelector
