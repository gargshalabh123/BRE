import React, { useState } from 'react'
import { ChevronRight, ChevronDown, Folder, FolderOpen, File } from 'lucide-react'

interface TreeNode {
  name: string
  type: 'file' | 'folder'
  children?: TreeNode[]
  size?: number
  compressed_size?: number
  full_path?: string
}

interface Props {
  tree: TreeNode
  onFileClick?: (filePath: string) => void
}

const FileTreeNode: React.FC<{
  node: TreeNode
  level: number
  onFileClick?: (filePath: string) => void
}> = ({ node, level, onFileClick }) => {
  const [isExpanded, setIsExpanded] = useState(level < 2) // Auto-expand first 2 levels

  const handleClick = () => {
    if (node.type === 'folder') {
      setIsExpanded(!isExpanded)
    } else if (node.full_path && onFileClick) {
      onFileClick(node.full_path)
    }
  }

  const formatSize = (bytes?: number) => {
    if (!bytes) return ''
    if (bytes < 1024) return `${bytes} B`
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`
  }

  return (
    <div>
      <div
        style={{
          paddingLeft: `${level * 20}px`,
          display: 'flex',
          alignItems: 'center',
          padding: '6px 10px',
          cursor: 'pointer',
          borderRadius: '4px',
          transition: 'background 0.2s'
        }}
        className="tree-node"
        onClick={handleClick}
        onMouseEnter={(e) => (e.currentTarget.style.background = '#f0f0f0')}
        onMouseLeave={(e) => (e.currentTarget.style.background = 'transparent')}
      >
        {node.type === 'folder' ? (
          <>
            {isExpanded ? (
              <ChevronDown size={16} style={{ marginRight: '4px', color: '#666' }} />
            ) : (
              <ChevronRight size={16} style={{ marginRight: '4px', color: '#666' }} />
            )}
            {isExpanded ? (
              <FolderOpen size={16} style={{ marginRight: '8px', color: '#667eea' }} />
            ) : (
              <Folder size={16} style={{ marginRight: '8px', color: '#667eea' }} />
            )}
          </>
        ) : (
          <>
            <span style={{ width: '20px', marginRight: '4px' }}></span>
            <File size={16} style={{ marginRight: '8px', color: '#999' }} />
          </>
        )}
        <span style={{ flex: 1, fontSize: '14px' }}>{node.name}</span>
        {node.size !== undefined && (
          <span style={{ fontSize: '12px', color: '#999', marginLeft: '10px' }}>
            {formatSize(node.size)}
          </span>
        )}
      </div>
      {node.type === 'folder' && isExpanded && node.children && (
        <div>
          {node.children.map((child, index) => (
            <FileTreeNode
              key={`${child.name}-${index}`}
              node={child}
              level={level + 1}
              onFileClick={onFileClick}
            />
          ))}
        </div>
      )}
    </div>
  )
}

const FileTreeViewer: React.FC<Props> = ({ tree, onFileClick }) => {
  return (
    <div style={{
      border: '1px solid #e0e0e0',
      borderRadius: '8px',
      padding: '10px',
      maxHeight: '500px',
      overflowY: 'auto',
      backgroundColor: '#fff'
    }}>
      <FileTreeNode node={tree} level={0} onFileClick={onFileClick} />
    </div>
  )
}

export default FileTreeViewer
