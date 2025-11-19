import React from 'react'
import { FileArchive, Files, FileCode, Percent, HardDrive, Code } from 'lucide-react'
import { ZipExplorerData } from '../services/api'

interface Props {
  data: ZipExplorerData
}

const ZipSummaryCards: React.FC<Props> = ({ data }) => {
  const { statistics, language_distribution, zip_info } = data

  // Count files by language
  const cobolCount = language_distribution.languages['cobol']?.file_count || 0
  const sqlCount = language_distribution.languages['sql']?.file_count || 0
  const as400Count = language_distribution.languages['as400']?.file_count || 0

  return (
    <div className="grid" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))' }}>
      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <FileArchive size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>ZIP File</h3>
        </div>
        <div className="value" style={{ fontSize: '18px' }}>{zip_info.filename}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {zip_info.size_mb} MB
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <Files size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>Total Files</h3>
        </div>
        <div className="value">{statistics.total_files.toLocaleString()}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {statistics.text_files} text, {statistics.binary_files} binary
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <FileCode size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>Lines of Code</h3>
        </div>
        <div className="value">{statistics.total_lines_of_code.toLocaleString()}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          Across all files
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <HardDrive size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>Total Size</h3>
        </div>
        <div className="value">{statistics.total_size_mb.toFixed(2)} MB</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          Uncompressed
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <Percent size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>Compression</h3>
        </div>
        <div className="value">{statistics.overall_compression_ratio.toFixed(1)}%</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {statistics.total_compressed_mb.toFixed(2)} MB compressed
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <Code size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>COBOL Files</h3>
        </div>
        <div className="value">{cobolCount}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {language_distribution.languages['cobol']?.total_loc.toLocaleString() || 0} LOC
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <Code size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>SQL Files</h3>
        </div>
        <div className="value">{sqlCount}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {language_distribution.languages['sql']?.total_loc.toLocaleString() || 0} LOC
        </div>
      </div>

      <div className="stat-card">
        <div style={{ display: 'flex', alignItems: 'center', marginBottom: '10px' }}>
          <Code size={24} color="#667eea" style={{ marginRight: '10px' }} />
          <h3 style={{ margin: 0, fontSize: '14px', color: '#666' }}>AS400 Files</h3>
        </div>
        <div className="value">{as400Count}</div>
        <div style={{ fontSize: '12px', color: '#999', marginTop: '5px' }}>
          {language_distribution.languages['as400']?.total_loc.toLocaleString() || 0} LOC
        </div>
      </div>
    </div>
  )
}

export default ZipSummaryCards
