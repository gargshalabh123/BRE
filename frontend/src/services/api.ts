import axios from 'axios'

const API_BASE_URL = '/api'

export interface UploadResponse {
  upload_id: string
  filename: string
  size: number
  is_archive: boolean
  files_count: number
  files: string[]
  analysis_path: string
}

export interface AnalysisResults {
  summary: {
    total_files: number
    total_size_bytes: number
    total_size_mb: number
    file_types: Record<string, number>
  }
  files: Array<{
    path: string
    name: string
    extension: string
    type: string
    size: number
  }>
  metrics: {
    total_loc: number
    total_sloc: number
    total_comments: number
    total_blank: number
    by_file: Array<{
      file: string
      loc: number
      sloc: number
      comments: number
      blank: number
      complexity?: number
      functions?: number
    }>
  }
  dependencies: Record<string, string[]>
  detailed_dependencies?: Record<string, Array<{
    target: string
    type: string
    line: number
    signature: string
    parameters: string[]
    description: string
  }>>
  database_operations: {
    queries: Array<{
      file: string
      query: string
      type: string
      line: number
    }>
    total_count: number
    by_type: Record<string, number>
  }
  business_rules: Array<{
    file: string
    line: number
    type: string
    code: string
  }>
}

export interface ExplanationRequest {
  upload_id: string
  file_path: string
  code_snippet?: string
  line_start?: number
  line_end?: number
  provider?: string
}

export interface ExplanationResponse {
  file_path: string
  explanation: string
  provider: string
  code_length: number
}

export interface SummaryResponse {
  upload_id: string
  summary: string
  provider: string
  statistics: any
}

export interface ZipUploadResponse {
  upload_id: string
  filename: string
  size_bytes: number
  status: string
}

export interface ZipSummary {
  filename: string
  total_files: number
  total_size_mb: number
  total_loc: number
  languages: Array<{
    language: string
    file_count: number
    total_loc: number
    percentage: number
  }>
  top_extensions: Array<[string, { count: number; size: number; loc: number }]>
}

export interface FileMetadata {
  path: string
  name: string
  directory: string
  extension: string
  language: string
  size_bytes: number
  size_kb: number
  compressed_size: number
  compression_ratio: number
  date_time: string | null
  mime_type: string
  estimated_loc: number
  is_text: boolean
}

export interface ZipExplorerData {
  zip_info: {
    filename: string
    path: string
    size_bytes: number
    size_mb: number
    created: string
    modified: string
    total_files: number
    compression: string
  }
  file_tree: {
    name: string
    type: 'file' | 'folder'
    children?: any[]
    size?: number
    full_path?: string
  }
  files: FileMetadata[]
  statistics: {
    total_files: number
    total_size_bytes: number
    total_size_mb: number
    total_compressed_bytes: number
    total_compressed_mb: number
    overall_compression_ratio: number
    total_lines_of_code: number
    text_files: number
    binary_files: number
    by_extension: Record<string, { count: number; size: number; loc: number }>
    by_directory: Record<string, { count: number; size: number; loc: number }>
    largest_files: Array<{ path: string; size_kb: number; loc: number }>
    unique_directories: number
    unique_extensions: number
  }
  language_distribution: {
    languages: Record<string, {
      file_count: number
      total_size: number
      total_loc: number
      files: string[]
    }>
    languages_sorted: Array<{
      language: string
      file_count: number
      total_size_kb: number
      total_loc: number
      percentage: number
    }>
  }
  detailed_analysis: any
}

const api = {
  // Upload endpoints
  uploadFile: async (file: File): Promise<UploadResponse> => {
    const formData = new FormData()
    formData.append('file', file)
    const response = await axios.post(`${API_BASE_URL}/upload`, formData, {
      headers: { 'Content-Type': 'multipart/form-data' }
    })
    return response.data
  },

  deleteUpload: async (uploadId: string): Promise<void> => {
    await axios.delete(`${API_BASE_URL}/upload/${uploadId}`)
  },

  // Analysis endpoints
  analyzeFullCodebase: async (uploadId: string): Promise<AnalysisResults> => {
    const response = await axios.post(`${API_BASE_URL}/analysis/${uploadId}/full`)
    return response.data
  },

  analyzeMetrics: async (uploadId: string): Promise<any> => {
    const response = await axios.post(`${API_BASE_URL}/analysis/${uploadId}/metrics`)
    return response.data
  },

  analyzeDependencies: async (uploadId: string): Promise<any> => {
    const response = await axios.post(`${API_BASE_URL}/analysis/${uploadId}/dependencies`)
    return response.data
  },

  analyzeDatabase: async (uploadId: string): Promise<any> => {
    const response = await axios.post(`${API_BASE_URL}/analysis/${uploadId}/database`)
    return response.data
  },

  extractBusinessRules: async (uploadId: string): Promise<any> => {
    const response = await axios.post(`${API_BASE_URL}/analysis/${uploadId}/business-rules`)
    return response.data
  },

  getFileContent: async (uploadId: string, filePath: string): Promise<any> => {
    const response = await axios.get(`${API_BASE_URL}/analysis/${uploadId}/file/${filePath}`)
    return response.data
  },

  // AI endpoints
  explainCode: async (request: ExplanationRequest): Promise<ExplanationResponse> => {
    const response = await axios.post(`${API_BASE_URL}/ai/explain-code`, request)
    return response.data
  },

  summarizeCodebase: async (uploadId: string, provider: string = 'anthropic'): Promise<SummaryResponse> => {
    const response = await axios.post(`${API_BASE_URL}/ai/summarize/${uploadId}?provider=${provider}`)
    return response.data
  },

  getAvailableProviders: async (): Promise<any> => {
    const response = await axios.get(`${API_BASE_URL}/ai/providers`)
    return response.data
  },

  // ZIP Explorer endpoints
  uploadZip: async (file: File): Promise<ZipUploadResponse> => {
    const formData = new FormData()
    formData.append('file', file)
    const response = await axios.post(`${API_BASE_URL}/zip/upload`, formData, {
      headers: { 'Content-Type': 'multipart/form-data' }
    })
    return response.data
  },

  getZipSummary: async (uploadId: string): Promise<ZipSummary> => {
    const response = await axios.get(`${API_BASE_URL}/zip/${uploadId}/summary`)
    return response.data
  },

  exploreZip: async (uploadId: string, detailed: boolean = false): Promise<ZipExplorerData> => {
    const response = await axios.get(`${API_BASE_URL}/zip/${uploadId}/explore?detailed=${detailed}`)
    return response.data
  },

  listZipFiles: async (uploadId: string, language?: string, extension?: string): Promise<{ total_files: number; files: FileMetadata[] }> => {
    const params = new URLSearchParams()
    if (language) params.append('language', language)
    if (extension) params.append('extension', extension)
    const response = await axios.get(`${API_BASE_URL}/zip/${uploadId}/files?${params.toString()}`)
    return response.data
  },

  getZipFileContent: async (uploadId: string, filePath: string): Promise<{ path: string; content: string; lines: number; size: number }> => {
    const response = await axios.get(`${API_BASE_URL}/zip/${uploadId}/file/${filePath}`)
    return response.data
  },

  extractZip: async (uploadId: string): Promise<any> => {
    const response = await axios.post(`${API_BASE_URL}/zip/${uploadId}/extract`)
    return response.data
  },

  getZipStatistics: async (uploadId: string): Promise<any> => {
    const response = await axios.get(`${API_BASE_URL}/zip/${uploadId}/statistics`)
    return response.data
  },

  deleteZip: async (uploadId: string): Promise<void> => {
    await axios.delete(`${API_BASE_URL}/zip/${uploadId}`)
  }
}

export default api
