import axios from 'axios'

const API_BASE_URL = 'http://localhost:8000/api'

// Configure axios with timeouts
const apiClient = axios.create({
  baseURL: API_BASE_URL,
  timeout: 180000, // 3 minutes for long-running operations like analysis
  headers: {
    'Content-Type': 'application/json'
  }
})

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

// New AI Analysis Interfaces
export interface AIAnalysisResponse {
  analysis: string
  provider: string
  model: string
  cached: boolean
  cached_at?: string
  prompt?: string  // Debug: Include prompt for verification
}

export interface FileAnalysisRequest {
  upload_id: string
  file: string
  loc?: number
  sloc?: number
  complexity?: number
  comments?: number
  blank?: number
  functions?: number
  provider?: string
}

export interface RuleAnalysisRequest {
  upload_id: string
  file: string
  line: number
  type: string
  code: string
  description?: string
  provider?: string
}

export interface DependencyAnalysisRequest {
  upload_id: string
  file: string
  type: string
  target: string
  line?: number
  lines?: number[]
  signature?: string
  parameters?: string[]
  description?: string
  provider?: string
}

export interface QueryAnalysisRequest {
  upload_id: string
  file: string
  line: number
  type: string
  query: string
  category?: string
  provider?: string
}

export interface ChatRequest {
  upload_id: string
  message: string
  session_id?: string
  provider?: string
}

export interface ChatResponse {
  message: string
  provider: string
  model: string
  context_used: string[]
}

export interface ZipUploadResponse {
  upload_id: string
  filename: string
  size_bytes: number
  status: string
  total_files?: number
  available_extensions?: Record<string, number>
  recommended_extensions?: string[]
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

// Agentic AI Interfaces
export interface AgenticAnalysisResponse {
  upload_id: string
  query: string
  agent_type: string | null
  answer: string
  reasoning_trace: Array<{
    iteration?: number
    thought?: string
    action?: any
    observation?: any
    error?: string
  }>
  iterations_used: number
  status: string
  provider: string
  model: string
}

export interface AgenticChatResponse {
  upload_id: string
  message: string
  response: string
  reasoning_trace: Array<any>
  conversation_aware: boolean
  provider: string
  model: string
}

export interface MultiAgentResponse {
  upload_id: string
  agents_used: string[]
  results: Record<string, {
    answer?: string
    reasoning_trace?: Array<any>
    status?: string
    error?: string
  }>
  synthesis?: string
  provider: string
  model: string
}

const api = {
  // Upload endpoints
  uploadFile: async (file: File): Promise<UploadResponse> => {
    const formData = new FormData()
    formData.append('file', file)
    const response = await apiClient.post('/upload', formData, {
      headers: { 'Content-Type': 'multipart/form-data' }
    })
    return response.data
  },

  deleteUpload: async (uploadId: string): Promise<void> => {
    await apiClient.delete(`/upload/${uploadId}`)
  },

  // Analysis endpoints
  analyzeFullCodebase: async (uploadId: string, selectedExtensions?: string[]): Promise<AnalysisResults> => {
    const response = await apiClient.post(`/analysis/${uploadId}/full`, {
      selected_extensions: selectedExtensions
    })
    return response.data
  },

  analyzeMetrics: async (uploadId: string): Promise<any> => {
    const response = await apiClient.post(`/analysis/${uploadId}/metrics`)
    return response.data
  },

  analyzeDependencies: async (uploadId: string): Promise<any> => {
    const response = await apiClient.post(`/analysis/${uploadId}/dependencies`)
    return response.data
  },

  analyzeDatabase: async (uploadId: string): Promise<any> => {
    const response = await apiClient.post(`/analysis/${uploadId}/database`)
    return response.data
  },

  extractBusinessRules: async (uploadId: string): Promise<any> => {
    const response = await apiClient.post(`/analysis/${uploadId}/business-rules`)
    return response.data
  },

  getFileContent: async (uploadId: string, filePath: string): Promise<any> => {
    const response = await apiClient.get(`/analysis/${uploadId}/file/${filePath}`)
    return response.data
  },

  // AI endpoints
  explainCode: async (request: ExplanationRequest): Promise<ExplanationResponse> => {
    const response = await apiClient.post(`/ai/explain-code`, request)
    return response.data
  },

  summarizeCodebase: async (uploadId: string, provider: string = 'anthropic'): Promise<SummaryResponse> => {
    const response = await apiClient.post(`/ai/insights`, {
      upload_id: uploadId,
      scope: 'codebase',
      provider: provider
    })
    return { summary: response.data.analysis, provider: response.data.provider, model: response.data.model }
  },

  getAvailableProviders: async (): Promise<any> => {
    const response = await apiClient.get(`/ai/providers`)
    return response.data
  },

  // New AI Analysis endpoints
  analyzeFile: async (request: FileAnalysisRequest): Promise<AIAnalysisResponse> => {
    const response = await apiClient.post(`/ai/analyze-file`, request)
    return response.data
  },

  analyzeRule: async (request: RuleAnalysisRequest): Promise<AIAnalysisResponse> => {
    const response = await apiClient.post(`/ai/analyze-rule`, request)
    return response.data
  },

  analyzeDependency: async (request: DependencyAnalysisRequest): Promise<AIAnalysisResponse> => {
    const response = await apiClient.post(`/ai/analyze-dependency`, request)
    return response.data
  },

  analyzeQuery: async (request: QueryAnalysisRequest): Promise<AIAnalysisResponse> => {
    const response = await apiClient.post(`/ai/analyze-query`, request)
    return response.data
  },

  generateInsights: async (uploadId: string, scope: string, itemId?: string, provider?: string): Promise<AIAnalysisResponse> => {
    const response = await apiClient.post(`/ai/insights`, {
      upload_id: uploadId,
      scope,
      item_id: itemId,
      provider
    })
    return response.data
  },

  chat: async (request: ChatRequest): Promise<ChatResponse> => {
    const response = await apiClient.post(`/ai/chat`, request)
    return response.data
  },

  getAIHistory: async (uploadId: string): Promise<any> => {
    const response = await apiClient.get(`/ai/history/${uploadId}`)
    return response.data
  },

  // ZIP Explorer endpoints
  uploadZip: async (file: File): Promise<ZipUploadResponse> => {
    const formData = new FormData()
    formData.append('file', file)
    const response = await apiClient.post(`/zip/upload`, formData, {
      headers: { 'Content-Type': 'multipart/form-data' }
    })
    return response.data
  },

  getZipSummary: async (uploadId: string): Promise<ZipSummary> => {
    const response = await apiClient.get(`/zip/${uploadId}/summary`)
    return response.data
  },

  exploreZip: async (uploadId: string, detailed: boolean = false): Promise<ZipExplorerData> => {
    const response = await apiClient.get(`/zip/${uploadId}/explore?detailed=${detailed}`)
    return response.data
  },

  listZipFiles: async (uploadId: string, language?: string, extension?: string): Promise<{ total_files: number; files: FileMetadata[] }> => {
    const params = new URLSearchParams()
    if (language) params.append('language', language)
    if (extension) params.append('extension', extension)
    const response = await apiClient.get(`/zip/${uploadId}/files?${params.toString()}`)
    return response.data
  },

  getZipFileContent: async (uploadId: string, filePath: string): Promise<{ path: string; content: string; lines: number; size: number }> => {
    const response = await apiClient.get(`/zip/${uploadId}/file/${filePath}`)
    return response.data
  },

  extractZip: async (uploadId: string): Promise<any> => {
    const response = await apiClient.post(`/zip/${uploadId}/extract`)
    return response.data
  },

  getZipStatistics: async (uploadId: string): Promise<any> => {
    const response = await apiClient.get(`/zip/${uploadId}/statistics`)
    return response.data
  },

  deleteZip: async (uploadId: string): Promise<void> => {
    await apiClient.delete(`/zip/${uploadId}`)
  },

  // Saved analysis endpoints
  deleteSavedAnalysis: async (uploadId: string): Promise<void> => {
    await apiClient.delete(`/analysis/saved/${uploadId}`)
  },

  deleteAllSavedAnalyses: async (): Promise<{ message: string; deleted_count: number }> => {
    const response = await axios.delete(`${API_BASE_URL}/analysis/saved`)
    return response.data
  },

  // Agentic AI Analysis endpoints
  agenticAnalyze: async (uploadId: string, query: string, agentType?: string, provider?: string): Promise<AgenticAnalysisResponse> => {
    const response = await apiClient.post(`/agentic/analyze`, {
      upload_id: uploadId,
      query,
      agent_type: agentType,
      provider
    })
    return response.data
  },

  agenticChat: async (uploadId: string, message: string, conversationHistory?: any[], provider?: string): Promise<AgenticChatResponse> => {
    const response = await apiClient.post(`/agentic/chat`, {
      upload_id: uploadId,
      message,
      conversation_history: conversationHistory,
      provider
    })
    return response.data
  },

  multiAgentAnalysis: async (uploadId: string, agentTypes: string[], provider?: string): Promise<MultiAgentResponse> => {
    const response = await apiClient.post(`/agentic/multi-agent`, {
      upload_id: uploadId,
      agent_types: agentTypes,
      provider
    })
    return response.data
  },

  getAvailableAgents: async (): Promise<any> => {
    const response = await apiClient.get(`/agentic/agents`)
    return response.data
  },

  getAvailableTools: async (): Promise<any> => {
    const response = await apiClient.get(`/agentic/tools`)
    return response.data
  },

  getAgenticStats: async (uploadId: string, provider?: string): Promise<any> => {
    const response = await apiClient.get(`/agentic/stats/${uploadId}${provider ? `?provider=${provider}` : ''}`)
    return response.data
  },

  // Enhanced Dependency Analysis endpoints
  getAllPrograms: async (uploadId: string): Promise<any> => {
    const response = await apiClient.get(`/dependencies/programs/${uploadId}`)
    return response.data
  },

  getProgramCallers: async (uploadId: string, programName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/callers`, {
      upload_id: uploadId,
      program_name: programName
    })
    return response.data
  },

  getProgramCallees: async (uploadId: string, programName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/callees`, {
      upload_id: uploadId,
      program_name: programName
    })
    return response.data
  },

  getDatabaseUsage: async (uploadId: string, programName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/database-usage`, {
      upload_id: uploadId,
      program_name: programName
    })
    return response.data
  },

  getTableImpact: async (uploadId: string, tableName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/table-impact`, {
      upload_id: uploadId,
      table_name: tableName
    })
    return response.data
  },

  getExecutionPath: async (uploadId: string, fromProgram: string, toProgram: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/execution-path`, {
      upload_id: uploadId,
      from_program: fromProgram,
      to_program: toProgram
    })
    return response.data
  },

  getCopybookUsage: async (uploadId: string, copybookName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/copybook-usage`, {
      upload_id: uploadId,
      copybook_name: copybookName
    })
    return response.data
  },

  getProgramCopybooks: async (uploadId: string, programName: string): Promise<any> => {
    const response = await apiClient.post(`/dependencies/program-copybooks`, {
      upload_id: uploadId,
      program_name: programName
    })
    return response.data
  },

  getDependencySummary: async (uploadId: string): Promise<any> => {
    const response = await apiClient.get(`/dependencies/summary/${uploadId}`)
    return response.data
  }
}

export default api
