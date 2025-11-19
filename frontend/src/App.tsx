import React, { useState } from 'react'
import { BrowserRouter as Router, Routes, Route, Link } from 'react-router-dom'
import UploadPage from './pages/UploadPage'
import AnalysisPage from './pages/AnalysisPage'
import FileDetailPage from './pages/FileDetailPage'

function App() {
  return (
    <Router>
      <div className="app">
        <Routes>
          <Route path="/" element={<UploadPage />} />
          <Route path="/analysis/:uploadId" element={<AnalysisPage />} />
          <Route path="/analysis/:uploadId/file/*" element={<FileDetailPage />} />
        </Routes>
      </div>
    </Router>
  )
}

export default App
