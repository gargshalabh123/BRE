import React, { createContext, useContext, useState, useEffect } from 'react'

interface AIProviderContextType {
  selectedProvider: string
  setSelectedProvider: (provider: string) => void
  availableProviders: any
  setAvailableProviders: (providers: any) => void
}

const AIProviderContext = createContext<AIProviderContextType | undefined>(undefined)

export const AIProviderProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  // Load from localStorage or default to 'openai'
  const [selectedProvider, setSelectedProviderState] = useState<string>(() => {
    return localStorage.getItem('ai_provider') || 'openai'
  })
  const [availableProviders, setAvailableProviders] = useState<any>(null)

  // Save to localStorage whenever provider changes
  const setSelectedProvider = (provider: string) => {
    setSelectedProviderState(provider)
    localStorage.setItem('ai_provider', provider)
  }

  return (
    <AIProviderContext.Provider
      value={{
        selectedProvider,
        setSelectedProvider,
        availableProviders,
        setAvailableProviders
      }}
    >
      {children}
    </AIProviderContext.Provider>
  )
}

export const useAIProvider = () => {
  const context = useContext(AIProviderContext)
  if (!context) {
    throw new Error('useAIProvider must be used within AIProviderProvider')
  }
  return context
}
