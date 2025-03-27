import { useState, useCallback } from 'react';
import useLocalStorage from './useLocalStorage';

const useGenerationHistory = () => {
  // Usa localStorage per persistere la cronologia
  const [history, setHistory] = useLocalStorage('abap-generation-history', []);
  const [searchResults, setSearchResults] = useState([]);
  
  // Aggiungi un elemento alla cronologia
  const addToHistory = useCallback((item) => {
    const historyItem = {
      ...item,
      id: Date.now().toString(),
      timestamp: new Date().toISOString()
    };
    
    setHistory(prev => [historyItem, ...prev].slice(0, 50)); // Limita a 50 elementi
  }, [setHistory]);
  
  // Pulisci tutta la cronologia
  const clearHistory = useCallback(() => {
    if (window.confirm('Sei sicuro di voler cancellare tutta la cronologia?')) {
      setHistory([]);
      setSearchResults([]);
    }
  }, [setHistory]);
  
  // Cerca nella cronologia
  const searchHistory = useCallback((term) => {
    if (!term) {
      setSearchResults([]);
      return;
    }
    
    const results = history.filter(item => 
      item.description.toLowerCase().includes(term.toLowerCase()) ||
      item.constructType.toLowerCase().includes(term.toLowerCase()) ||
      (item.generatedCode && item.generatedCode.toLowerCase().includes(term.toLowerCase()))
    );
    
    setSearchResults(results);
  }, [history]);
  
  return {
    history,
    searchResults: searchResults.length > 0 ? searchResults : history,
    addToHistory,
    clearHistory,
    searchHistory
  };
};

export default useGenerationHistory;