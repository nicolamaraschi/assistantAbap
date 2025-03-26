import React, { createContext, useContext, useState, useEffect } from 'react';
import useLocalStorage from '../hooks/useLocalStorage';

// Creazione del contesto
const AbapContext = createContext();

// Provider del contesto
export const AbapProvider = ({ children }) => {
  // Stato per il tipo di costrutto selezionato
  const [selectedConstructType, setSelectedConstructType] = useState('if-else');
  
  // Stato per la gestione dei preferiti
  const [favorites, setFavorites] = useLocalStorage('abap-favorites', []);
  
  // Stato per i modelli salvati
  const [savedTemplates, setSavedTemplates] = useLocalStorage('abap-templates', []);
  
  // Stato per visualizzare il codice generato
  const [generatedCode, setGeneratedCode] = useState('* Il codice ABAP apparirà qui');
  
  // Stato per le impostazioni
  const [settings, setSettings] = useLocalStorage('abap-settings', {
    autoFormat: true,
    showLineNumbers: true,
    syntaxHighlighting: true,
    theme: 'dark'
  });

  // Gestione della tab attiva
  const [activeTab, setActiveTab] = useState('standard');
  
  // Stato del form per ogni tipo di costrutto
  const [formState, setFormState] = useState({});
  
  // Aggiorna lo stato del form per un determinato tipo di costrutto
  const updateFormState = (constructType, newState) => {
    setFormState(prevState => ({
      ...prevState,
      [constructType]: {
        ...prevState[constructType],
        ...newState
      }
    }));
  };
  
  // Aggiungi un preferito
  const addToFavorites = (constructType) => {
    if (!favorites.some(fav => fav.id === constructType.id)) {
      setFavorites([...favorites, constructType]);
    }
  };
  
  // Rimuovi un preferito
  const removeFromFavorites = (constructTypeId) => {
    setFavorites(favorites.filter(fav => fav.id !== constructTypeId));
  };
  
  // Salva un modello
  const saveTemplate = (name, constructType, formData) => {
    const template = {
      id: Date.now().toString(),
      name,
      constructType,
      formData,
      timestamp: new Date().toISOString()
    };
    
    setSavedTemplates([...savedTemplates, template]);
  };
  
  // Aggiorna le impostazioni
  const updateSettings = (newSettings) => {
    setSettings({
      ...settings,
      ...newSettings
    });
  };

  // Valori esposti dal contesto
  const value = {
    selectedConstructType,
    setSelectedConstructType,
    favorites,
    addToFavorites,
    removeFromFavorites,
    savedTemplates,
    saveTemplate,
    generatedCode,
    setGeneratedCode,
    settings,
    updateSettings,
    activeTab,
    setActiveTab,
    formState,
    updateFormState
  };

  return (
    <AbapContext.Provider value={value}>
      {children}
    </AbapContext.Provider>
  );
};

// Custom hook per utilizzare il contesto
export const useAbap = () => {
  const context = useContext(AbapContext);
  if (context === undefined) {
    throw new Error('useAbap must be used within an AbapProvider');
  }
  return context;
};

export default AbapContext;
