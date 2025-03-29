import React, { createContext, useContext, useState, useRef } from 'react';
import useLocalStorage from '../hooks/useLocalStorage';

// Creazione del contesto
const AbapContext = createContext();

// Provider del contesto
export const AbapProvider = ({ children }) => {
  // Stato per il tipo di costrutto selezionato
  const [selectedConstructType, setSelectedConstructType] = useState('if-else');
  
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
  
  // Usa useRef invece di useState per lo stato del form per evitare rirender
  const formStateRef = useRef({});
  
  // Funzione per aggiornare lo stato del form senza causare rirender
  const updateFormState = (constructType, newState) => {
    // Aggiorna solo lo stato nella ref, SENZA salvare in localStorage
    formStateRef.current = {
      ...formStateRef.current,
      [constructType]: newState
    };
  };
  
  // Funzione per leggere lo stato di un form specifico
  const getFormState = (constructType) => {
    return formStateRef.current[constructType] || null;
  };
  
  // Funzione per resettare lo stato del form di un tipo specifico
  const resetFormState = (constructType) => {
    // Crea un nuovo stato vuoto
    formStateRef.current = {
      ...formStateRef.current,
      [constructType]: {}
    };
    
    return {};
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
    generatedCode,
    setGeneratedCode,
    settings,
    updateSettings,
    activeTab,
    setActiveTab,
    getFormState,
    updateFormState,
    resetFormState,
    formState: formStateRef.current // Esponi lo stato attuale come oggetto non reattivo
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