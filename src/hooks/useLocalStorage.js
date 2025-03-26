import { useState, useEffect } from 'react';

// Hook personalizzato per utilizzare localStorage
const useLocalStorage = (key, initialValue) => {
  // Funzione per ottenere il valore iniziale da localStorage o utilizzare quello fornito
  const initialize = () => {
    try {
      const item = localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error('Error reading from localStorage:', error);
      return initialValue;
    }
  };

  // Stato React che utilizza il valore inizializzato
  const [storedValue, setStoredValue] = useState(initialize);

  // Funzione per aggiornare il valore in localStorage e nello stato
  const setValue = (value) => {
    try {
      // Consente al valore di essere una funzione per mantenere la stessa API di useState
      const valueToStore = value instanceof Function ? value(storedValue) : value;
      
      // Aggiorna lo stato React
      setStoredValue(valueToStore);
      
      // Aggiorna localStorage
      localStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error('Error saving to localStorage:', error);
    }
  };

  // Effetto per sincronizzare con altri componenti che usano lo stesso key
  useEffect(() => {
    const handleStorageChange = (e) => {
      if (e.key === key) {
        try {
          setStoredValue(e.newValue ? JSON.parse(e.newValue) : initialValue);
        } catch (error) {
          console.error('Error parsing storage event value:', error);
        }
      }
    };

    // Ascolto degli eventi di cambiamento in localStorage
    window.addEventListener('storage', handleStorageChange);
    
    // Cleanup dell'event listener
    return () => {
      window.removeEventListener('storage', handleStorageChange);
    };
  }, [key, initialValue]);

  return [storedValue, setValue];
};

export default useLocalStorage;
