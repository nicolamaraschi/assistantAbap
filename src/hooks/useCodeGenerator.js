import { useState } from 'react';
import * as codeGenerators from '../utils/codeGenerators';
import { formatAbapCode } from '../utils/formatters';

// Hook personalizzato per la generazione di codice ABAP
const useCodeGenerator = () => {
  const [code, setCode] = useState('* Il codice ABAP apparirà qui');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);

  // Funzione per generare il codice
  const generateCode = (constructType, formData, options = {}) => {
    setIsLoading(true);
    setError(null);
    
    try {
      // Verifica se esiste un generatore per il tipo di costrutto
      if (!codeGenerators[constructType]) {
        throw new Error(`Nessun generatore disponibile per il tipo di costrutto: ${constructType}`);
      }
      
      // Genera il codice utilizzando il generatore appropriato
      let generatedCode = codeGenerators[constructType](formData);
      
      // Applica formattazione se richiesto
      if (options.autoFormat) {
        generatedCode = formatAbapCode(generatedCode);
      }
      
      // Aggiorna lo stato del codice generato
      setCode(generatedCode);
      setIsLoading(false);
      
      return generatedCode;
    } catch (err) {
      setError(err.message);
      setIsLoading(false);
      return null;
    }
  };

  return {
    code,
    setCode,
    isLoading,
    error,
    generateCode
  };
};

export default useCodeGenerator;
