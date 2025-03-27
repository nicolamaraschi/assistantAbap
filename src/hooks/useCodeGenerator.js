import { useState } from 'react';
import { generateCodeByType } from '../utils/codeGenerators';
import { formatAbapCode } from '../utils/formatters';

// Hook personalizzato per la generazione di codice ABAP con debug dettagliato
const useCodeGenerator = () => {
  const [code, setCode] = useState('* Il codice ABAP apparirà qui');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);

  // Funzione per generare il codice con debug avanzato
  const generateCode = (constructType, formData, options = {}) => {
    // Abilita il logging dettagliato per il debug
    console.group(`🔍 Debug Generazione Codice ABAP`);
    console.log('Tipo di costrutto:', constructType);
    console.log('Dati del form:', JSON.parse(JSON.stringify(formData))); // Deep clone per evitare problemi di log
    
    // Reimposta gli stati di errore e caricamento
    setIsLoading(true);
    setError(null);
    
    try {
      // Genera il codice utilizzando la funzione centralizzata
      let generatedCode;
      try {
        generatedCode = generateCodeByType(constructType, formData);
      } catch (generatorError) {
        console.error('❌ Errore nel generatore di codice:', generatorError);
        console.log('Dati passati:', formData);
        
        setError(`Errore nella generazione del codice: ${generatorError.message}`);
        setIsLoading(false);
        return null;
      }

      // Verifica la generazione del codice
      console.log('Codice generato:', generatedCode);
      
      if (!generatedCode) {
        console.warn('⚠️ Codice generato vuoto o undefined');
        setError('Impossibile generare il codice');
        setIsLoading(false);
        return null;
      }

      // Applica formattazione se richiesto
      if (options.autoFormat) {
        try {
          generatedCode = formatAbapCode(generatedCode);
        } catch (formatError) {
          console.error('❌ Errore durante la formattazione:', formatError);
        }
      }
      
      // Aggiorna lo stato con il codice generato
      setCode(generatedCode);
      setIsLoading(false);
      
      console.log('✅ Codice generato con successo');
      console.groupEnd();
      
      return generatedCode;
    } catch (error) {
      console.error('❌ Errore generale:', error);
      console.groupEnd();
      
      setError(`Errore imprevisto: ${error.message}`);
      setIsLoading(false);
      return null;
    }
  };

  return {
    code,         // Codice ABAP generato
    setCode,      // Funzione per impostare manualmente il codice
    isLoading,    // Stato di caricamento
    error,        // Eventuali errori durante la generazione
    generateCode  // Funzione principale per generare il codice
  };
};

export default useCodeGenerator;