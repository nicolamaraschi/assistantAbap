import { useState, useCallback, useRef, useEffect } from 'react';

/**
 * Hook personalizzato per gestire la posizione del cursore nei campi di input controllati
 */
export default function useCursorPosition() {
  // Mantiene un riferimento agli elementi attivi correnti
  const activeElementRef = useRef(null);
  // Mantiene la posizione del cursore
  const cursorPositionRef = useRef({});

  // Funzione per gestire il cambio del valore nei campi di input
  const handleInputChange = useCallback((e, onChange) => {
    const { name, value, type, checked } = e.target;
    
    // Salva il riferimento all'elemento attivo
    activeElementRef.current = e.target;
    
    // Salva la posizione del cursore
    if (type !== 'checkbox' && type !== 'radio') {
      cursorPositionRef.current[name] = {
        start: e.target.selectionStart,
        end: e.target.selectionEnd
      };
    }
    
    // Chiama la funzione onChange originale
    if (onChange) {
      onChange({
        target: {
          name,
          value: type === 'checkbox' ? checked : value,
          type
        }
      });
    }
  }, []);

  // Effetto per ripristinare la posizione del cursore
  useEffect(() => {
    // Utilizziamo requestAnimationFrame per assicurarci che l'esecuzione avvenga dopo il rendering
    requestAnimationFrame(() => {
      const element = activeElementRef.current;
      if (element && document.activeElement === element) {
        const name = element.name;
        const position = cursorPositionRef.current[name];
        
        if (position) {
          element.selectionStart = position.start;
          element.selectionEnd = position.end;
        }
      }
    });
  });

  return { handleInputChange };
}
