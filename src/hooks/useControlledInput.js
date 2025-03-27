import { useRef, useLayoutEffect } from 'react';

export default function useControlledInput(value, onChange) {
  // Riferimento alla posizione attuale del cursore
  const cursorPositionRef = useRef(null);
  
  // Riferimento all'elemento input/textarea
  const inputRef = useRef(null);
  
  // Gestore del cambiamento degli input
  const handleChange = (e) => {
    // Salva la posizione del cursore
    cursorPositionRef.current = e.target.selectionStart;
    
    // Chiama la funzione onChange originale
    if (onChange) {
      onChange(e);
    }
  };
  
  // Ripristina la posizione del cursore dopo ogni rendering
  useLayoutEffect(() => {
    const input = inputRef.current;
    if (input && cursorPositionRef.current !== null) {
      input.selectionStart = cursorPositionRef.current;
      input.selectionEnd = cursorPositionRef.current;
    }
  });
  
  return {
    ref: inputRef,
    value,
    onChange: handleChange
  };
}