import { useEffect } from 'react';
import useControlledInput from '../../hooks/useControlledInput';

// Questa è una soluzione di ripiego (hack) che modifica il comportamento degli elementi nativi
function MonkeyPatch() {
  useEffect(() => {
    // Salva i metodi originali
    const originalInputOnChange = HTMLInputElement.prototype.addEventListener;
    const originalTextareaOnChange = HTMLTextAreaElement.prototype.addEventListener;
    
    // Funzione per gestire l'evento
    const handleEvent = function(type, listener, options) {
      if (type === 'input' || type === 'change') {
        // Posizione corrente del cursore
        const cursorPosition = this.selectionStart;
        
        // Avvolge il listener originale per preservare la posizione del cursore
        const wrappedListener = (e) => {
          listener.call(this, e);
          
          // Ripristina la posizione del cursore
          setTimeout(() => {
            if (document.activeElement === this) {
              this.selectionStart = cursorPosition;
              this.selectionEnd = cursorPosition;
            }
          }, 0);
        };
        
        // Chiama il metodo originale con il listener avvolto
        return originalInputOnChange.call(this, type, wrappedListener, options);
      }
      
      // Per tutti gli altri eventi, comportamento normale
      if (this.tagName === 'INPUT') {
        return originalInputOnChange.call(this, type, listener, options);
      } else {
        return originalTextareaOnChange.call(this, type, listener, options);
      }
    };
    
    // Sostituisci i metodi originali
    HTMLInputElement.prototype.addEventListener = handleEvent;
    HTMLTextAreaElement.prototype.addEventListener = handleEvent;
    
    // Ripristina i metodi originali alla pulizia
    return () => {
      HTMLInputElement.prototype.addEventListener = originalInputOnChange;
      HTMLTextAreaElement.prototype.addEventListener = originalTextareaOnChange;
    };
  }, []);
  
  return null;
}

export default MonkeyPatch;