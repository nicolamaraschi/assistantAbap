import { useState, useCallback } from 'react';
import { TOAST_TYPES } from '../components/common/Toast';

// Hook per gestire i toast nell'applicazione
const useToast = () => {
  const [toasts, setToasts] = useState([]);
  
  // Aggiunge un nuovo toast
  const addToast = useCallback(({ message, type = TOAST_TYPES.INFO, duration = 3000 }) => {
    const id = Date.now().toString();
    setToasts(prevToasts => [...prevToasts, { id, message, type, duration }]);
    return id;
  }, []);
  
  // Rimuove un toast specifico
  const removeToast = useCallback(id => {
    setToasts(prevToasts => prevToasts.filter(toast => toast.id !== id));
  }, []);
  
  // Helper per i tipi comuni di toast
  const showSuccess = useCallback(message => addToast({ message, type: TOAST_TYPES.SUCCESS }), [addToast]);
  const showError = useCallback(message => addToast({ message, type: TOAST_TYPES.ERROR }), [addToast]);
  const showWarning = useCallback(message => addToast({ message, type: TOAST_TYPES.WARNING }), [addToast]);
  const showInfo = useCallback(message => addToast({ message, type: TOAST_TYPES.INFO }), [addToast]);
  
  return {
    toasts,
    addToast,
    removeToast,
    showSuccess,
    showError,
    showWarning,
    showInfo
  };
};

export default useToast;
