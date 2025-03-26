import { useState, useEffect } from 'react';
import { useAbap } from '../context/AbapContext';

// Hook personalizzato per gestire le impostazioni
const useSettings = () => {
  const { settings, updateSettings } = useAbap();
  const [localSettings, setLocalSettings] = useState(settings);

  // Aggiorna le impostazioni locali quando cambiano nel context
  useEffect(() => {
    setLocalSettings(settings);
  }, [settings]);

  // Funzione per aggiornare le impostazioni
  const handleUpdateSettings = (newSettings) => {
    updateSettings(newSettings);
    setLocalSettings({
      ...localSettings,
      ...newSettings
    });
  };

  return {
    settings: localSettings,
    updateSettings: handleUpdateSettings
  };
};

export default useSettings;