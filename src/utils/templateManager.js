// Utility per la gestione dei template

// Salva un nuovo template
export const saveTemplate = (templates, data) => {
  const { name, constructType, formData, generatedCode } = data;
  
  // Crea un nuovo template con ID univoco
  const newTemplate = {
    id: Date.now().toString(),
    name,
    constructType,
    formData,
    generatedCode,
    timestamp: new Date().toISOString()
  };
  
  // Aggiungi il nuovo template alla collezione
  return [...templates, newTemplate];
};

// Aggiorna un template esistente
export const updateTemplate = (templates, id, data) => {
  return templates.map(template => {
    if (template.id === id) {
      return {
        ...template,
        ...data,
        timestamp: new Date().toISOString() // Aggiorna il timestamp
      };
    }
    return template;
  });
};

// Elimina un template
export const deleteTemplate = (templates, id) => {
  return templates.filter(template => template.id !== id);
};

// Trova un template per ID
export const findTemplateById = (templates, id) => {
  return templates.find(template => template.id === id);
};

// Filtra template per tipo di costrutto
export const filterTemplatesByType = (templates, constructType) => {
  return templates.filter(template => template.constructType === constructType);
};
