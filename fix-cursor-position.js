const fs = require('fs');
const path = require('path');

const formsDirectory = path.join(__dirname, 'src/components/forms');

// Leggi tutti i file nella directory dei form
fs.readdir(formsDirectory, (err, files) => {
  if (err) {
    console.error('Errore nella lettura della directory:', err);
    return;
  }

  // Filtra solo i file JavaScript
  const jsFiles = files.filter(file => file.endsWith('.js') && file !== 'IfElseForm.js');
  console.log(`Trovati ${jsFiles.length} file da processare`);

  jsFiles.forEach(file => {
    const filePath = path.join(formsDirectory, file);
    
    fs.readFile(filePath, 'utf8', (err, data) => {
      if (err) {
        console.error(`Errore nella lettura del file ${file}:`, err);
        return;
      }

      let updatedContent = data;
      
      // Aggiungi l'importazione del hook
      if (!updatedContent.includes('useCursorPosition')) {
        updatedContent = updatedContent.replace(
          /import React.*/,
          `import React, { useState, useEffect } from 'react';\nimport useCursorPosition from '../../hooks/useCursorPosition';`
        );
      }
      
      // Aggiungi la dichiarazione del hook
      if (!updatedContent.includes('handleInputChange')) {
        updatedContent = updatedContent.replace(
          /const \[\w+, set\w+\] = useState\([\s\S]*?\);/,
          match => `${match}\n\n  // Hook per gestire la posizione del cursore\n  const { handleInputChange } = useCursorPosition();`
        );
      }
      
      // Sostituisci il gestore di cambiamento
      if (!updatedContent.includes('handleInputChange')) {
        updatedContent = updatedContent.replace(
          /const handleChange = \(e\) => {[\s\S]*?};/,
          `const handleChange = (e) => {
    handleInputChange(e, (newE) => {
      const { name, value, type } = newE.target;
      setFormData({
        ...formData,
        [name]: type === 'checkbox' ? newE.target.checked : value
      });
    });
  };`
        );
      }
      
      // Correggi la gestione dei checkbox
      updatedContent = updatedContent.replace(
        /onChange={handleChange}/g,
        (match, offset) => {
          // Controlla se c'è type="checkbox" nelle vicinanze
          const surroundingText = updatedContent.substring(Math.max(0, offset - 50), Math.min(updatedContent.length, offset + 50));
          if (surroundingText.includes('type="checkbox"')) {
            return 'onChange={(e) => setFormData({...formData, [e.target.name]: e.target.checked})}';
          }
          return match;
        }
      );

      fs.writeFile(filePath, updatedContent, 'utf8', err => {
        if (err) {
          console.error(`Errore nella scrittura del file ${file}:`, err);
          return;
        }
        console.log(`File ${file} aggiornato con successo`);
      });
    });
  });
});
