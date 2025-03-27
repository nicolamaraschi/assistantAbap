const fs = require('fs');
const path = require('path');

// Directory dei componenti form
const formsDirectory = path.join(__dirname, 'src/components/forms');

// Leggi tutti i file nella directory
fs.readdir(formsDirectory, (err, files) => {
  if (err) {
    console.error('Errore nella lettura della directory:', err);
    return;
  }

  // Filtra i file JavaScript
  const jsFiles = files.filter(file => file.endsWith('.js'));
  console.log(`Trovati ${jsFiles.length} file JavaScript nella directory dei form`);

  // Per ogni file...
  jsFiles.forEach(file => {
    const filePath = path.join(formsDirectory, file);
    
    // Leggi il contenuto del file
    fs.readFile(filePath, 'utf8', (err, data) => {
      if (err) {
        console.error(`Errore nella lettura del file ${file}:`, err);
        return;
      }

      // Aggiungi le importazioni se non esistono già
      let updatedContent = data;
      
      if (!updatedContent.includes('import ControlledInput')) {
        // Cerca il punto dopo l'ultima importazione
        const lastImportIndex = updatedContent.lastIndexOf('import ');
        let insertIndex = updatedContent.indexOf(';', lastImportIndex) + 1;
        
        if (insertIndex > 0) {
          updatedContent = 
            updatedContent.slice(0, insertIndex) + 
            '\nimport ControlledInput from \'../common/ControlledInput\';\nimport ControlledTextarea from \'../common/ControlledTextarea\';' + 
            updatedContent.slice(insertIndex);
        }
      }

      // Sostituisci textarea con ControlledTextarea - prima del replace per input
      updatedContent = updatedContent.replace(
        /<textarea(\s+[^>]*)>/g, 
        '<ControlledTextarea$1>'
      );
      updatedContent = updatedContent.replace(
        /<\/textarea>/g, 
        '</ControlledTextarea>'
      );

      // Sostituisci input con ControlledInput
      // Esclude input type="checkbox" e type="radio"
      const inputRegex = /<input\s+([^>]*(?:(?!type=['"](?:checkbox|radio)['"])[^>])*(?:type=['"](?!checkbox|radio)[^'"]*['"][^>]*)?)>/g;
      updatedContent = updatedContent.replace(
        inputRegex,
        '<ControlledInput $1>'
      );

      // Scrivi il contenuto aggiornato nel file
      fs.writeFile(filePath, updatedContent, 'utf8', err => {
        if (err) {
          console.error(`Errore nella scrittura del file ${file}:`, err);
          return;
        }
        console.log(`✓ File ${file} aggiornato con successo`);
      });
    });
  });
});
