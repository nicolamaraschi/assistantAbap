// Utilità per formattare il codice ABAP

// Formatta il codice ABAP
export const formatAbapCode = (code) => {
  // Questa è una versione semplificata di un formattatore ABAP
  // In una versione completa, implementeresti regole di formattazione più dettagliate
  
  // Esempio di formattazione base: regola indentazione
  const lines = code.split('\n');
  let formattedLines = [];
  let indentLevel = 0;
  
  for (let line of lines) {
    // Rimozione degli spazi in eccesso
    line = line.trim();
    
    // Riduce l'indentazione per le linee che chiudono un blocco
    if (line.match(/^END(IF|CASE|LOOP|DO|WHILE|TRY|FORM|CLASS|METHOD|MODULE)\.?$/)) {
      indentLevel = Math.max(0, indentLevel - 1);
    }
    
    // Applica l'indentazione corrente
    const formattedLine = ' '.repeat(indentLevel * 2) + line;
    formattedLines.push(formattedLine);
    
    // Aumenta l'indentazione per le linee che aprono un blocco
    if (line.match(/^(IF|CASE|LOOP|DO|WHILE|TRY|FORM|CLASS|METHOD|MODULE)/) && 
        !line.match(/^(CLASS|METHOD|MODULE).*\.$/)) {
      indentLevel++;
    }
    
    // Aumenta l'indentazione per ELSE e ELSEIF
    if (line.match(/^(ELSE|ELSEIF|CATCH|CLEANUP)/)) {
      indentLevel++;
    }
  }
  
  return formattedLines.join('\n');
};

// Aggiungi numeri di riga al codice
export const addLineNumbers = (code) => {
  const lines = code.split('\n');
  return lines.map((line, index) => {
    const lineNumber = (index + 1).toString().padStart(3, '0');
    return `${lineNumber}: ${line}`;
  }).join('\n');
};

// Evidenziazione sintassi ABAP (versione semplificata)
export const highlightAbapSyntax = (code) => {
  // In una vera implementazione useresti una libreria come react-syntax-highlighter
  // Questa è una semplificazione per scopi dimostrativi
  
  const keywords = [
    'IF', 'ELSE', 'ELSEIF', 'ENDIF', 'CASE', 'WHEN', 'OTHERS', 'ENDCASE',
    'LOOP', 'ENDLOOP', 'DO', 'ENDDO', 'WHILE', 'ENDWHILE', 'TRY', 'CATCH',
    'CLEANUP', 'ENDTRY', 'SELECT', 'FROM', 'WHERE', 'GROUP', 'ORDER', 'INSERT',
    'UPDATE', 'DELETE', 'MODIFY', 'CLASS', 'ENDCLASS', 'METHOD', 'ENDMETHOD',
    'FORM', 'ENDFORM', 'TYPES', 'DATA', 'CONSTANTS', 'FIELD-SYMBOLS'
  ];
  
  let highlightedCode = code;
  
  // Evidenzia le parole chiave
  for (const keyword of keywords) {
    const regex = new RegExp(`\\b${keyword}\\b`, 'g');
    highlightedCode = highlightedCode.replace(regex, `<span class="keyword">${keyword}</span>`);
  }
  
  // Evidenzia stringhe
  highlightedCode = highlightedCode.replace(/'[^']*'/g, match => `<span class="string">${match}</span>`);
  
  // Evidenzia commenti
  highlightedCode = highlightedCode.replace(/\*.*$/gm, match => `<span class="comment">${match}</span>`);
  
  return highlightedCode;
};
