// Analizza il codice ABAP per suggerire breakpoint strategici
export const analyzeForBreakpoints = (code) => {
    const lines = code.split('\n');
    const suggestions = [];
    
    // Array di pattern da cercare
    const patterns = [
      { regex: /^\s*(SELECT|UPDATE|DELETE|INSERT|MODIFY)/i, reason: "Operazione sul database" },
      { regex: /^\s*(LOOP AT|DO|WHILE)/i, reason: "Inizio di un ciclo" },
      { regex: /^\s*(CALL FUNCTION|CALL METHOD)/i, reason: "Chiamata di funzione/metodo" },
      { regex: /^\s*(TRY)/i, reason: "Inizio blocco di gestione errori" },
      { regex: /^\s*(IF|CASE)/i, reason: "Inizio logica condizionale" },
      { regex: /^\s*(MOVE|CLEAR|FREE|APPEND|COLLECT)/i, reason: "Manipolazione dati" },
      { regex: /^\s*(READ TABLE|SORT|DELETE)/i, reason: "Operazione su tabella interna" },
      { regex: /^\s*(COMMIT WORK|ROLLBACK WORK)/i, reason: "Operazione transazionale" }
    ];
    
    // Analizza ogni riga di codice
    lines.forEach((line, index) => {
      for (const pattern of patterns) {
        if (pattern.regex.test(line)) {
          suggestions.push({
            line: index + 1,
            code: line.trim(),
            reason: pattern.reason
          });
          break;
        }
      }
    });
    
    // Filtra le suggestioni per evitare troppi breakpoint ravvicinati
    return optimizeBreakpoints(suggestions);
  };
  
  // Ottimizza i suggerimenti per non avere troppi breakpoint
  const optimizeBreakpoints = (suggestions) => {
    if (suggestions.length <= 5) return suggestions;
    
    // Se ci sono più di 5 suggerimenti, seleziona i più rilevanti o distribuiscili
    const optimized = [];
    
    // Prioritizza operazioni critiche
    const critical = suggestions.filter(s => 
      s.reason.includes("database") || 
      s.reason.includes("transazionale")
    );
    
    // Aggiungi tutte le operazioni critiche
    optimized.push(...critical);
    
    // Distribuisci gli altri breakpoint in modo uniforme
    const remaining = suggestions.filter(s => !critical.includes(s));
    const step = Math.max(1, Math.floor(remaining.length / (5 - critical.length)));
    
    for (let i = 0; i < remaining.length; i += step) {
      if (optimized.length < 5) {
        optimized.push(remaining[i]);
      }
    }
    
    return optimized.sort((a, b) => a.line - b.line);
  };