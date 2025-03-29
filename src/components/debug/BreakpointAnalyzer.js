// src/components/debug/BreakpointAnalyzer.js
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import { FiSearch, FiCopy, FiCheckCircle } from 'react-icons/fi';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

const BreakpointAnalyzer = () => {
  const [code, setCode] = useState('');
  const [suggestions, setSuggestions] = useState([]);
  const [analyzing, setAnalyzing] = useState(false);
  const [copied, setCopied] = useState(null);
  
  // Accesso al contesto ABAP
  const { generatedCode } = useAbap();
  
  // Aggiornare il codice quando cambia il codice generato
  useEffect(() => {
    if (generatedCode && generatedCode !== '* Il codice ABAP apparirà qui') {
      setCode(generatedCode);
    }
  }, [generatedCode]);
  
  const handleAnalyze = () => {
    setAnalyzing(true);
    
    // Usa setTimeout per non bloccare l'UI
    setTimeout(() => {
      const results = analyzeForBreakpoints(code);
      setSuggestions(results);
      setAnalyzing(false);
    }, 100);
  };
  
  const handleCodeChange = (e) => {
    setCode(e.target.value);
    // Resetta i suggerimenti quando il codice cambia
    setSuggestions([]);
  };
  
  const handleCopy = (suggestion) => {
    // Copia il punto di breakpoint negli appunti
    const breakpointText = `BREAK-POINT. "Linea ${suggestion.line}: ${suggestion.reason}`;
    navigator.clipboard.writeText(breakpointText).then(() => {
      setCopied(suggestion.id);
      setTimeout(() => setCopied(null), 2000);
    });
  };
  
  const handleCopyInstrumented = () => {
    // Genera una copia del codice con i breakpoint inseriti
    let lines = code.split('\n');
    
    // Inseriamo i breakpoint in ordine inverso per evitare di modificare i numeri di riga
    [...suggestions].reverse().forEach(suggestion => {
      if (suggestion.line > 0 && suggestion.line <= lines.length) {
        const indentation = lines[suggestion.line - 1].match(/^\s*/)[0];
        lines.splice(suggestion.line - 1, 0, `${indentation}BREAK-POINT. "Auto-inserted: ${suggestion.reason}`);
      }
    });
    
    navigator.clipboard.writeText(lines.join('\n')).then(() => {
      setCopied('all');
      setTimeout(() => setCopied(null), 2000);
    });
  };
  
  return (
    <AnalyzerContainer>
      <AnalyzerHeader>
        <h3>Analisi Breakpoint</h3>
        <p>Analizza il tuo codice ABAP per trovare i punti ottimali dove inserire breakpoint per il debug.</p>
      </AnalyzerHeader>
      
      <CodeTextarea 
        value={code}
        onChange={handleCodeChange}
        placeholder="Incolla qui il tuo codice ABAP..."
        rows={15}
      />
      
      <AnalyzeButtonRow>
        <Button 
          variant="primary"
          icon={<FiSearch />}
          onClick={handleAnalyze}
          disabled={!code || analyzing}
        >
          {analyzing ? 'Analisi in corso...' : 'Analizza per Breakpoint'}
        </Button>
        
        {suggestions.length > 0 && (
          <Button
            variant="outline"
            onClick={handleCopyInstrumented}
          >
            {copied === 'all' ? 'Copiato!' : 'Copia Codice con Breakpoint'}
          </Button>
        )}
      </AnalyzeButtonRow>
      
      {suggestions.length > 0 && (
        <ResultsContainer>
          <ResultsHeader>
            <h4>Breakpoint suggeriti {`(${suggestions.length})`}</h4>
            <BreakpointLegend>
              <LegendItem color="#e6f7ff">Operazioni DB</LegendItem>
              <LegendItem color="#fff2e8">Loop/Cicli</LegendItem>
              <LegendItem color="#f6ffed">Logica condizionale</LegendItem>
              <LegendItem color="#f9f0ff">Operazioni dati</LegendItem>
            </BreakpointLegend>
          </ResultsHeader>
          
          <BreakpointsList>
            {suggestions.map((sugg) => (
              <BreakpointItem 
                key={sugg.id} 
                type={getBreakpointType(sugg.reason)}
              >
                <BreakpointInfo>
                  <BreakpointLine>Linea {sugg.line}</BreakpointLine>
                  <BreakpointCode>{sugg.code}</BreakpointCode>
                </BreakpointInfo>
                <BreakpointDetails>
                  <BreakpointReason>{sugg.reason}</BreakpointReason>
                  <BreakpointCopy onClick={() => handleCopy(sugg)}>
                    {copied === sugg.id ? <FiCheckCircle /> : <FiCopy />}
                  </BreakpointCopy>
                </BreakpointDetails>
              </BreakpointItem>
            ))}
          </BreakpointsList>
          
          <SuggestionTip>
            <strong>SUGGERIMENTO:</strong> Posiziona i breakpoint prima delle istruzioni indicate per verificare i valori delle variabili prima dell'esecuzione.
          </SuggestionTip>
        </ResultsContainer>
      )}
    </AnalyzerContainer>
  );
};

// Funzione per analizzare il codice ABAP per i breakpoint
const analyzeForBreakpoints = (code) => {
  const lines = code.split('\n');
  const suggestions = [];
  
  // Array di pattern da cercare con priorità assegnate
  const patterns = [
    { regex: /^\s*(SELECT|UPDATE|DELETE|INSERT|MODIFY)/i, reason: "Operazione sul database", priority: 10 },
    { regex: /^\s*(COMMIT WORK|ROLLBACK WORK)/i, reason: "Operazione transazionale", priority: 9 },
    { regex: /^\s*(CALL FUNCTION|CALL METHOD|CALL TRANSFORMATION)/i, reason: "Chiamata esterna", priority: 8 },
    { regex: /^\s*(TRY)/i, reason: "Inizio gestione errori", priority: 7 },
    { regex: /^\s*(CATCH)/i, reason: "Gestione eccezione", priority: 7 },
    { regex: /^\s*(RAISE EXCEPTION|THROW)/i, reason: "Sollevamento eccezione", priority: 7 },
    { regex: /^\s*(LOOP AT|DO|WHILE)/i, reason: "Inizio di un ciclo", priority: 6 },
    { regex: /^\s*(READ TABLE)/i, reason: "Lettura tabella interna", priority: 6 },
    { regex: /^\s*(IF|CASE)/i, reason: "Decisione condizionale", priority: 5 },
    { regex: /^\s*(ELSEIF|ELSE|WHEN)/i, reason: "Ramo condizionale", priority: 4 },
    { regex: /^\s*(SORT|COLLECT|DELETE)/i, reason: "Operazione su tabella interna", priority: 3 },
    { regex: /^\s*(CONCATENATE|SPLIT|TRANSLATE|CONDENSE)/i, reason: "Manipolazione stringhe", priority: 2 },
    { regex: /^\s*(MOVE|CLEAR|FREE|APPEND)/i, reason: "Manipolazione dati", priority: 1 }
  ];
  
  // Analisi delle linee più complesse per capire le variabili chiave
  const keyVariables = extractKeyVariables(code);
  
  // Analizza ogni riga di codice
  lines.forEach((line, index) => {
    for (const pattern of patterns) {
      if (pattern.regex.test(line)) {
        // Determina se questa riga usa variabili chiave
        const usesKeyVariables = keyVariables.some(varName => 
          line.toLowerCase().includes(varName.toLowerCase())
        );
        
        const suggestion = {
          id: `bp-${index + 1}`,
          line: index + 1,
          code: line.trim(),
          reason: pattern.reason,
          priority: pattern.priority + (usesKeyVariables ? 3 : 0)
        };
        
        // Cerca di ottenere un po' di contesto
        if (index > 0) {
          suggestion.context = lines[index - 1].trim();
        }
        
        suggestions.push(suggestion);
        break;
      }
    }
  });
  
  // Effettua una ricerca più approfondita su assegnazioni a variabili chiave
  lines.forEach((line, index) => {
    if (keyVariables.length > 0) {
      keyVariables.forEach(varName => {
        if (line.match(new RegExp(`\\b${varName}\\s*=`))) {
          suggestions.push({
            id: `bp-key-${index + 1}`,
            line: index + 1,
            code: line.trim(),
            reason: `Assegnazione a variabile chiave '${varName}'`,
            priority: 8
          });
        }
      });
    }
  });
  
  // Ottimizza i suggerimenti ordinandoli per priorità e rimuovendo duplicati vicini
  const optimizedSuggestions = optimizeBreakpoints(suggestions);
  
  return optimizedSuggestions;
};

// Funzione per ottimizzare i punti di breakpoint
const optimizeBreakpoints = (suggestions) => {
  // Ordina per priorità
  const sorted = [...suggestions].sort((a, b) => b.priority - a.priority);
  
  const optimized = [];
  const distanceThreshold = 3; // Numero minimo di linee tra i breakpoint
  
  // Filtra i breakpoint troppo vicini tra loro
  sorted.forEach(suggestion => {
    // Controlla se questo breakpoint è troppo vicino a uno già incluso
    const tooClose = optimized.some(existing => 
      Math.abs(existing.line - suggestion.line) < distanceThreshold
    );
    
    // Se non è troppo vicino o è ad alta priorità, includilo
    if (!tooClose || suggestion.priority >= 8) {
      optimized.push(suggestion);
    }
  });
  
  // Limita a 8 breakpoint per non sovraccaricare l'utente
  return optimized.slice(0, 8).sort((a, b) => a.line - b.line);
};

// Estrae le variabili chiave che sembrano importanti nel codice
const extractKeyVariables = (code) => {
  const keyVars = [];
  
  // Cerca variabili che vengono usate in condizioni IF o confronti
  const ifConditions = code.match(/IF\s+([a-z0-9_-]+)(?:\s*[=<>]|\s+IS)/ig) || [];
  ifConditions.forEach(match => {
    const varName = match.replace(/IF\s+/i, '').split(/\s+/)[0].trim();
    if (varName && !keyVars.includes(varName)) {
      keyVars.push(varName);
    }
  });
  
  // Cerca variabili utilizzate in loop
  const loopVars = code.match(/LOOP AT\s+([a-z0-9_-]+)/ig) || [];
  loopVars.forEach(match => {
    const varName = match.replace(/LOOP AT\s+/i, '').split(/\s+/)[0].trim();
    if (varName && !keyVars.includes(varName)) {
      keyVars.push(varName);
    }
  });
  
  // Cerca variabili che sembrano essere parametri di output
  const exportVars = code.match(/EXPORTING\s+(?:[a-z0-9_-]+\s*=\s*)?([a-z0-9_-]+)/ig) || [];
  exportVars.forEach(match => {
    const varName = match.replace(/EXPORTING\s+/i, '').split(/\s*=\s*/)[0].trim();
    if (varName && !keyVars.includes(varName)) {
      keyVars.push(varName);
    }
  });
  
  // Limita il numero di variabili chiave
  return keyVars.slice(0, 5);
};

// Determina il tipo di breakpoint per lo stile
const getBreakpointType = (reason) => {
  if (reason.toLowerCase().includes('database') || reason.toLowerCase().includes('transazionale')) {
    return 'database';
  }
  if (reason.toLowerCase().includes('ciclo') || reason.toLowerCase().includes('loop')) {
    return 'loop';
  }
  if (reason.toLowerCase().includes('condizionale') || reason.toLowerCase().includes('decisione')) {
    return 'condition';
  }
  return 'data';
};

// Stili del componente
const AnalyzerContainer = styled.div`
  display: flex;
  flex-direction: column;
  gap: 15px;
`;

const AnalyzerHeader = styled.div`
  h3 {
    margin-top: 0;
    margin-bottom: 10px;
  }
  
  p {
    margin-top: 0;
    color: #666;
  }
`;

const CodeTextarea = styled.textarea`
  width: 100%;
  padding: 15px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-family: 'Fira Code', 'Courier New', monospace;
  font-size: 14px;
  line-height: 1.5;
  resize: vertical;
`;

const AnalyzeButtonRow = styled.div`
  display: flex;
  gap: 10px;
`;

const ResultsContainer = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-top: 10px;
`;

const ResultsHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
  
  h4 {
    margin: 0;
    font-size: 16px;
  }
`;

const BreakpointLegend = styled.div`
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
`;

const LegendItem = styled.div`
  display: flex;
  align-items: center;
  font-size: 12px;
  
  &::before {
    content: "";
    display: inline-block;
    width: 12px;
    height: 12px;
    margin-right: 5px;
    background-color: ${props => props.color};
    border: 1px solid rgba(0, 0, 0, 0.1);
    border-radius: 2px;
  }
`;

const BreakpointsList = styled.div`
  display: flex;
  flex-direction: column;
  gap: 10px;
  max-height: 400px;
  overflow-y: auto;
`;

const BreakpointItem = styled.div`
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px;
  border-radius: 4px;
  border-left: 3px solid;
  background-color: ${props => {
    switch (props.type) {
      case 'database':
        return '#e6f7ff'; // Light blue
      case 'loop':
        return '#fff2e8'; // Light orange
      case 'condition': 
        return '#f6ffed'; // Light green
      case 'data':
      default:
        return '#f9f0ff'; // Light purple
    }
  }};
  
  border-color: ${props => {
    switch (props.type) {
      case 'database':
        return '#1890ff'; // Blue
      case 'loop':
        return '#fa8c16'; // Orange
      case 'condition': 
        return '#52c41a'; // Green
      case 'data':
      default:
        return '#722ed1'; // Purple
    }
  }};
`;

const BreakpointInfo = styled.div`
  display: flex;
  gap: 15px;
`;

const BreakpointLine = styled.div`
  font-weight: bold;
  min-width: 70px;
  padding: 3px 6px;
  background: rgba(0, 0, 0, 0.05);
  border-radius: 3px;
  font-size: 13px;
`;

const BreakpointCode = styled.div`
  font-family: 'Fira Code', 'Courier New', monospace;
  flex: 1;
  overflow-x: auto;
  white-space: nowrap;
  background: rgba(255, 255, 255, 0.7);
  padding: 3px 6px;
  border-radius: 3px;
  font-size: 13px;
`;

const BreakpointDetails = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const BreakpointReason = styled.div`
  font-style: italic;
  color: #666;
  font-size: 13px;
`;

const BreakpointCopy = styled.div`
  cursor: pointer;
  color: #1890ff;
  display: flex;
  align-items: center;
  padding: 3px 8px;
  border-radius: 3px;
  transition: background-color 0.2s;
  
  &:hover {
    background-color: rgba(24, 144, 255, 0.1);
  }
`;

const SuggestionTip = styled.div`
  margin-top: 15px;
  padding: 10px 12px;
  background: #e6f7ff;
  border-radius: 4px;
  border-left: 4px solid #1890ff;
  color: #333;
  font-size: 13px;
  line-height: 1.5;
`;

export default BreakpointAnalyzer;