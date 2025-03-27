import React, { useState } from 'react';
import styled from 'styled-components';
import { analyzeForBreakpoints } from '../../utils/breakpointAnalyzer';
import Button from '../common/Button';
import { FiSearch } from 'react-icons/fi';

const BreakpointAnalyzer = () => {
  const [code, setCode] = useState('');
  const [suggestions, setSuggestions] = useState([]);
  const [analyzing, setAnalyzing] = useState(false);
  
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
  
  return (
    <AnalyzerContainer>
      <h3>Analisi Breakpoint</h3>
      <p>Incolla il tuo codice ABAP qui sotto per ricevere suggerimenti su dove posizionare i breakpoint per un debug efficace.</p>
      
      <CodeTextarea 
        value={code}
        onChange={handleCodeChange}
        placeholder="Incolla qui il tuo codice ABAP..."
        rows={15}
      />
      
      <AnalyzeButton 
        variant="primary"
        icon={<FiSearch />}
        onClick={handleAnalyze}
        disabled={!code || analyzing}
      >
        {analyzing ? 'Analisi in corso...' : 'Analizza per Breakpoint'}
      </AnalyzeButton>
      
      {suggestions.length > 0 && (
        <ResultsContainer>
          <h4>Breakpoint suggeriti:</h4>
          <BreakpointsList>
            {suggestions.map((sugg, index) => (
              <BreakpointItem key={index}>
                <BreakpointLine>Linea {sugg.line}</BreakpointLine>
                <BreakpointCode>{sugg.code}</BreakpointCode>
                <BreakpointReason>{sugg.reason}</BreakpointReason>
              </BreakpointItem>
            ))}
          </BreakpointsList>
          <SuggestionTip>
            Suggerimento: Posiziona i breakpoint prima delle istruzioni indicate per verificare i valori delle variabili prima dell'esecuzione.
          </SuggestionTip>
        </ResultsContainer>
      )}
    </AnalyzerContainer>
  );
};

// Stili del componente
const AnalyzerContainer = styled.div`
  margin: 20px 0;
`;

const CodeTextarea = styled.textarea`
  width: 100%;
  padding: 15px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-family: 'Fira Code', 'Courier New', monospace;
  font-size: 14px;
  margin-bottom: 15px;
`;

const AnalyzeButton = styled(Button)`
  margin-bottom: 20px;
`;

const ResultsContainer = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
`;

const BreakpointsList = styled.div`
  margin-top: 10px;
`;

const BreakpointItem = styled.div`
  padding: 10px;
  margin-bottom: 10px;
  border-left: 3px solid #0066cc;
  background: white;
  border-radius: 0 4px 4px 0;
  display: flex;
  align-items: center;
`;

const BreakpointLine = styled.div`
  font-weight: bold;
  width: 80px;
  margin-right: 15px;
`;

const BreakpointCode = styled.div`
  font-family: 'Fira Code', 'Courier New', monospace;
  flex: 1;
  padding: 5px;
  background: #f0f0f0;
  border-radius: 3px;
  margin-right: 15px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
`;

const BreakpointReason = styled.div`
  color: #666;
  font-style: italic;
  width: 180px;
`;

const SuggestionTip = styled.div`
  margin-top: 15px;
  padding: 10px;
  background: #e7f3ff;
  border-radius: 4px;
  font-style: italic;
`;

export default BreakpointAnalyzer;