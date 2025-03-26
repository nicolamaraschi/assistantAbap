import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { tomorrow, prism } from 'react-syntax-highlighter/dist/esm/styles/prism';
import { FiCopy, FiDownload, FiSave, FiCheck } from 'react-icons/fi';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

// Componente per visualizzare l'anteprima del codice generato
const CodePreview = ({ code, onSaveTemplate }) => {
  const { settings, selectedConstructType, formState } = useAbap();
  const [copied, setCopied] = useState(false);
  const [highlightedCode, setHighlightedCode] = useState(code);
  
  // Aggiorna il codice quando cambia
  useEffect(() => {
    setHighlightedCode(code);
  }, [code]);
  
  // Gestisce la copia negli appunti
  const handleCopy = () => {
    navigator.clipboard.writeText(code)
      .then(() => {
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
      })
      .catch(err => {
        console.error('Impossibile copiare il codice:', err);
      });
  };
  
  // Gestisce il download come file di testo
  const handleDownload = () => {
    const blob = new Blob([code], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'codice_abap.txt';
    a.click();
    URL.revokeObjectURL(url);
  };
  
  // Gestisce il salvataggio come template
  const handleSaveTemplate = () => {
    const name = prompt('Inserisci un nome per questo template:');
    if (name && onSaveTemplate) {
      // Ottieni i dati del form per il tipo di costrutto selezionato
      const formData = formState[selectedConstructType] || {};
      
      // Salva il template con il codice generato
      onSaveTemplate(name, formData);
    }
  };
  
  return (
    <PreviewContainer>
      <PreviewHeader>
        <h3>Anteprima Codice</h3>
        <ActionsContainer>
          <Button 
            variant="text" 
            size="small" 
            icon={copied ? <FiCheck /> : <FiCopy />} 
            onClick={handleCopy}
            title="Copia negli appunti"
          >
            {copied ? 'Copiato!' : 'Copia'}
          </Button>
          <Button 
            variant="text" 
            size="small" 
            icon={<FiDownload />} 
            onClick={handleDownload}
            title="Scarica come file"
          >
            Scarica
          </Button>
          <Button 
            variant="text" 
            size="small" 
            icon={<FiSave />} 
            onClick={handleSaveTemplate}
            title="Salva come template riutilizzabile"
          >
            Salva Template
          </Button>
        </ActionsContainer>
      </PreviewHeader>
      
      <CodeContainer theme={settings.theme}>
        {settings.syntaxHighlighting ? (
          <SyntaxHighlighter
            language="abap"
            style={settings.theme === 'dark' ? tomorrow : prism}
            showLineNumbers={settings.showLineNumbers}
            wrapLines={true}
            customStyle={{
              margin: 0,
              padding: '20px',
              borderRadius: '4px',
              fontSize: '14px',
              height: '100%',
              background: settings.theme === 'dark' ? '#2d2d2d' : '#f8f8f8',
              overflow: 'auto'
            }}
          >
            {code}
          </SyntaxHighlighter>
        ) : (
          <pre>{settings.showLineNumbers ? addLineNumbers(code) : code}</pre>
        )}
      </CodeContainer>
      
      {code && code.length > 0 && (
        <InfoContainer>
          <CodeStats>
            <StatItem>
              <StatLabel>Righe:</StatLabel>
              <StatValue>{countLines(code)}</StatValue>
            </StatItem>
            <StatItem>
              <StatLabel>Caratteri:</StatLabel>
              <StatValue>{code.length}</StatValue>
            </StatItem>
          </CodeStats>
          <LastUpdated>
            Ultimo aggiornamento: {new Date().toLocaleTimeString()}
          </LastUpdated>
        </InfoContainer>
      )}
    </PreviewContainer>
  );
};

// Funzione per aggiungere numeri di riga
const addLineNumbers = (code) => {
  return code.split('\n').map((line, index) => {
    const lineNumber = (index + 1).toString().padStart(3, '0');
    return `${lineNumber}: ${line}`;
  }).join('\n');
};

// Funzione per contare le righe di codice
const countLines = (code) => {
  return code.split('\n').length;
};

// Stili del componente
const PreviewContainer = styled.div`
  display: flex;
  flex-direction: column;
  height: 100%;
  background: white;
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  overflow: hidden;
`;

const PreviewHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 15px;
  background: #f0f4f8;
  border-bottom: 1px solid #ddd;
  
  h3 {
    margin: 0;
    font-size: 16px;
    color: #333;
  }
`;

const ActionsContainer = styled.div`
  display: flex;
  gap: 8px;
`;

const CodeContainer = styled.div`
  flex-grow: 1;
  overflow: auto;
  position: relative;
  font-family: 'Fira Code', 'Courier New', monospace;
  background: ${props => props.theme === 'dark' ? '#2d2d2d' : '#f8f8f8'};
  color: ${props => props.theme === 'dark' ? '#f8f8f8' : '#2d2d2d'};
  
  pre {
    margin: 0;
    padding: 20px;
    white-space: pre-wrap;
    font-size: 14px;
    line-height: 1.5;
    height: 100%;
    overflow: auto;
  }
`;

const InfoContainer = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px 15px;
  background: #f5f5f5;
  border-top: 1px solid #eee;
  font-size: 12px;
  color: #666;
`;

const CodeStats = styled.div`
  display: flex;
  gap: 15px;
`;

const StatItem = styled.div`
  display: flex;
  align-items: center;
`;

const StatLabel = styled.span`
  margin-right: 5px;
  font-weight: bold;
`;

const StatValue = styled.span``;

const LastUpdated = styled.div`
  font-style: italic;
`;

export default CodePreview;