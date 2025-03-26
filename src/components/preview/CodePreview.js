import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { tomorrow, prism } from 'react-syntax-highlighter/dist/esm/styles/prism';
import { FiCopy, FiDownload, FiSave, FiCheck } from 'react-icons/fi';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

// Componente per visualizzare l'anteprima del codice generato
const CodePreview = ({ code }) => {
  const { settings } = useAbap();
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
    // Questo sarà implementato altrove utilizzando il context
    const name = prompt('Inserisci un nome per questo modello:');
    if (name) {
      // logica di salvataggio implementata nel context
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
          >
            {copied ? 'Copiato!' : 'Copia'}
          </Button>
          <Button 
            variant="text" 
            size="small" 
            icon={<FiDownload />} 
            onClick={handleDownload}
          >
            Scarica
          </Button>
          <Button 
            variant="text" 
            size="small" 
            icon={<FiSave />} 
            onClick={handleSaveTemplate}
          >
            Salva Template
          </Button>
        </ActionsContainer>
      </PreviewHeader>
      
      <CodeContainer>
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
            }}
          >
            {code}
          </SyntaxHighlighter>
        ) : (
          <pre>{settings.showLineNumbers ? addLineNumbers(code) : code}</pre>
        )}
      </CodeContainer>
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

export default CodePreview;
