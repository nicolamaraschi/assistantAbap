// src/components/history/GenerationHistory.js
import React, { useState } from 'react';
import styled from 'styled-components';
import { FiCopy, FiTrash2, FiClock, FiCode } from 'react-icons/fi';
import Button from '../common/Button';
import SearchBar from '../search/SearchBar';

const GenerationHistory = ({ history = [], onSelect, onClear, onCopy }) => {
  const [searchTerm, setSearchTerm] = useState('');
  
  // Filtra la cronologia in base al termine di ricerca
  const filteredHistory = history.filter(item => 
    item.description.toLowerCase().includes(searchTerm.toLowerCase()) ||
    item.constructType.toLowerCase().includes(searchTerm.toLowerCase())
  );
  
  // Formatta la data
  const formatDate = (dateString) => {
    const date = new Date(dateString);
    return date.toLocaleDateString('it-IT', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  };
  
  return (
    <Container>
      <Header>
        <SearchBar
          onSearch={setSearchTerm}
          placeholder="Cerca nella cronologia..."
        />
        <Button
          variant="text"
          size="small"
          icon={<FiTrash2 />}
          onClick={onClear}
          title="Cancella tutta la cronologia"
          disabled={history.length === 0}
        >
          Cancella Cronologia
        </Button>
      </Header>
      
      {filteredHistory.length === 0 ? (
        <EmptyState>
          <p>Nessuna generazione nella cronologia.</p>
        </EmptyState>
      ) : (
        <HistoryList>
          {filteredHistory.map(item => (
            <HistoryItem key={item.id}>
              <ItemHeader>
                <ItemTitle>{item.description}</ItemTitle>
                <ItemType>{item.constructType}</ItemType>
              </ItemHeader>
              
              <ItemDate>
                <FiClock size={14} />
                <span>{formatDate(item.timestamp)}</span>
              </ItemDate>
              
              <ItemPreview>
                <FiCode size={14} />
                <code>{item.generatedCode.substring(0, 100)}{item.generatedCode.length > 100 ? '...' : ''}</code>
              </ItemPreview>
              
              <ItemActions>
                <Button
                  variant="text"
                  size="small"
                  onClick={() => onSelect(item)}
                  title="Riutilizza questo codice"
                >
                  Riutilizza
                </Button>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiCopy />}
                  onClick={() => onCopy(item)}
                  title="Copia negli appunti"
                >
                  Copia
                </Button>
              </ItemActions>
            </HistoryItem>
          ))}
        </HistoryList>
      )}
    </Container>
  );
};

const Container = styled.div``;

const Header = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
`;

const EmptyState = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  height: 200px;
  text-align: center;
  background: #f9f9f9;
  border-radius: 8px;
  
  p {
    max-width: 80%;
    color: #666;
  }
`;

const HistoryList = styled.div`
  display: flex;
  flex-direction: column;
  gap: 12px;
  max-height: 500px;
  overflow-y: auto;
`;

const HistoryItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 8px;
  padding: 15px;
  transition: all 0.2s ease;
  
  &:hover {
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
    border-color: #ddd;
  }
`;

const ItemHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
`;

const ItemTitle = styled.h4`
  margin: 0;
  font-size: 16px;
  color: #333;
`;

const ItemType = styled.span`
  font-size: 12px;
  background: #e0e0e0;
  padding: 3px 8px;
  border-radius: 12px;
  color: #555;
`;

const ItemDate = styled.div`
  display: flex;
  align-items: center;
  gap: 5px;
  font-size: 12px;
  color: #666;
  margin-bottom: 10px;
`;

const ItemPreview = styled.div`
  display: flex;
  align-items: flex-start;
  gap: 5px;
  background: #f0f0f0;
  padding: 8px;
  border-radius: 4px;
  font-size: 12px;
  color: #555;
  margin-bottom: 10px;
  
  code {
    font-family: 'Fira Code', 'Courier New', monospace;
    white-space: pre-wrap;
    overflow: hidden;
  }
`;

const ItemActions = styled.div`
  display: flex;
  gap: 8px;
  justify-content: flex-end;
`;

export default GenerationHistory;