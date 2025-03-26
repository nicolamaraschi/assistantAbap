import React, { useState } from 'react';
import styled from 'styled-components';
import { FiTrash2, FiEdit, FiCopy, FiEye } from 'react-icons/fi';
import Button from '../common/Button';
import SearchBar from '../search/SearchBar';
import { useAbap } from '../../context/AbapContext';

// Componente per visualizzare e gestire i template salvati
const TemplatesPanel = ({ onSelectTemplate }) => {
  const { savedTemplates, setGeneratedCode, deleteTemplate } = useAbap();
  const [searchTerm, setSearchTerm] = useState('');
  
  // Filtra i template per la ricerca
  const filteredTemplates = savedTemplates.filter(template => 
    template.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
    template.constructType.toLowerCase().includes(searchTerm.toLowerCase())
  );
  
  // Gestisce la selezione di un template
  const handleSelectTemplate = (template) => {
    if (onSelectTemplate) {
      onSelectTemplate(template);
    }
  };
  
  // Gestisce l'anteprima di un template nel pannello di output
  const handlePreviewTemplate = (template) => {
    if (template.generatedCode) {
      setGeneratedCode(template.generatedCode);
    }
  };
  
  // Gestisce l'eliminazione di un template
  const handleDeleteTemplate = (id) => {
    if (window.confirm('Sei sicuro di voler eliminare questo template?')) {
      deleteTemplate(id);
    }
  };
  
  // Formatta la data di creazione del template
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
      <SearchBar
        onSearch={setSearchTerm}
        placeholder="Cerca template..."
      />
      
      {filteredTemplates.length === 0 ? (
        <EmptyState>
          <p>Nessun template salvato. Crea e salva i tuoi template per ritrovarli qui.</p>
        </EmptyState>
      ) : (
        <TemplatesList>
          {filteredTemplates.map(template => (
            <TemplateItem key={template.id}>
              <TemplateHeader>
                <TemplateName>{template.name}</TemplateName>
                <TemplateType>{template.constructType}</TemplateType>
              </TemplateHeader>
              
              <TemplateDate>
                Creato: {formatDate(template.timestamp)}
              </TemplateDate>
              
              <TemplateActions>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiCopy />}
                  onClick={() => handleSelectTemplate(template)}
                  title="Usa questo template"
                >
                  Usa
                </Button>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiEye />}
                  onClick={() => handlePreviewTemplate(template)}
                  title="Anteprima del codice"
                >
                  Anteprima
                </Button>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiTrash2 />}
                  onClick={() => handleDeleteTemplate(template.id)}
                  title="Elimina questo template"
                >
                  Rimuovi
                </Button>
              </TemplateActions>
            </TemplateItem>
          ))}
        </TemplatesList>
      )}
    </Container>
  );
};

// Stili del componente
const Container = styled.div`
  display: flex;
  flex-direction: column;
  height: 100%;
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

const TemplatesList = styled.div`
  display: flex;
  flex-direction: column;
  gap: 12px;
  overflow-y: auto;
  margin-top: 15px;
`;

const TemplateItem = styled.div`
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

const TemplateHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
`;

const TemplateName = styled.h4`
  margin: 0;
  font-size: 16px;
  color: #333;
`;

const TemplateType = styled.span`
  font-size: 12px;
  background: #e0e0e0;
  padding: 3px 8px;
  border-radius: 12px;
  color: #555;
`;

const TemplateDate = styled.div`
  font-size: 12px;
  color: #666;
  margin-bottom: 10px;
`;

const TemplateActions = styled.div`
  display: flex;
  gap: 8px;
  justify-content: flex-end;
  margin-top: 10px;
`;

export default TemplatesPane