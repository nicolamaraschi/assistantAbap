import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';

// Componente per il form CASE
const CaseForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    variable: 'sy-index',
    cases: [
      { id: 1, value: '1', action: 'WRITE: / \'Primo caso\'.' },
      { id: 2, value: '2', action: 'WRITE: / \'Secondo caso\'.' }
    ],
    defaultAction: 'WRITE: / \'Caso di default\'.'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['case']) {
      setFormData(formState['case']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('case', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento del campo variabile
  const handleVariableChange = (e) => {
    setFormData({
      ...formData,
      variable: e.target.value
    });
  };
  
  // Gestisce il cambiamento dei casi
  const handleCaseChange = (id, field, value) => {
    setFormData({
      ...formData,
      cases: formData.cases.map(c => 
        c.id === id ? { ...c, [field]: value } : c
      )
    });
  };
  
  // Aggiunge un nuovo caso
  const handleAddCase = () => {
    const newId = Math.max(0, ...formData.cases.map(c => c.id)) + 1;
    setFormData({
      ...formData,
      cases: [
        ...formData.cases,
        { id: newId, value: `${newId}`, action: `WRITE: / 'Caso ${newId}'.` }
      ]
    });
  };
  
  // Rimuove un caso
  const handleRemoveCase = (id) => {
    setFormData({
      ...formData,
      cases: formData.cases.filter(c => c.id !== id)
    });
  };
  
  // Gestisce il cambiamento dell'azione predefinita
  const handleDefaultActionChange = (e) => {
    setFormData({
      ...formData,
      defaultAction: e.target.value
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('case', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Variabile di controllo:">
        <input
          type="text"
          value={formData.variable}
          onChange={handleVariableChange}
        />
      </FormGroup>
      
      <FormGroup label="Casi:">
        {formData.cases.map(caseItem => (
          <CaseItem key={caseItem.id}>
            <CaseHeader>
              <FormGroup label="Valore:" inline>
                <input
                  type="text"
                  value={caseItem.value}
                  onChange={(e) => handleCaseChange(caseItem.id, 'value', e.target.value)}
                />
              </FormGroup>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveCase(caseItem.id)}
              />
            </CaseHeader>
            <FormGroup label="Azione:">
              <textarea
                value={caseItem.action}
                onChange={(e) => handleCaseChange(caseItem.id, 'action', e.target.value)}
                rows={3}
              />
            </FormGroup>
          </CaseItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddCase}
        >
          Aggiungi caso
        </Button>
      </FormGroup>
      
      <FormGroup label="Azione predefinita (WHEN OTHERS):">
        <textarea
          value={formData.defaultAction}
          onChange={handleDefaultActionChange}
          rows={3}
        />
      </FormGroup>
      
      <ButtonContainer>
        <Button 
          variant="primary" 
          onClick={handleGenerate}
          fullWidth
        >
          Genera Codice
        </Button>
      </ButtonContainer>
    </FormContainer>
  );
};

// Stili del componente
const FormContainer = styled.div`
  padding: 15px;
  
  input[type="text"],
  textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    transition: border-color 0.3s, box-shadow 0.3s;
    font-family: 'Courier New', monospace;
  }
  
  input[type="text"]:focus,
  textarea:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  textarea {
    resize: vertical;
  }
`;

const CaseItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const CaseHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default CaseForm;