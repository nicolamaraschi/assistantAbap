import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const DataDeclarationForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    variables: [
      { id: 1, name: 'lv_text', type: 'STRING', initialValue: '', isReference: false },
      { id: 2, name: 'lv_number', type: 'INT4', initialValue: '0', isReference: false }
    ]
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['data-declaration']) {
      setFormData(formState['data-declaration']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('data-declaration', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento delle variabili
  const handleVariableChange = (id, field, value) => {
    setFormData({
      ...formData,
      variables: formData.variables.map(v => 
        v.id === id ? { ...v, [field]: field === 'isReference' ? !v.isReference : value } : v
      )
    });
  };
  
  // Aggiunge una nuova variabile
  const handleAddVariable = () => {
    const newId = Math.max(0, ...formData.variables.map(v => v.id)) + 1;
    setFormData({
      ...formData,
      variables: [
        ...formData.variables,
        { id: newId, name: `lv_var${newId}`, type: 'STRING', initialValue: '', isReference: false }
      ]
    });
  };
  
  // Rimuove una variabile
  const handleRemoveVariable = (id) => {
    setFormData({
      ...formData,
      variables: formData.variables.filter(v => v.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('data-declaration', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Dichiarazioni DATA:">
        {formData.variables.map(variable => (
          <VariableItem key={variable.id}>
            <VariableHeader>
              <VariableTitle>Variabile {variable.id}</VariableTitle>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveVariable(variable.id)}
              />
            </VariableHeader>
            <FormGroup label="Nome:" inline>
              <ControlledInput type="text"
                value={variable.name}
                onChange={(e) => handleVariableChange(variable.id, 'name', e.target.value)}
              />
            </FormGroup>
            <FormGroup label="Tipo:" inline>
              <ControlledInput type="text"
                value={variable.type}
                onChange={(e) => handleVariableChange(variable.id, 'type', e.target.value)}
              />
            </FormGroup>
            <FormGroup label="Valore iniziale (opzionale):" inline>
              <ControlledInput type="text"
                value={variable.initialValue}
                onChange={(e) => handleVariableChange(variable.id, 'initialValue', e.target.value)}
                placeholder="es. 'testo' o IS INITIAL"
              />
            </FormGroup>
            <FormGroup inline>
              <ControlledInput type="checkbox"
                id={`isReference-${variable.id}`}
                checked={variable.isReference}
                onChange={() => handleVariableChange(variable.id, 'isReference')}
              />
              <label htmlFor={`isReference-${variable.id}`}>Riferimento (REF TO)</label>
            </FormGroup>
          </VariableItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddVariable}
        >
          Aggiungi variabile
        </Button>
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
  
  input[type="text"] {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    transition: border-color 0.3s, box-shadow 0.3s;
    font-family: 'Courier New', monospace;
  }
  
  input[type="text"]:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  input[type="checkbox"] {
    margin-right: 8px;
  }
`;

const VariableItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const VariableHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const VariableTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default DataDeclarationForm;
