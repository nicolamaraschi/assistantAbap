import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

const ModifyForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    table: 'lt_table',
    source: 'ls_line',
    index: '',
    addTransporting: false,
    fields: 'field1 field2 field3'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['modify']) {
      setFormData(formState['modify']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('modify', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('modify', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Tabella da modificare:">
        <input
          type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Struttura di origine:">
        <input
          type="text"
          name="source"
          value={formData.source}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Indice (opzionale):">
        <input
          type="text"
          name="index"
          value={formData.index}
          onChange={handleChange}
          placeholder="es. lv_index o un numero"
        />
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="addTransporting"
          checked={formData.addTransporting}
          onChange={handleChange}
          id="addTransporting"
        />
        <label htmlFor="addTransporting">Specifica campi da trasportare (TRANSPORTING)</label>
      </FormGroup>
      
      {formData.addTransporting && (
        <FormGroup label="Campi da trasportare:">
          <input
            type="text"
            name="fields"
            value={formData.fields}
            onChange={handleChange}
            placeholder="es. field1 field2 field3"
          />
        </FormGroup>
      )}
      
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
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default ModifyForm;
