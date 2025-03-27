import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const InsertForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    table: 'ztable',
    insertType: 'values',
    fields: 'id = lv_id,\n    name = lv_name,\n    date = sy-datum',
    source: 'lt_data'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['insert']) {
      setFormData(formState['insert']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('insert', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('insert', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Tabella:">
        <ControlledInput type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo di inserimento:">
        <select
          name="insertType"
          value={formData.insertType}
          onChange={handleChange}
        >
          <option value="values">Valori specifici (VALUES)</option>
          <option value="table">Da tabella interna (FROM TABLE)</option>
        </select>
      </FormGroup>
      
      {formData.insertType === 'values' ? (
        <FormGroup label="Campi da inserire:">
          <ControlledTextarea
            name="fields"
            value={formData.fields}
            onChange={handleChange}
            rows={4}
            placeholder="es. id = lv_id, name = lv_name"
          />
        </FormGroup>
      ) : (
        <FormGroup label="Tabella interna di origine:">
          <ControlledInput type="text"
            name="source"
            value={formData.source}
            onChange={handleChange}
            placeholder="es. lt_data"
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
  textarea,
  select {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    transition: border-color 0.3s, box-shadow 0.3s;
    font-family: 'Courier New', monospace;
  }
  
  input[type="text"]:focus,
  textarea:focus,
  select:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  textarea {
    resize: vertical;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default InsertForm;
