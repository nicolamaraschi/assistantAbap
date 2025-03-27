import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const InternalTableForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'lt_table',
    type: 'ty_structure',
    tableType: 'STANDARD',
    keyType: 'DEFAULT',
    initialSize: '10',
    withHeader: false
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['internal-table']) {
      setFormData(formState['internal-table']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('internal-table', formData);
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
      onGenerate('internal-table', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome della tabella interna:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo di riga:">
        <ControlledInput type="text"
          name="type"
          value={formData.type}
          onChange={handleChange}
          placeholder="es. ty_structure, string, ref to cl_class"
        />
      </FormGroup>
      
      <FormGroup label="Tipo di tabella:">
        <select
          name="tableType"
          value={formData.tableType}
          onChange={handleChange}
        >
          <option value="STANDARD">STANDARD TABLE</option>
          <option value="SORTED">SORTED TABLE</option>
          <option value="HASHED">HASHED TABLE</option>
          <option value="INDEX">INDEX TABLE</option>
        </select>
      </FormGroup>
      
      <FormGroup label="Tipo di chiave:">
        <select
          name="keyType"
          value={formData.keyType}
          onChange={handleChange}
        >
          <option value="DEFAULT">DEFAULT KEY</option>
          <option value="STANDARD">STANDARD KEY</option>
          <option value="EMPTY">EMPTY KEY</option>
          <option value="NON-UNIQUE">NON-UNIQUE KEY</option>
          <option value="UNIQUE">UNIQUE KEY</option>
        </select>
      </FormGroup>
      
      <FormGroup label="Dimensione iniziale (opzionale):">
        <ControlledInput type="text"
          name="initialSize"
          value={formData.initialSize}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="withHeader"
          checked={formData.withHeader}
          onChange={handleChange}
          id="withHeader"
        />
        <label htmlFor="withHeader">Aggiungi header line</label>
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
  select:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default InternalTableForm;
