import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const FormForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'process_data',
    params: 'iv_input TYPE string\n      iv_option TYPE char1',
    content: '* Inserisci qui il contenuto del form\nWRITE: / iv_input.\nIF iv_option = \'X\'.\n  WRITE: / \'Opzione attiva\'.\nENDIF.'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['form']) {
      setFormData(formState['form']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('form', formData);
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
      onGenerate('form', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome del Form:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Parametri (USING):">
        <ControlledTextarea
          name="params"
          value={formData.params}
          onChange={handleChange}
          rows={3}
          placeholder="es. iv_input TYPE string"
        />
      </FormGroup>
      
      <FormGroup label="Contenuto del Form:">
        <ControlledTextarea
          name="content"
          value={formData.content}
          onChange={handleChange}
          rows={7}
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

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default FormForm;
