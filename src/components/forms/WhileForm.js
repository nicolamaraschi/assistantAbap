import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const WhileForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    condition: 'lv_counter < 10',
    content: 'WRITE: / lv_counter.\nADD 1 TO lv_counter.'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['while']) {
      setFormData(formState['while']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('while', formData);
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
      onGenerate('while', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Condizione:">
        <ControlledInput type="text"
          name="condition"
          value={formData.condition}
          onChange={handleChange}
          placeholder="es. lv_counter < 10"
        />
      </FormGroup>
      
      <FormGroup label="Contenuto del loop:">
        <ControlledTextarea
          name="content"
          value={formData.content}
          onChange={handleChange}
          rows={5}
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
    min-height: 80px;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default WhileForm;
