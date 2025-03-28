import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

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
        <StyledInputContainer>
          <StyledInput 
            type="text"
            name="condition"
            value={formData.condition}
            onChange={handleChange}
          />
          {!formData.condition && <PlaceholderText>es. lv_counter &lt; 10</PlaceholderText>}
        </StyledInputContainer>
      </FormGroup>
      
      <FormGroup label="Contenuto del loop:">
        <StyledTextareaContainer>
          <StyledTextarea
            name="content"
            value={formData.content}
            onChange={handleChange}
            rows={5}
          />
          {!formData.content && <PlaceholderText>Inserisci il contenuto del loop</PlaceholderText>}
        </StyledTextareaContainer>
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
`;

const StyledInputContainer = styled.div`
  position: relative;
  width: 100%;
`;

const StyledInput = styled.input`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  background-color: transparent;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const StyledTextareaContainer = styled.div`
  position: relative;
  width: 100%;
`;

const StyledTextarea = styled.textarea`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  resize: vertical;
  min-height: 80px;
  background-color: transparent;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const PlaceholderText = styled.div`
  position: absolute;
  top: 10px;
  left: 10px;
  color: #aaa;
  pointer-events: none;
  z-index: 1;
  font-family: 'Courier New', monospace;
  font-size: 15px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default WhileForm;