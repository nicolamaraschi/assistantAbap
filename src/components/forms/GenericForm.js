import React, { useState } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';

// Componente form generico per tutti i costrutti non ancora implementati
const GenericForm = ({ constructType, onGenerate }) => {
  const [formData, setFormData] = useState({
    content: `* Inserisci qui i parametri per il costrutto ${constructType}`
  });
  
  const handleChange = (e) => {
    setFormData({
      ...formData,
      content: e.target.value
    });
  };
  
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate(constructType, { content: formData.content });
    }
  };
  
  return (
    <FormContainer>
      <FormGroup>
        <h3>Costrutto {constructType}</h3>
        <p>L'implementazione completa di questo form è in fase di sviluppo.</p>
        <p>Inserisci i dettagli del costrutto e premi "Genera Codice":</p>
      </FormGroup>
      
      <FormGroup>
        <textarea
          name="content"
          value={formData.content}
          onChange={handleChange}
          rows={10}
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
  
  textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
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

export default GenericForm;