import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

const DoEndDoForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    times: '5',
    content: 'WRITE: / sy-index.',
    condition: '',
    addExitAt: false
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['do-enddo']) {
      setFormData(formState['do-enddo']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('do-enddo', formData);
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
      onGenerate('do-enddo', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Numero di iterazioni (opzionale):">
        <input
          type="text"
          name="times"
          value={formData.times}
          onChange={handleChange}
          placeholder="Lascia vuoto per loop infinito"
        />
      </FormGroup>
      
      <FormGroup label="Contenuto del loop:">
        <textarea
          name="content"
          value={formData.content}
          onChange={handleChange}
          rows={5}
        />
      </FormGroup>
      
      <FormGroup label="Condizione di controllo (opzionale):">
        <input
          type="text"
          name="condition"
          value={formData.condition}
          onChange={handleChange}
          placeholder="es. sy-index > 10"
        />
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="addExitAt"
          checked={formData.addExitAt}
          onChange={handleChange}
          id="addExitAt"
        />
        <label htmlFor="addExitAt">Aggiungi EXIT condizionale</label>
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

export default DoEndDoForm;
