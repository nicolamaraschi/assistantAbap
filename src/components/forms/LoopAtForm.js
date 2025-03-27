import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Componente per il form LOOP AT
const LoopAtForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    table: 'lt_table',
    variable: 'ls_line',
    whereCondition: '',
    content: 'WRITE: / ls_line-field.',
    useAssigning: false,
    addIndex: false
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['loop-at']) {
      setFormData(formState['loop-at']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('loop-at', formData);
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
      onGenerate('loop-at', formData);
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
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="useAssigning"
          checked={formData.useAssigning}
          onChange={handleChange}
          id="useAssigning"
        />
        <label htmlFor="useAssigning">Usa ASSIGNING invece di INTO</label>
      </FormGroup>
      
      <FormGroup label={formData.useAssigning ? "Nome Field-Symbol:" : "Variabile di destinazione:"}>
        <ControlledInput type="text"
          name="variable"
          value={formData.variable}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Condizione WHERE (opzionale):">
        <ControlledInput type="text"
          name="whereCondition"
          value={formData.whereCondition}
          onChange={handleChange}
          placeholder="es. field = 'value'"
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addIndex"
          checked={formData.addIndex}
          onChange={handleChange}
          id="addIndex"
        />
        <label htmlFor="addIndex">Aggiungi FROM/TO (limita il loop)</label>
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

export default LoopAtForm;