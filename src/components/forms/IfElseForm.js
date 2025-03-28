import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

// Componente per il form IF-ELSE
const IfElseForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    condition: 'campo = valore',
    trueAction: 'WRITE: / \'Condizione verificata\'.',
    falseAction: 'WRITE: / \'Condizione non verificata\'.',
    addElseIf: false,
    elseIfCondition: 'campo = altro_valore',
    elseIfAction: 'WRITE: / \'Condizione ELSEIF verificata\'.'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['if-else']) {
      setFormData(formState['if-else']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('if-else', formData);
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
      onGenerate('if-else', formData);
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
          {!formData.condition && <PlaceholderText>Inserisci la condizione</PlaceholderText>}
        </StyledInputContainer>
      </FormGroup>
      
      <FormGroup label="Blocco IF:">
        <StyledTextareaContainer>
          <StyledTextarea
            name="trueAction"
            value={formData.trueAction}
            onChange={handleChange}
            rows={4}
          />
          {!formData.trueAction && <PlaceholderText>Inserisci il codice per il blocco IF</PlaceholderText>}
        </StyledTextareaContainer>
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="addElseIf"
          checked={formData.addElseIf}
          onChange={handleChange}
          id="addElseIf"
        />
        <label htmlFor="addElseIf">Aggiungi blocco ELSEIF</label>
      </FormGroup>
      
      {formData.addElseIf && (
        <>
          <FormGroup label="Condizione ELSEIF:">
            <StyledInputContainer>
              <StyledInput
                type="text"
                name="elseIfCondition"
                value={formData.elseIfCondition}
                onChange={handleChange}
              />
              {!formData.elseIfCondition && <PlaceholderText>Inserisci la condizione ELSEIF</PlaceholderText>}
            </StyledInputContainer>
          </FormGroup>
          
          <FormGroup label="Blocco ELSEIF:">
            <StyledTextareaContainer>
              <StyledTextarea
                name="elseIfAction"
                value={formData.elseIfAction}
                onChange={handleChange}
                rows={4}
              />
              {!formData.elseIfAction && <PlaceholderText>Inserisci il codice per il blocco ELSEIF</PlaceholderText>}
            </StyledTextareaContainer>
          </FormGroup>
        </>
      )}
      
      <FormGroup label="Blocco ELSE:">
        <StyledTextareaContainer>
          <StyledTextarea
            name="falseAction"
            value={formData.falseAction}
            onChange={handleChange}
            rows={4}
          />
          {!formData.falseAction && <PlaceholderText>Inserisci il codice per il blocco ELSE</PlaceholderText>}
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

export default IfElseForm;