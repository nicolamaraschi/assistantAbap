import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';
import { useAbap } from '../../context/AbapContext';

// Componente per il form IF-ELSE
const IfElseFormExample = ({ onGenerate }) => {
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
        <ControlledInput
          type="text"
          name="condition"
          value={formData.condition}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Blocco IF:">
        <ControlledTextarea
          name="trueAction"
          value={formData.trueAction}
          onChange={handleChange}
          rows={4}
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
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
            <ControlledInput
              type="text"
              name="elseIfCondition"
              value={formData.elseIfCondition}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="Blocco ELSEIF:">
            <ControlledTextarea
              name="elseIfAction"
              value={formData.elseIfAction}
              onChange={handleChange}
              rows={4}
            />
          </FormGroup>
        </>
      )}
      
      <FormGroup label="Blocco ELSE:">
        <ControlledTextarea
          name="falseAction"
          value={formData.falseAction}
          onChange={handleChange}
          rows={4}
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
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default IfElseFormExample;
