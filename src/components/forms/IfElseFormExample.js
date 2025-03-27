import React, { useState, useRef, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import * as codeGenerators from '../../utils/codeGenerators'; // Importa direttamente i generatori

// Componente per il form IF-ELSE
const IfElseForm = ({ onGenerate }) => {
  // Accesso al context
  const { getFormState, updateFormState, setGeneratedCode } = useAbap();
  const formType = 'if-else'; // Nome del form
  
  // Inizializza lo stato
  const [formData, setFormData] = useState(() => {
    // Prova a caricare i dati dal context
    const savedData = getFormState(formType);
    if (savedData) {
      return savedData;
    }
    
    // Valori predefiniti
    return {
      condition: 'campo = valore',
      trueAction: 'WRITE: / \'Condizione verificata\'.',
      falseAction: 'WRITE: / \'Condizione non verificata\'.',
      addElseIf: false,
      elseIfCondition: 'campo = altro_valore',
      elseIfAction: 'WRITE: / \'Condizione ELSEIF verificata\'.'
    };
  });
  
  // Flag per evitare cicli
  const isInitialized = useRef(false);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    
    // Mantiene la posizione del cursore
    const cursorPosition = e.target.selectionStart;
    
    setFormData(prevData => {
      const newData = {
        ...prevData,
        [name]: type === 'checkbox' ? checked : value
      };
      
      // Ripristina la posizione del cursore
      setTimeout(() => {
        if (e.target && document.activeElement === e.target) {
          e.target.selectionStart = cursorPosition;
          e.target.selectionEnd = cursorPosition;
        }
      }, 0);
      
      return newData;
    });
  };
  
  // Gestisce la generazione del codice - SOLUZIONE COMPLETA
  const handleGenerate = () => {
    // Salva lo stato nel context
    updateFormState(formType, formData);
    
    // IMPORTANTE: Genera il codice direttamente qui 
    if (codeGenerators[formType]) {
      const generatedCode = codeGenerators[formType](formData);
      
      // Aggiorna direttamente lo stato del codice generato nel context
      setGeneratedCode(generatedCode);
      
      // Chiama anche il callback se esiste
      if (onGenerate) {
        onGenerate(formType, formData);
      }
    } else {
      console.error(`Nessun generatore trovato per il tipo: ${formType}`);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Condizione:">
        <Input
          type="text"
          name="condition"
          value={formData.condition}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Blocco IF:">
        <Textarea
          name="trueAction"
          value={formData.trueAction}
          onChange={handleChange}
          rows={4}
        />
      </FormGroup>
      
      <FormGroup inline>
        <CheckboxInput
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
            <Input
              type="text"
              name="elseIfCondition"
              value={formData.elseIfCondition}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="Blocco ELSEIF:">
            <Textarea
              name="elseIfAction"
              value={formData.elseIfAction}
              onChange={handleChange}
              rows={4}
            />
          </FormGroup>
        </>
      )}
      
      <FormGroup label="Blocco ELSE:">
        <Textarea
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

const Input = styled.input`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const Textarea = styled.textarea`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  resize: vertical;
  min-height: 80px;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const CheckboxInput = styled.input`
  margin-right: 8px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default IfElseForm;