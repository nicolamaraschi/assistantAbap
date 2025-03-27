import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const MessageForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    messageType: 'E',
    messageId: 'ZS',
    messageNumber: '001',
    with1: '',
    with2: '',
    with3: '',
    with4: '',
    useVariables: false,
    displayAsDialog: true
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['message']) {
      setFormData(formState['message']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('message', formData);
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
      onGenerate('message', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Tipo di messaggio:">
        <select
          name="messageType"
          value={formData.messageType}
          onChange={handleChange}
        >
          <option value="I">I - Informativo</option>
          <option value="S">S - Successo</option>
          <option value="W">W - Avviso</option>
          <option value="E">E - Errore</option>
          <option value="A">A - Errore anomalo</option>
          <option value="X">X - Errore di sistema</option>
        </select>
      </FormGroup>
      
      <FormGroup label="ID Messaggio:">
        <ControlledInput type="text"
          name="messageId"
          value={formData.messageId}
          onChange={handleChange}
          placeholder="es. ZS"
        />
      </FormGroup>
      
      <FormGroup label="Numero messaggio:">
        <ControlledInput type="text"
          name="messageNumber"
          value={formData.messageNumber}
          onChange={handleChange}
          placeholder="es. 001"
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="useVariables"
          checked={formData.useVariables}
          onChange={handleChange}
          id="useVariables"
        />
        <label htmlFor="useVariables">Aggiungi parametri WITH</label>
      </FormGroup>
      
      {formData.useVariables && (
        <VariablesContainer>
          <FormGroup label="WITH 1:">
            <ControlledInput type="text"
              name="with1"
              value={formData.with1}
              onChange={handleChange}
              placeholder="es. lv_var1"
            />
          </FormGroup>
          
          <FormGroup label="WITH 2:">
            <ControlledInput type="text"
              name="with2"
              value={formData.with2}
              onChange={handleChange}
              placeholder="es. lv_var2"
            />
          </FormGroup>
          
          <FormGroup label="WITH 3:">
            <ControlledInput type="text"
              name="with3"
              value={formData.with3}
              onChange={handleChange}
              placeholder="es. lv_var3"
            />
          </FormGroup>
          
          <FormGroup label="WITH 4:">
            <ControlledInput type="text"
              name="with4"
              value={formData.with4}
              onChange={handleChange}
              placeholder="es. lv_var4"
            />
          </FormGroup>
        </VariablesContainer>
      )}
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="displayAsDialog"
          checked={formData.displayAsDialog}
          onChange={handleChange}
          id="displayAsDialog"
        />
        <label htmlFor="displayAsDialog">Visualizza come finestra di dialogo</label>
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

const VariablesContainer = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-top: 10px;
  margin-bottom: 15px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default MessageForm;
