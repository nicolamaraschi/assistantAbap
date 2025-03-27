import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const MethodDefinitionForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    className: 'lcl_my_class',
    methodName: 'method1',
    importing: 'iv_input TYPE string',
    exporting: 'ev_result TYPE string',
    changing: '',
    returning: '',
    raising: 'cx_sy_conversion_error',
    content: '* Inserisci qui il corpo del metodo\nev_result = iv_input.'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['method-definition']) {
      setFormData(formState['method-definition']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('method-definition', formData);
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
      onGenerate('method-definition', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Classe:">
        <ControlledInput type="text"
          name="className"
          value={formData.className}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Nome Metodo:">
        <ControlledInput type="text"
          name="methodName"
          value={formData.methodName}
          onChange={handleChange}
        />
      </FormGroup>
      
      <ParametersContainer>
        <FormGroup label="IMPORTING:">
          <ControlledTextarea
            name="importing"
            value={formData.importing}
            onChange={handleChange}
            rows={2}
            placeholder="es. iv_param1 TYPE string"
          />
        </FormGroup>
        
        <FormGroup label="EXPORTING:">
          <ControlledTextarea
            name="exporting"
            value={formData.exporting}
            onChange={handleChange}
            rows={2}
            placeholder="es. ev_result TYPE string"
          />
        </FormGroup>
        
        <FormGroup label="CHANGING:">
          <ControlledTextarea
            name="changing"
            value={formData.changing}
            onChange={handleChange}
            rows={2}
            placeholder="es. cv_value TYPE string"
          />
        </FormGroup>
        
        <FormGroup label="RETURNING:">
          <ControlledInput type="text"
            name="returning"
            value={formData.returning}
            onChange={handleChange}
            placeholder="es. VALUE(rv_result) TYPE string"
          />
        </FormGroup>
        
        <FormGroup label="RAISING:">
          <ControlledInput type="text"
            name="raising"
            value={formData.raising}
            onChange={handleChange}
            placeholder="es. cx_sy_zerodivide cx_sy_conversion_error"
          />
        </FormGroup>
      </ParametersContainer>
      
      <FormGroup label="Corpo del metodo:">
        <ControlledTextarea
          name="content"
          value={formData.content}
          onChange={handleChange}
          rows={8}
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
    min-height: 50px;
  }
`;

const ParametersContainer = styled.div`
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

export default MethodDefinitionForm;
