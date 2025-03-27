import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const BapiCallForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    bapiName: 'BAPI_USER_GET_DETAIL',
    importsSection: 'USERNAME = lv_username',
    exportsSection: 'ADDRESS = ls_address\nLOGONDATA = ls_logondata',
    tablesSection: 'PROFILES = lt_profiles\nACTGROUPS = lt_actgroups',
    checkReturn: true,
    addComments: true
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['bapi-call']) {
      setFormData(formState['bapi-call']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('bapi-call', formData);
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
      onGenerate('bapi-call', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome BAPI:">
        <ControlledInput type="text"
          name="bapiName"
          value={formData.bapiName}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Parametri IMPORTING:">
        <ControlledTextarea
          name="importsSection"
          value={formData.importsSection}
          onChange={handleChange}
          rows={4}
          placeholder="es. USERNAME = lv_username"
        />
      </FormGroup>
      
      <FormGroup label="Parametri EXPORTING:">
        <ControlledTextarea
          name="exportsSection"
          value={formData.exportsSection}
          onChange={handleChange}
          rows={4}
          placeholder="es. ADDRESS = ls_address"
        />
      </FormGroup>
      
      <FormGroup label="Parametri TABLES:">
        <ControlledTextarea
          name="tablesSection"
          value={formData.tablesSection}
          onChange={handleChange}
          rows={4}
          placeholder="es. PROFILES = lt_profiles"
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="checkReturn"
          checked={formData.checkReturn}
          onChange={handleChange}
          id="checkReturn"
        />
        <label htmlFor="checkReturn">Verifica parametro RETURN</label>
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addComments"
          checked={formData.addComments}
          onChange={handleChange}
          id="addComments"
        />
        <label htmlFor="addComments">Aggiungi commenti esplicativi</label>
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

export default BapiCallForm;
