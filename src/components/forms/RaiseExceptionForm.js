import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const RaiseExceptionForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    exceptionClass: 'cx_sy_zerodivide',
    textId: 'division_by_zero',
    addExporting: false,
    exportingParams: [
      { id: 1, name: 'value', value: 'lv_value' }
    ]
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['raise']) {
      setFormData(formState['raise']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('raise', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce il cambiamento dei parametri exporting
  const handleParamChange = (id, field, value) => {
    setFormData({
      ...formData,
      exportingParams: formData.exportingParams.map(p => 
        p.id === id ? { ...p, [field]: value } : p
      )
    });
  };
  
  // Aggiunge un nuovo parametro exporting
  const handleAddParam = () => {
    const newId = Math.max(0, ...formData.exportingParams.map(p => p.id)) + 1;
    setFormData({
      ...formData,
      exportingParams: [
        ...formData.exportingParams,
        { id: newId, name: `param${newId}`, value: `lv_param${newId}` }
      ]
    });
  };
  
  // Rimuove un parametro exporting
  const handleRemoveParam = (id) => {
    setFormData({
      ...formData,
      exportingParams: formData.exportingParams.filter(p => p.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('raise', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Classe eccezione:">
        <ControlledInput type="text"
          name="exceptionClass"
          value={formData.exceptionClass}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="ID Testo (opzionale):">
        <ControlledInput type="text"
          name="textId"
          value={formData.textId}
          onChange={handleChange}
          placeholder="es. division_by_zero"
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addExporting"
          checked={formData.addExporting}
          onChange={handleChange}
          id="addExporting"
        />
        <label htmlFor="addExporting">Aggiungi parametri EXPORTING</label>
      </FormGroup>
      
      {formData.addExporting && (
        <FormGroup label="Parametri EXPORTING:">
          {formData.exportingParams.map(param => (
            <ParamItem key={param.id}>
              <ParamField>
                <label>Nome:</label>
                <ControlledInput type="text"
                  value={param.name}
                  onChange={(e) => handleParamChange(param.id, 'name', e.target.value)}
                />
              </ParamField>
              <ParamField>
                <label>Valore:</label>
                <ControlledInput type="text"
                  value={param.value}
                  onChange={(e) => handleParamChange(param.id, 'value', e.target.value)}
                />
              </ParamField>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveParam(param.id)}
                disabled={formData.exportingParams.length === 1}
              />
            </ParamItem>
          ))}
          
          <Button
            variant="outline"
            size="small"
            icon={<FiPlus />}
            onClick={handleAddParam}
          >
            Aggiungi parametro
          </Button>
        </FormGroup>
      )}
      
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
  
  input[type="text"] {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    transition: border-color 0.3s, box-shadow 0.3s;
    font-family: 'Courier New', monospace;
  }
  
  input[type="text"]:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const ParamItem = styled.div`
  display: flex;
  gap: 10px;
  align-items: flex-end;
  margin-bottom: 10px;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
  
  &:last-of-type {
    border-bottom: none;
  }
`;

const ParamField = styled.div`
  flex: 1;
  
  label {
    display: block;
    margin-bottom: 5px;
    font-size: 14px;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default RaiseExceptionForm;
