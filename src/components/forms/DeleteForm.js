import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';

const DeleteForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    table: 'ztable',
    deleteType: 'where',
    where: 'id = lv_id',
    source: 'lt_del_entries',
    index: ''
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['delete']) {
      setFormData(formState['delete']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('delete', formData);
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
      onGenerate('delete', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Tabella:">
        <input
          type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo di eliminazione:">
        <select
          name="deleteType"
          value={formData.deleteType}
          onChange={handleChange}
        >
          <option value="where">Con condizione WHERE</option>
          <option value="table">Da tabella interna (FROM TABLE)</option>
          <option value="index">Per indice</option>
        </select>
      </FormGroup>
      
      {formData.deleteType === 'where' && (
        <FormGroup label="Condizione WHERE:">
          <input
            type="text"
            name="where"
            value={formData.where}
            onChange={handleChange}
            placeholder="es. id = lv_id"
          />
        </FormGroup>
      )}
      
      {formData.deleteType === 'table' && (
        <FormGroup label="Tabella interna di origine:">
          <input
            type="text"
            name="source"
            value={formData.source}
            onChange={handleChange}
            placeholder="es. lt_del_entries"
          />
        </FormGroup>
      )}
      
      {formData.deleteType === 'index' && (
        <FormGroup label="Indice:">
          <input
            type="text"
            name="index"
            value={formData.index}
            onChange={handleChange}
            placeholder="es. lv_index o un numero"
          />
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

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default DeleteForm;
