import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Componente per il form SELECT
const SelectForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    fields: '*',
    table: 'ztable',
    into: 'TABLE lt_result',
    where: '',
    orderBy: '',
    groupBy: '',
    having: '',
    addJoin: false,
    joinType: 'INNER JOIN',
    joinTable: 'ztable2',
    joinCondition: 'ztable~id = ztable2~id'
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['select']) {
      setFormData(formState['select']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('select', formData);
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
      onGenerate('select', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Campi da selezionare:" tooltip="Specificare i campi separati da virgola, o * per tutti">
        <ControlledInput type="text"
          name="fields"
          value={formData.fields}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tabella:">
        <ControlledInput type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="INTO:">
        <ControlledInput type="text"
          name="into"
          value={formData.into}
          onChange={handleChange}
          placeholder="es. TABLE lt_result o CORRESPONDING FIELDS OF TABLE lt_result"
        />
      </FormGroup>
      
      <FormGroup label="WHERE (opzionale):">
        <ControlledInput type="text"
          name="where"
          value={formData.where}
          onChange={handleChange}
          placeholder="es. id = '123'"
        />
      </FormGroup>
      
      <AdvancedSection>
        <SectionTitle>Opzioni avanzate</SectionTitle>
        
        <FormGroup label="ORDER BY (opzionale):">
          <ControlledInput type="text"
            name="orderBy"
            value={formData.orderBy}
            onChange={handleChange}
            placeholder="es. created_at DESCENDING"
          />
        </FormGroup>
        
        <FormGroup label="GROUP BY (opzionale):">
          <ControlledInput type="text"
            name="groupBy"
            value={formData.groupBy}
            onChange={handleChange}
            placeholder="es. category"
          />
        </FormGroup>
        
        <FormGroup label="HAVING (opzionale):">
          <ControlledInput type="text"
            name="having"
            value={formData.having}
            onChange={handleChange}
            placeholder="es. COUNT(*) > 5"
          />
        </FormGroup>
        
        <FormGroup inline>
          <ControlledInput type="checkbox"
            name="addJoin"
            checked={formData.addJoin}
            onChange={handleChange}
            id="addJoin"
          />
          <label htmlFor="addJoin">Aggiungi JOIN</label>
        </FormGroup>
        
        {formData.addJoin && (
          <>
            <FormGroup label="Tipo di JOIN:">
              <select
                name="joinType"
                value={formData.joinType}
                onChange={handleChange}
              >
                <option value="INNER JOIN">INNER JOIN</option>
                <option value="LEFT OUTER JOIN">LEFT OUTER JOIN</option>
                <option value="RIGHT OUTER JOIN">RIGHT OUTER JOIN</option>
              </select>
            </FormGroup>
            
            <FormGroup label="Tabella JOIN:">
              <ControlledInput type="text"
                name="joinTable"
                value={formData.joinTable}
                onChange={handleChange}
              />
            </FormGroup>
            
            <FormGroup label="Condizione JOIN:">
              <ControlledInput type="text"
                name="joinCondition"
                value={formData.joinCondition}
                onChange={handleChange}
                placeholder="es. table1~id = table2~id"
              />
            </FormGroup>
          </>
        )}
      </AdvancedSection>
      
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
  textarea,
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
  textarea:focus,
  select:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const AdvancedSection = styled.div`
  margin-top: 20px;
  padding: 15px;
  background: #f9f9f9;
  border-radius: 8px;
  border: 1px solid #eee;
`;

const SectionTitle = styled.h4`
  margin-top: 0;
  margin-bottom: 15px;
  font-size: 16px;
  color: #333;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default SelectForm;