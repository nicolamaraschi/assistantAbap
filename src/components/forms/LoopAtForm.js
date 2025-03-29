// LoopAtForm.js - Migliorato con GROUP BY e REFERENCE INTO
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const LoopAtForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    table: 'lt_table',
    variable: 'ls_line',
    whereCondition: '',
    content: 'WRITE: / ls_line-field.',
    useAssigning: false,
    addIndex: false,
    indexFrom: '1',
    indexTo: '10',
    useGroupBy: false,  // Nuova opzione
    groupByField: 'field', // Nuovo campo
    groupByOrder: 'ASCENDING', // Nuovo campo
    useReferenceInto: false // Nuova opzione
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
        <ControlledInput
          type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Condizione WHERE (opzionale):">
        <ControlledInput
          type="text"
          name="whereCondition"
          value={formData.whereCondition}
          onChange={handleChange}
          placeholder="es. field = 'value'"
        />
      </FormGroup>
      
      <AdvancedOptions>
        <h4>Opzioni Avanzate</h4>
        
        <AdvancedSection>
          <h5>Tipo di Loop</h5>
          
          <FormGroup inline>
            <input
              type="radio"
              name="loopType"
              id="loopInto"
              checked={!formData.useAssigning && !formData.useReferenceInto}
              onChange={() => setFormData({
                ...formData,
                useAssigning: false,
                useReferenceInto: false
              })}
            />
            <label htmlFor="loopInto">INTO</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="radio"
              name="loopType"
              id="loopAssigning"
              checked={formData.useAssigning}
              onChange={() => setFormData({
                ...formData,
                useAssigning: true,
                useReferenceInto: false
              })}
            />
            <label htmlFor="loopAssigning">ASSIGNING Field-Symbol</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="radio"
              name="loopType"
              id="loopReference"
              checked={formData.useReferenceInto}
              onChange={() => setFormData({
                ...formData,
                useAssigning: false,
                useReferenceInto: true
              })}
            />
            <label htmlFor="loopReference">REFERENCE INTO</label>
          </FormGroup>
        </AdvancedSection>
        
        <FormGroup label="Variabile di destinazione:">
          <ControlledInput
            type="text"
            name="variable"
            value={formData.variable}
            onChange={handleChange}
            placeholder={formData.useAssigning ? "Nome field-symbol" : (formData.useReferenceInto ? "Nome reference" : "Nome variabile")}
          />
        </FormGroup>
        
        <AdvancedSection>
          <FormGroup inline>
            <input
              type="checkbox"
              name="addIndex"
              checked={formData.addIndex}
              onChange={handleChange}
              id="addIndex"
            />
            <label htmlFor="addIndex">Limita intervallo di loop (FROM/TO)</label>
          </FormGroup>
          
          {formData.addIndex && (
            <IndexRange>
              <FormGroup label="Da:">
                <ControlledInput
                  type="text"
                  name="indexFrom"
                  value={formData.indexFrom}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="A:">
                <ControlledInput
                  type="text"
                  name="indexTo"
                  value={formData.indexTo}
                  onChange={handleChange}
                />
              </FormGroup>
            </IndexRange>
          )}
        </AdvancedSection>
        
        <AdvancedSection>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useGroupBy"
              checked={formData.useGroupBy}
              onChange={handleChange}
              id="useGroupBy"
            />
            <label htmlFor="useGroupBy">Usa GROUP BY</label>
          </FormGroup>
          
          {formData.useGroupBy && (
            <>
              <FormGroup label="Campo di raggruppamento:">
                <ControlledInput
                  type="text"
                  name="groupByField"
                  value={formData.groupByField}
                  onChange={handleChange}
                  placeholder="es. field"
                />
              </FormGroup>
              
              <FormGroup label="Ordinamento:">
                <select
                  name="groupByOrder"
                  value={formData.groupByOrder}
                  onChange={handleChange}
                >
                  <option value="ASCENDING">ASCENDING</option>
                  <option value="DESCENDING">DESCENDING</option>
                </select>
              </FormGroup>
            </>
          )}
        </AdvancedSection>
      </AdvancedOptions>
      
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
`;

const AdvancedOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 20px;
  
  h4 {
    margin-top: 0;
    margin-bottom: 15px;
    font-size: 16px;
    color: #333;
  }
`;

const AdvancedSection = styled.div`
  background: #fff;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 12px;
  margin-bottom: 12px;
  
  h5 {
    margin-top: 0;
    margin-bottom: 10px;
    font-size: 14px;
    color: #444;
  }
`;

const IndexRange = styled.div`
  display: flex;
  gap: 15px;
  
  > div {
    flex: 1;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default LoopAtForm;