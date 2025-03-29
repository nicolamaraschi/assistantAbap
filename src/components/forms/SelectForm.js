// SelectForm.js - Migliorato con UNION e FOR ALL ENTRIES
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

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
    joinCondition: 'ztable~id = ztable2~id',
    useUnion: false,         // Nuovo campo
    unionType: 'UNION ALL',  // Nuovo campo
    unionSelect: 'SELECT field1, field2\n  FROM ztable2\n  WHERE field2 = \'value\'', // Nuovo campo
    useForAllEntries: false, // Nuovo campo
    forAllEntriesTable: 'lt_keys', // Nuovo campo
    forAllEntriesWhere: 'ztable~id = lt_keys-id' // Nuovo campo
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
        <ControlledInput
          type="text"
          name="fields"
          value={formData.fields}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tabella:">
        <ControlledInput
          type="text"
          name="table"
          value={formData.table}
          onChange={handleChange}
        />
      </FormGroup>
      
      <SelectOptions>
        <OptionTabs>
          <OptionTab active={!formData.addJoin && !formData.useForAllEntries} onClick={() => setFormData({
            ...formData,
            addJoin: false,
            useForAllEntries: false
          })}>
            Select Standard
          </OptionTab>
          <OptionTab active={formData.addJoin} onClick={() => setFormData({
            ...formData,
            addJoin: true,
            useForAllEntries: false
          })}>
            Join
          </OptionTab>
          <OptionTab active={formData.useForAllEntries} onClick={() => setFormData({
            ...formData,
            addJoin: false,
            useForAllEntries: true
          })}>
            For All Entries
          </OptionTab>
        </OptionTabs>
        
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
              <ControlledInput
                type="text"
                name="joinTable"
                value={formData.joinTable}
                onChange={handleChange}
              />
            </FormGroup>
            
            <FormGroup label="Condizione JOIN:">
              <ControlledInput
                type="text"
                name="joinCondition"
                value={formData.joinCondition}
                onChange={handleChange}
              />
            </FormGroup>
          </>
        )}
        
        {formData.useForAllEntries && (
          <>
            <FormGroup label="Tabella FOR ALL ENTRIES:">
              <ControlledInput
                type="text"
                name="forAllEntriesTable"
                value={formData.forAllEntriesTable}
                onChange={handleChange}
              />
            </FormGroup>
            
            <FormGroup label="Condizione FOR ALL ENTRIES:">
              <ControlledInput
                type="text"
                name="forAllEntriesWhere"
                value={formData.forAllEntriesWhere}
                onChange={handleChange}
              />
            </FormGroup>
          </>
        )}
      </SelectOptions>
      
      <FormGroup label="INTO:">
        <ControlledInput
          type="text"
          name="into"
          value={formData.into}
          onChange={handleChange}
        />
      </FormGroup>
      
      {!formData.useForAllEntries && (
        <FormGroup label="WHERE (opzionale):">
          <ControlledInput
            type="text"
            name="where"
            value={formData.where}
            onChange={handleChange}
          />
        </FormGroup>
      )}
      
      <AdvancedOptions>
        <h4>Opzioni Avanzate</h4>
        
        <AdvancedSection>
          <FormGroup label="ORDER BY (opzionale):">
            <ControlledInput
              type="text"
              name="orderBy"
              value={formData.orderBy}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="GROUP BY (opzionale):">
            <ControlledInput
              type="text"
              name="groupBy"
              value={formData.groupBy}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="HAVING (opzionale):">
            <ControlledInput
              type="text"
              name="having"
              value={formData.having}
              onChange={handleChange}
            />
          </FormGroup>
        </AdvancedSection>
        
        <AdvancedSection>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useUnion"
              checked={formData.useUnion}
              onChange={handleChange}
              id="useUnion"
            />
            <label htmlFor="useUnion">Aggiungi UNION</label>
          </FormGroup>
          
          {formData.useUnion && (
            <>
              <FormGroup label="Tipo di UNION:">
                <select
                  name="unionType"
                  value={formData.unionType}
                  onChange={handleChange}
                >
                  <option value="UNION">UNION</option>
                  <option value="UNION ALL">UNION ALL</option>
                </select>
              </FormGroup>
              
              <FormGroup label="SELECT UNION:">
                <ControlledTextarea
                  name="unionSelect"
                  value={formData.unionSelect}
                  onChange={handleChange}
                  rows={5}
                />
              </FormGroup>
            </>
          )}
        </AdvancedSection>
      </AdvancedOptions>
      
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

const SelectOptions = styled.div`
  margin-bottom: 15px;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
`;

const OptionTabs = styled.div`
  display: flex;
  margin-bottom: 15px;
  border-bottom: 1px solid #ddd;
`;

const OptionTab = styled.div`
  padding: 8px 16px;
  cursor: pointer;
  border-bottom: 2px solid ${props => props.active ? '#0066cc' : 'transparent'};
  color: ${props => props.active ? '#0066cc' : '#333'};
  font-weight: ${props => props.active ? 'bold' : 'normal'};
  
  &:hover {
    background-color: #f9f9f9;
  }
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
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default SelectForm;