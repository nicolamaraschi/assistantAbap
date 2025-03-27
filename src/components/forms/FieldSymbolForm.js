import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const FieldSymbolForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'fs',
    type: 'ANY',
    addMultiple: false,
    additionalSymbols: [
      { id: 1, name: 'fs_table', type: 'ANY TABLE' }
    ]
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['field-symbol']) {
      setFormData(formState['field-symbol']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('field-symbol', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce il cambiamento dei simboli aggiuntivi
  const handleSymbolChange = (id, field, value) => {
    setFormData({
      ...formData,
      additionalSymbols: formData.additionalSymbols.map(s => 
        s.id === id ? { ...s, [field]: value } : s
      )
    });
  };
  
  // Aggiunge un nuovo simbolo
  const handleAddSymbol = () => {
    const newId = Math.max(0, ...formData.additionalSymbols.map(s => s.id)) + 1;
    setFormData({
      ...formData,
      additionalSymbols: [
        ...formData.additionalSymbols,
        { id: newId, name: `fs_${newId}`, type: 'ANY' }
      ]
    });
  };
  
  // Rimuove un simbolo
  const handleRemoveSymbol = (id) => {
    setFormData({
      ...formData,
      additionalSymbols: formData.additionalSymbols.filter(s => s.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('field-symbol', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome Field-Symbol:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo:">
        <ControlledInput type="text"
          name="type"
          value={formData.type}
          onChange={handleChange}
          placeholder="es. ANY, STANDARD TABLE, ref TO cl_class"
        />
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addMultiple"
          checked={formData.addMultiple}
          onChange={handleChange}
          id="addMultiple"
        />
        <label htmlFor="addMultiple">Aggiungi field-symbols multipli</label>
      </FormGroup>
      
      {formData.addMultiple && (
        <AdditionalSymbols>
          <h4>Field-Symbols aggiuntivi:</h4>
          
          {formData.additionalSymbols.map(symbol => (
            <SymbolItem key={symbol.id}>
              <SymbolField>
                <label>Nome:</label>
                <ControlledInput type="text"
                  value={symbol.name}
                  onChange={(e) => handleSymbolChange(symbol.id, 'name', e.target.value)}
                />
              </SymbolField>
              
              <SymbolField>
                <label>Tipo:</label>
                <ControlledInput type="text"
                  value={symbol.type}
                  onChange={(e) => handleSymbolChange(symbol.id, 'type', e.target.value)}
                />
              </SymbolField>
              
              <Button
                variant="text"
                size="small"
                onClick={() => handleRemoveSymbol(symbol.id)}
              >
                Rimuovi
              </Button>
            </SymbolItem>
          ))}
          
          <Button
            variant="outline"
            size="small"
            onClick={handleAddSymbol}
          >
            Aggiungi Field-Symbol
          </Button>
        </AdditionalSymbols>
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

const AdditionalSymbols = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-top: 10px;
  margin-bottom: 15px;
  
  h4 {
    margin-top: 0;
    margin-bottom: 10px;
    font-size: 16px;
  }
`;

const SymbolItem = styled.div`
  display: flex;
  gap: 10px;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
  
  &:last-of-type {
    border-bottom: none;
  }
`;

const SymbolField = styled.div`
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

export default FieldSymbolForm;
