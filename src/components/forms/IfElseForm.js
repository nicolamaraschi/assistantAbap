// IfElseForm.js - Migliorato con supporto multi-ELSEIF
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const IfElseForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    condition: 'campo = valore',
    trueAction: 'WRITE: / \'Condizione verificata\'.',
    falseAction: 'WRITE: / \'Condizione non verificata\'.',
    addElseIf: false,
    elseIfConditions: [], // Nuovo array per multiple condizioni ELSEIF
    elseIfCondition: 'campo = altro_valore', // Mantenuto per retrocompatibilità
    elseIfAction: 'WRITE: / \'Condizione ELSEIF verificata\'.' // Mantenuto per retrocompatibilità
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['if-else']) {
      setFormData(formState['if-else']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('if-else', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce il cambiamento delle condizioni ELSEIF
  const handleElseIfChange = (index, field, value) => {
    const newElseIfConditions = [...formData.elseIfConditions];
    newElseIfConditions[index] = {
      ...newElseIfConditions[index],
      [field]: value
    };
    
    setFormData({
      ...formData,
      elseIfConditions: newElseIfConditions
    });
  };
  
  // Aggiunge una nuova condizione ELSEIF
  const handleAddElseIf = () => {
    // Se è la prima condizione aggiunta e il vecchio formato ha dati, convertiamoli
    if (formData.elseIfConditions.length === 0 && formData.elseIfCondition) {
      setFormData({
        ...formData,
        elseIfConditions: [
          {
            condition: formData.elseIfCondition,
            action: formData.elseIfAction
          }
        ]
      });
    } else {
      setFormData({
        ...formData,
        elseIfConditions: [
          ...formData.elseIfConditions,
          {
            condition: 'altra_condizione',
            action: 'WRITE: / \'Altra condizione verificata\'.'
          }
        ]
      });
    }
  };
  
  // Rimuove una condizione ELSEIF
  const handleRemoveElseIf = (index) => {
    const newElseIfConditions = [...formData.elseIfConditions];
    newElseIfConditions.splice(index, 1);
    
    setFormData({
      ...formData,
      elseIfConditions: newElseIfConditions
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('if-else', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Condizione:">
        <ControlledInput
          type="text"
          name="condition"
          value={formData.condition}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Blocco IF:">
        <ControlledTextarea
          name="trueAction"
          value={formData.trueAction}
          onChange={handleChange}
          rows={4}
        />
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="addElseIf"
          checked={formData.addElseIf}
          onChange={handleChange}
          id="addElseIf"
        />
        <label htmlFor="addElseIf">Aggiungi blocchi ELSEIF</label>
      </FormGroup>
      
      {formData.addElseIf && (
        <>
          {formData.elseIfConditions.length > 0 ? (
            // Nuovo formato multi-ELSEIF
            <ElseIfContainer>
              {formData.elseIfConditions.map((elseIf, index) => (
                <ElseIfBlock key={index}>
                  <ElseIfHeader>
                    <h4>Blocco ELSEIF {index + 1}</h4>
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveElseIf(index)}
                    />
                  </ElseIfHeader>
                  
                  <FormGroup label="Condizione ELSEIF:">
                    <ControlledInput
                      type="text"
                      value={elseIf.condition}
                      onChange={(e) => handleElseIfChange(index, 'condition', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Blocco ELSEIF:">
                    <ControlledTextarea
                      value={elseIf.action}
                      onChange={(e) => handleElseIfChange(index, 'action', e.target.value)}
                      rows={3}
                    />
                  </FormGroup>
                </ElseIfBlock>
              ))}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddElseIf}
              >
                Aggiungi altro ELSEIF
              </Button>
            </ElseIfContainer>
          ) : (
            // Vecchio formato retro-compatibile
            <>
              <FormGroup label="Condizione ELSEIF:">
                <ControlledInput
                  type="text"
                  name="elseIfCondition"
                  value={formData.elseIfCondition}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Blocco ELSEIF:">
                <ControlledTextarea
                  name="elseIfAction"
                  value={formData.elseIfAction}
                  onChange={handleChange}
                  rows={4}
                />
              </FormGroup>
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddElseIf}
              >
                Aggiungi altro ELSEIF
              </Button>
            </>
          )}
        </>
      )}
      
      <FormGroup label="Blocco ELSE:">
        <ControlledTextarea
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

const ElseIfContainer = styled.div`
  margin-bottom: 15px;
`;

const ElseIfBlock = styled.div`
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const ElseIfHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  
  h4 {
    margin: 0;
    font-size: 16px;
    color: #333;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default IfElseForm;