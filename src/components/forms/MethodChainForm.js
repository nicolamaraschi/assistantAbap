import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiArrowUp, FiArrowDown } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const MethodChainForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    object: 'lo_object',
    methods: [
      { id: 1, name: 'method1', params: [{ id: 1, name: 'iv_param1', value: 'lv_value1' }] },
      { id: 2, name: 'method2', params: [] }
    ]
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['method-chain']) {
      setFormData(formState['method-chain']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('method-chain', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value
    });
  };
  
  // Gestisce il cambiamento dei metodi
  const handleMethodChange = (id, field, value) => {
    setFormData({
      ...formData,
      methods: formData.methods.map(m => 
        m.id === id ? { ...m, [field]: value } : m
      )
    });
  };
  
  // Aggiunge un nuovo metodo
  const handleAddMethod = () => {
    const newId = Math.max(0, ...formData.methods.map(m => m.id)) + 1;
    setFormData({
      ...formData,
      methods: [
        ...formData.methods,
        { id: newId, name: `method${newId}`, params: [] }
      ]
    });
  };
  
  // Rimuove un metodo
  const handleRemoveMethod = (id) => {
    setFormData({
      ...formData,
      methods: formData.methods.filter(m => m.id !== id)
    });
  };
  
  // Sposta un metodo in alto
  const handleMoveMethodUp = (id) => {
    const index = formData.methods.findIndex(m => m.id === id);
    if (index > 0) {
      const newMethods = [...formData.methods];
      [newMethods[index - 1], newMethods[index]] = [newMethods[index], newMethods[index - 1]];
      setFormData({
        ...formData,
        methods: newMethods
      });
    }
  };
  
  // Sposta un metodo in basso
  const handleMoveMethodDown = (id) => {
    const index = formData.methods.findIndex(m => m.id === id);
    if (index < formData.methods.length - 1) {
      const newMethods = [...formData.methods];
      [newMethods[index], newMethods[index + 1]] = [newMethods[index + 1], newMethods[index]];
      setFormData({
        ...formData,
        methods: newMethods
      });
    }
  };
  
  // Gestisce il cambiamento dei parametri
  const handleParamChange = (methodId, paramId, field, value) => {
    setFormData({
      ...formData,
      methods: formData.methods.map(m => 
        m.id === methodId ? {
          ...m,
          params: m.params.map(p => 
            p.id === paramId ? { ...p, [field]: value } : p
          )
        } : m
      )
    });
  };
  
  // Aggiunge un nuovo parametro
  const handleAddParam = (methodId) => {
    const method = formData.methods.find(m => m.id === methodId);
    if (method) {
      const newId = Math.max(0, ...method.params.map(p => p.id), 0) + 1;
      setFormData({
        ...formData,
        methods: formData.methods.map(m => 
          m.id === methodId ? {
            ...m,
            params: [
              ...m.params,
              { id: newId, name: `iv_param${newId}`, value: `lv_value${newId}` }
            ]
          } : m
        )
      });
    }
  };
  
  // Rimuove un parametro
  const handleRemoveParam = (methodId, paramId) => {
    setFormData({
      ...formData,
      methods: formData.methods.map(m => 
        m.id === methodId ? {
          ...m,
          params: m.params.filter(p => p.id !== paramId)
        } : m
      )
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('method-chain', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Oggetto iniziale:">
        <ControlledInput type="text"
          name="object"
          value={formData.object}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Catena di metodi:">
        {formData.methods.map((method, index) => (
          <MethodItem key={method.id}>
            <MethodHeader>
              <MethodTitle>
                {index > 0 ? '->' : ''} {method.name}
              </MethodTitle>
              <MethodActions>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiArrowUp />}
                  onClick={() => handleMoveMethodUp(method.id)}
                  disabled={index === 0}
                  title="Sposta in alto"
                />
                <Button
                  variant="text"
                  size="small"
                  icon={<FiArrowDown />}
                  onClick={() => handleMoveMethodDown(method.id)}
                  disabled={index === formData.methods.length - 1}
                  title="Sposta in basso"
                />
                <Button
                  variant="text"
                  size="small"
                  icon={<FiTrash2 />}
                  onClick={() => handleRemoveMethod(method.id)}
                  disabled={formData.methods.length === 1}
                  title="Rimuovi metodo"
                />
              </MethodActions>
            </MethodHeader>
            
            <FormGroup label="Nome metodo:">
              <ControlledInput type="text"
                value={method.name}
                onChange={(e) => handleMethodChange(method.id, 'name', e.target.value)}
              />
            </FormGroup>
            
            <FormGroup label="Parametri:">
              {method.params.length > 0 ? (
                <ParamList>
                  {method.params.map(param => (
                    <ParamItem key={param.id}>
                      <ParamField>
                        <ControlledInput type="text"
                          value={param.name}
                          onChange={(e) => handleParamChange(method.id, param.id, 'name', e.target.value)}
                          placeholder="Nome parametro"
                        />
                      </ParamField>
                      <ParamEquals>=</ParamEquals>
                      <ParamField>
                        <ControlledInput type="text"
                          value={param.value}
                          onChange={(e) => handleParamChange(method.id, param.id, 'value', e.target.value)}
                          placeholder="Valore"
                        />
                      </ParamField>
                      <Button
                        variant="text"
                        size="small"
                        icon={<FiTrash2 />}
                        onClick={() => handleRemoveParam(method.id, param.id)}
                        title="Rimuovi parametro"
                      />
                    </ParamItem>
                  ))}
                </ParamList>
              ) : (
                <NoParams>Nessun parametro</NoParams>
              )}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={() => handleAddParam(method.id)}
              >
                Aggiungi parametro
              </Button>
            </FormGroup>
          </MethodItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddMethod}
        >
          Aggiungi metodo
        </Button>
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

const MethodItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const MethodHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const MethodTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const MethodActions = styled.div`
  display: flex;
  gap: 5px;
`;

const ParamList = styled.div`
  margin-bottom: 10px;
`;

const ParamItem = styled.div`
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
`;

const ParamField = styled.div`
  flex: 1;
`;

const ParamEquals = styled.div`
  font-weight: bold;
  color: #666;
`;

const NoParams = styled.div`
  color: #999;
  font-style: italic;
  margin-bottom: 10px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default MethodChainForm;
