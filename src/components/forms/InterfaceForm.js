import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const InterfaceForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'if_my_interface',
    constants: [
      { id: 1, name: 'c_const', type: 'STRING', value: '\'Value\'' }
    ],
    methods: [
      { id: 1, name: 'method1', importing: 'iv_input TYPE string', exporting: 'ev_result TYPE string', raising: '' }
    ]
  });
  
  // Stato per la tab attiva (constants o methods)
  const [activeTab, setActiveTab] = useState('constants');
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['interface']) {
      setFormData(formState['interface']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('interface', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value
    });
  };
  
  // Gestisce il cambiamento delle costanti
  const handleConstantChange = (id, field, value) => {
    setFormData({
      ...formData,
      constants: formData.constants.map(c => 
        c.id === id ? { ...c, [field]: value } : c
      )
    });
  };
  
  // Aggiunge una nuova costante
  const handleAddConstant = () => {
    const newId = Math.max(0, ...formData.constants.map(c => c.id)) + 1;
    setFormData({
      ...formData,
      constants: [
        ...formData.constants,
        { id: newId, name: `c_const${newId}`, type: 'STRING', value: '\'Value\'' }
      ]
    });
  };
  
  // Rimuove una costante
  const handleRemoveConstant = (id) => {
    setFormData({
      ...formData,
      constants: formData.constants.filter(c => c.id !== id)
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
        { id: newId, name: `method${newId}`, importing: '', exporting: '', raising: '' }
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
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('interface', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome interfaccia:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <TabsContainer>
        <TabHeader>
          <TabButton 
            active={activeTab === 'constants'} 
            onClick={() => setActiveTab('constants')}
          >
            Costanti
          </TabButton>
          <TabButton 
            active={activeTab === 'methods'} 
            onClick={() => setActiveTab('methods')}
          >
            Metodi
          </TabButton>
        </TabHeader>
        
        <TabContent>
          {activeTab === 'constants' ? (
            <FormGroup label="Costanti:">
              {formData.constants.map(constant => (
                <ConstantItem key={constant.id}>
                  <ConstantHeader>
                    <ConstantTitle>Costante {constant.id}</ConstantTitle>
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveConstant(constant.id)}
                    />
                  </ConstantHeader>
                  <FormGroup label="Nome:">
                    <ControlledInput type="text"
                      value={constant.name}
                      onChange={(e) => handleConstantChange(constant.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Tipo:">
                    <ControlledInput type="text"
                      value={constant.type}
                      onChange={(e) => handleConstantChange(constant.id, 'type', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Valore:">
                    <ControlledInput type="text"
                      value={constant.value}
                      onChange={(e) => handleConstantChange(constant.id, 'value', e.target.value)}
                    />
                  </FormGroup>
                </ConstantItem>
              ))}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddConstant}
              >
                Aggiungi costante
              </Button>
            </FormGroup>
          ) : (
            <FormGroup label="Metodi:">
              {formData.methods.map(method => (
                <MethodItem key={method.id}>
                  <MethodHeader>
                    <MethodTitle>Metodo {method.id}</MethodTitle>
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveMethod(method.id)}
                    />
                  </MethodHeader>
                  <FormGroup label="Nome:">
                    <ControlledInput type="text"
                      value={method.name}
                      onChange={(e) => handleMethodChange(method.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Parametri IMPORTING:">
                    <ControlledTextarea
                      value={method.importing}
                      onChange={(e) => handleMethodChange(method.id, 'importing', e.target.value)}
                      rows={2}
                      placeholder="es. iv_param1 TYPE string"
                    />
                  </FormGroup>
                  <FormGroup label="Parametri EXPORTING:">
                    <ControlledTextarea
                      value={method.exporting}
                      onChange={(e) => handleMethodChange(method.id, 'exporting', e.target.value)}
                      rows={2}
                      placeholder="es. ev_result TYPE string"
                    />
                  </FormGroup>
                  <FormGroup label="RAISING:">
                    <ControlledInput type="text"
                      value={method.raising}
                      onChange={(e) => handleMethodChange(method.id, 'raising', e.target.value)}
                      placeholder="es. cx_sy_zerodivide cx_sy_conversion_error"
                    />
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
          )}
        </TabContent>
      </TabsContainer>
      
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

const TabsContainer = styled.div`
  margin-top: 20px;
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
`;

const TabHeader = styled.div`
  display: flex;
  border-bottom: 1px solid #ddd;
`;

const TabButton = styled.button`
  flex: 1;
  padding: 10px;
  background: ${props => props.active ? '#0066cc' : '#f5f5f5'};
  color: ${props => props.active ? 'white' : '#333'};
  border: none;
  cursor: pointer;
  transition: all 0.2s ease;
  
  &:hover {
    background: ${props => props.active ? '#0055aa' : '#e0e0e0'};
  }
  
  &:not(:last-child) {
    border-right: 1px solid #ddd;
  }
`;

const TabContent = styled.div`
  padding: 15px;
`;

const ConstantItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const ConstantHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const ConstantTitle = styled.div`
  font-weight: bold;
  color: #444;
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

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default InterfaceForm;