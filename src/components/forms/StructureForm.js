import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const StructureForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'ty_structure',
    components: [
      { id: 1, name: 'field1', type: 'STRING' },
      { id: 2, name: 'field2', type: 'INT4' }
    ]
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['structure']) {
      setFormData(formState['structure']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('structure', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value
    });
  };
  
  // Gestisce il cambiamento dei componenti
  const handleComponentChange = (id, field, value) => {
    setFormData({
      ...formData,
      components: formData.components.map(c => 
        c.id === id ? { ...c, [field]: value } : c
      )
    });
  };
  
  // Aggiunge un nuovo componente
  const handleAddComponent = () => {
    const newId = Math.max(0, ...formData.components.map(c => c.id)) + 1;
    setFormData({
      ...formData,
      components: [
        ...formData.components,
        { id: newId, name: `field${newId}`, type: 'STRING' }
      ]
    });
  };
  
  // Rimuove un componente
  const handleRemoveComponent = (id) => {
    setFormData({
      ...formData,
      components: formData.components.filter(c => c.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('structure', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome Structure:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Componenti:">
        {formData.components.map(component => (
          <ComponentItem key={component.id}>
            <ComponentHeader>
              <ComponentTitle>Componente {component.id}</ComponentTitle>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveComponent(component.id)}
              />
            </ComponentHeader>
            <FormGroup label="Nome:" inline>
              <ControlledInput type="text"
                value={component.name}
                onChange={(e) => handleComponentChange(component.id, 'name', e.target.value)}
              />
            </FormGroup>
            <FormGroup label="Tipo:" inline>
              <ControlledInput type="text"
                value={component.type}
                onChange={(e) => handleComponentChange(component.id, 'type', e.target.value)}
              />
            </FormGroup>
          </ComponentItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddComponent}
        >
          Aggiungi componente
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

const ComponentItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const ComponentHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const ComponentTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default StructureForm;
