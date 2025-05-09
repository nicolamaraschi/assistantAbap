// ClassForm.js - Migliorato con EVENTS e opzione Singleton
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const ClassForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    name: 'lcl_my_class',
    visibility: 'PUBLIC',
    definition: 'DEFINITION',
    superclass: '',
    interfaces: '',
    attributes: [
      { id: 1, name: 'mv_attribute', type: 'STRING', visibility: 'PRIVATE' }
    ],
    methods: [
      { id: 1, name: 'constructor', importing: '', exporting: '', returning: null }
    ],
    finalClass: false,
    events: [], // Nuovo campo per gli eventi
    generateGetSet: false, // Nuovo campo
    isSingleton: false // Nuovo campo
  });
  
  // Stato per la visualizzazione delle diverse sezioni
  const [activeSection, setActiveSection] = useState('attributes');
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['class']) {
      setFormData(formState['class']);
    }
  }, [formState]);
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('class', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce il cambiamento degli attributi
  const handleAttributeChange = (id, field, value) => {
    setFormData({
      ...formData,
      attributes: formData.attributes.map(a => 
        a.id === id ? { ...a, [field]: value } : a
      )
    });
  };
  
  // Aggiunge un nuovo attributo
  const handleAddAttribute = () => {
    const newId = Math.max(0, ...formData.attributes.map(a => a.id)) + 1;
    setFormData({
      ...formData,
      attributes: [
        ...formData.attributes,
        { id: newId, name: `mv_attr${newId}`, type: 'STRING', visibility: 'PRIVATE' }
      ]
    });
  };
  
  // Rimuove un attributo
  const handleRemoveAttribute = (id) => {
    setFormData({
      ...formData,
      attributes: formData.attributes.filter(a => a.id !== id)
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
        { id: newId, name: `method${newId}`, importing: '', exporting: '', returning: null, visibility: 'PUBLIC' }
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
  
  // Gestisce il cambiamento degli eventi
  const handleEventChange = (id, field, value) => {
    setFormData({
      ...formData,
      events: formData.events.map(e => 
        e.id === id ? { ...e, [field]: value } : e
      )
    });
  };
  
  // Aggiunge un nuovo evento
  const handleAddEvent = () => {
    const newId = Math.max(0, ...(formData.events || []).map(e => e.id), 0) + 1;
    setFormData({
      ...formData,
      events: [
        ...(formData.events || []),
        { id: newId, name: `event${newId}`, paramName: 'rv_data', paramType: 'STRING' }
      ]
    });
  };
  
  // Rimuove un evento
  const handleRemoveEvent = (id) => {
    setFormData({
      ...formData,
      events: formData.events.filter(e => e.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('class', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome classe:">
        <ControlledInput type="text"
          name="name"
          value={formData.name}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo di definizione:">
        <select
          name="definition"
          value={formData.definition}
          onChange={handleChange}
        >
          <option value="DEFINITION">DEFINITION</option>
          <option value="IMPLEMENTATION">IMPLEMENTATION</option>
          <option value="DEFINITION FINAL">DEFINITION FINAL</option>
          <option value="DEFINITION ABSTRACT">DEFINITION ABSTRACT</option>
          <option value="DEFINITION DEFERRED">DEFINITION DEFERRED</option>
        </select>
      </FormGroup>
      
      <FormGroup label="Superclasse (opzionale):">
        <ControlledInput type="text"
          name="superclass"
          value={formData.superclass}
          onChange={handleChange}
          placeholder="es. cl_super"
        />
      </FormGroup>
      
      <FormGroup label="Interfaces (opzionale):">
        <ControlledInput type="text"
          name="interfaces"
          value={formData.interfaces}
          onChange={handleChange}
          placeholder="es. if_serializable if_clonable"
        />
      </FormGroup>
      
      <AdvancedOptions>
        <h4>Opzioni Avanzate</h4>
        
        <FormGroup inline>
          <input
            type="checkbox"
            name="isSingleton"
            checked={formData.isSingleton}
            onChange={handleChange}
            id="isSingleton"
          />
          <label htmlFor="isSingleton">Classe Singleton (con GET_INSTANCE)</label>
        </FormGroup>
        
        <FormGroup inline>
          <input
            type="checkbox"
            name="generateGetSet"
            checked={formData.generateGetSet}
            onChange={handleChange}
            id="generateGetSet"
          />
          <label htmlFor="generateGetSet">Genera metodi getter/setter per attributi pubblici</label>
        </FormGroup>
      </AdvancedOptions>
      
      <TabsContainer>
        <TabHeader>
          <TabButton active={activeSection === 'attributes'} onClick={() => setActiveSection('attributes')}>
            Attributi
          </TabButton>
          <TabButton active={activeSection === 'methods'} onClick={() => setActiveSection('methods')}>
            Metodi
          </TabButton>
          <TabButton active={activeSection === 'events'} onClick={() => setActiveSection('events')}>
            Eventi
          </TabButton>
        </TabHeader>
        
        <TabContent>
          {activeSection === 'attributes' && (
            <FormGroup label="Attributi:">
              {formData.attributes.map(attribute => (
                <AttributeItem key={attribute.id}>
                  <AttributeHeader>
                    <AttributeTitle>Attributo {attribute.id}</AttributeTitle>
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveAttribute(attribute.id)}
                    />
                  </AttributeHeader>
                  <FormGroup label="Nome:">
                    <ControlledInput type="text"
                      value={attribute.name}
                      onChange={(e) => handleAttributeChange(attribute.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Tipo:">
                    <ControlledInput type="text"
                      value={attribute.type}
                      onChange={(e) => handleAttributeChange(attribute.id, 'type', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Visibilità:">
                    <select
                      value={attribute.visibility}
                      onChange={(e) => handleAttributeChange(attribute.id, 'visibility', e.target.value)}
                    >
                      <option value="PUBLIC">PUBLIC</option>
                      <option value="PROTECTED">PROTECTED</option>
                      <option value="PRIVATE">PRIVATE</option>
                    </select>
                  </FormGroup>
                </AttributeItem>
              ))}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddAttribute}
              >
                Aggiungi attributo
              </Button>
            </FormGroup>
          )}
          
          {activeSection === 'methods' && (
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
                      disabled={method.name === 'constructor' && formData.methods.length === 1}
                    />
                  </MethodHeader>
                  <FormGroup label="Nome:">
                    <ControlledInput type="text"
                      value={method.name}
                      onChange={(e) => handleMethodChange(method.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  <FormGroup label="Visibilità:">
                    <select
                      value={method.visibility || 'PUBLIC'}
                      onChange={(e) => handleMethodChange(method.id, 'visibility', e.target.value)}
                    >
                      <option value="PUBLIC">PUBLIC</option>
                      <option value="PROTECTED">PROTECTED</option>
                      <option value="PRIVATE">PRIVATE</option>
                    </select>
                  </FormGroup>
                  <FormGroup label="Parametri IMPORTING (opzionale):">
                    <ControlledTextarea
                      value={method.importing}
                      onChange={(e) => handleMethodChange(method.id, 'importing', e.target.value)}
                      rows={2}
                      placeholder="es. iv_param1 TYPE string"
                    />
                  </FormGroup>
                  <FormGroup label="Parametri EXPORTING (opzionale):">
                    <ControlledTextarea
                      value={method.exporting}
                      onChange={(e) => handleMethodChange(method.id, 'exporting', e.target.value)}
                      rows={2}
                      placeholder="es. ev_result TYPE string"
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
          
          {activeSection === 'events' && (
            <FormGroup label="Eventi:">
              {formData.events && formData.events.length > 0 ? (
                formData.events.map(event => (
                  <EventItem key={event.id}>
                    <EventHeader>
                      <EventTitle>Evento {event.id}</EventTitle>
                      <Button
                        variant="text"
                        size="small"
                        icon={<FiTrash2 />}
                        onClick={() => handleRemoveEvent(event.id)}
                      />
                    </EventHeader>
                    <FormGroup label="Nome evento:">
                      <ControlledInput type="text"
                        value={event.name}
                        onChange={(e) => handleEventChange(event.id, 'name', e.target.value)}
                      />
                    </FormGroup>
                    <FormGroup label="Parametro (nome):">
                      <ControlledInput type="text"
                        value={event.paramName}
                        onChange={(e) => handleEventChange(event.id, 'paramName', e.target.value)}
                      />
                    </FormGroup>
                    <FormGroup label="Parametro (tipo):">
                      <ControlledInput type="text"
                        value={event.paramType}
                        onChange={(e) => handleEventChange(event.id, 'paramType', e.target.value)}
                      />
                    </FormGroup>
                  </EventItem>
                ))
              ) : (
                <EmptyState>
                  Nessun evento definito. Aggiungi un evento per includere la dichiarazione EVENTS nella classe.
                </EmptyState>
              )}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddEvent}
              >
                Aggiungi evento
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
  
  textarea {
    resize: vertical;
    min-height: 50px;
  }
`;

const AdvancedOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 20px;
  margin-top: 15px;
  
  h4 {
    margin-top: 0;
    margin-bottom: 15px;
    font-size: 16px;
    color: #333;
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

const AttributeItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const AttributeHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const AttributeTitle = styled.div`
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

const EventItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const EventHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const EventTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const EmptyState = styled.div`
  background: #f0f4f8;
  padding: 15px;
  border-radius: 4px;
  text-align: center;
  margin-bottom: 15px;
  color: #666;
  font-style: italic;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default ClassForm;