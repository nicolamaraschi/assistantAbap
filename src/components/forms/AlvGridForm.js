import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const AlvGridForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    tableName: 'lt_data',
    fieldCatalog: 'SFLIGHT',
    layout: {
      zebra: true,
      cwidth_opt: true,
      sel_mode: 'A',
      grid_title: 'ALV Grid Title'
    },
    variant: {
      report: 'ZPROGRAM',
      handle: 'DEFAULT',
      username: 'SY-UNAME'
    },
    events: {
      addEvents: false,
      topOfPage: false,
      endOfPage: false,
      userCommand: false,
      hotspot: false,
      doubleClick: false
    },
    addSorting: false,
    addFiltering: false
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['alv-grid']) {
      setFormData(formState['alv-grid']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('alv-grid', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value
    });
  };
  
  // Gestisce il cambiamento delle opzioni di layout
  const handleLayoutChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      layout: {
        ...formData.layout,
        [name]: type === 'checkbox' ? checked : value
      }
    });
  };
  
  // Gestisce il cambiamento delle opzioni di variant
  const handleVariantChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      variant: {
        ...formData.variant,
        [name]: value
      }
    });
  };
  
  // Gestisce il cambiamento delle opzioni di eventi
  const handleEventChange = (e) => {
    const { name, checked } = e.target;
    setFormData({
      ...formData,
      events: {
        ...formData.events,
        [name]: checked
      }
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('alv-grid', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormGroup label="Nome tabella interna:">
        <ControlledInput type="text"
          name="tableName"
          value={formData.tableName}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Struttura per field catalog:">
        <ControlledInput type="text"
          name="fieldCatalog"
          value={formData.fieldCatalog}
          onChange={handleChange}
          placeholder="es. SFLIGHT o una struttura personalizzata"
        />
      </FormGroup>
      
      <FormGroup label="Opzioni di layout:">
        <LayoutOptions>
          <LayoutOption>
            <ControlledInput type="checkbox"
              name="zebra"
              checked={formData.layout.zebra}
              onChange={handleLayoutChange}
              id="zebraStripes"
            />
            <label htmlFor="zebraStripes">Righe zebrate</label>
          </LayoutOption>
          
          <LayoutOption>
            <ControlledInput type="checkbox"
              name="cwidth_opt"
              checked={formData.layout.cwidth_opt}
              onChange={handleLayoutChange}
              id="optimizeWidth"
            />
            <label htmlFor="optimizeWidth">Ottimizza larghezze colonne</label>
          </LayoutOption>
          
          <FormGroup label="Modalità selezione:">
            <select
              name="sel_mode"
              value={formData.layout.sel_mode}
              onChange={handleLayoutChange}
            >
              <option value="A">A - Selezione multipla con marker</option>
              <option value="B">B - Selezione multipla senza marker</option>
              <option value="C">C - Selezione singola con marker</option>
              <option value="D">D - Selezione singola senza marker</option>
            </select>
          </FormGroup>
          
          <FormGroup label="Titolo grid:">
        <ControlledInput type="text"
          name="grid_title"
          value={formData.layout.grid_title}
          onChange={handleLayoutChange}
          placeholder="Inserisci titolo della grid"
        />
      </FormGroup>
        </LayoutOptions>
      </FormGroup>
      
      <FormGroup label="Variant:">
        <VariantOptions>
          <FormGroup label="Report:">
            <ControlledInput type="text"
              name="report"
              value={formData.variant.report}
              onChange={handleVariantChange}
              placeholder="es. ZPROGRAM o SY-REPID"
            />
          </FormGroup>
          
          <FormGroup label="Handle:">
            <ControlledInput type="text"
              name="handle"
              value={formData.variant.handle}
              onChange={handleVariantChange}
              placeholder="es. DEFAULT"
            />
          </FormGroup>
          
          <FormGroup label="Username:">
            <ControlledInput type="text"
              name="username"
              value={formData.variant.username}
              onChange={handleVariantChange}
              placeholder="es. SY-UNAME o spazio vuoto"
            />
          </FormGroup>
        </VariantOptions>
      </FormGroup>
      
      <FormGroup label="Eventi:">
        <EventOptions>
          <EventOption>
            <ControlledInput type="checkbox"
              name="addEvents"
              checked={formData.events.addEvents}
              onChange={handleEventChange}
              id="addEvents"
            />
            <label htmlFor="addEvents">Aggiungi gestione eventi</label>
          </EventOption>
          
          {formData.events.addEvents && (
            <>
              <EventOption>
                <ControlledInput type="checkbox"
                  name="topOfPage"
                  checked={formData.events.topOfPage}
                  onChange={handleEventChange}
                  id="topOfPage"
                />
                <label htmlFor="topOfPage">TOP_OF_PAGE</label>
              </EventOption>
              
              <EventOption>
                <ControlledInput type="checkbox"
                  name="endOfPage"
                  checked={formData.events.endOfPage}
                  onChange={handleEventChange}
                  id="endOfPage"
                />
                <label htmlFor="endOfPage">END_OF_PAGE</label>
              </EventOption>
              
              <EventOption>
                <ControlledInput type="checkbox"
                  name="userCommand"
                  checked={formData.events.userCommand}
                  onChange={handleEventChange}
                  id="userCommand"
                />
                <label htmlFor="userCommand">USER_COMMAND</label>
              </EventOption>
              
              <EventOption>
                <ControlledInput type="checkbox"
                  name="hotspot"
                  checked={formData.events.hotspot}
                  onChange={handleEventChange}
                  id="hotspot"
                />
                <label htmlFor="hotspot">HOTSPOT_CLICK</label>
              </EventOption>
              
              <EventOption>
                <ControlledInput type="checkbox"
                  name="doubleClick"
                  checked={formData.events.doubleClick}
                  onChange={handleEventChange}
                  id="doubleClick"
                />
                <label htmlFor="doubleClick">DOUBLE_CLICK</label>
              </EventOption>
            </>
          )}
        </EventOptions>
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addSorting"
          checked={formData.addSorting}
          onChange={(e) => setFormData({...formData, addSorting: e.target.checked})}
          id="addSorting"
        />
        <label htmlFor="addSorting">Aggiungi ordinamento personalizzato</label>
      </FormGroup>
      
      <FormGroup inline>
        <ControlledInput type="checkbox"
          name="addFiltering"
          checked={formData.addFiltering}
          onChange={(e) => setFormData({...formData, addFiltering: e.target.checked})}
          id="addFiltering"
        />
        <label htmlFor="addFiltering">Aggiungi filtri personalizzati</label>
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
  
  input[type="checkbox"] {
    margin-right: 8px;
  }
`;

const LayoutOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const LayoutOption = styled.div`
  margin-bottom: 10px;
`;

const VariantOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const EventOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const EventOption = styled.div`
  margin-bottom: 10px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default AlvGridForm;
