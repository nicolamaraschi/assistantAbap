import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiChevronDown, FiChevronUp } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

/**
 * Form per la generazione di applicazioni SAP Fiori
 * Permette la configurazione di vari aspetti dell'applicazione:
 * - Informazioni base (titolo, tipo, ID)
 * - Configurazione OData (servizio, entity set, navigazione)
 * - Interfaccia utente (layout, controlli, azioni)
 * - Impostazioni avanzate (bozze, autenticazione, internazionalizzazione)
 */
const FioriForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    appType: 'transactional',
    appTitle: 'My Fiori App',
    appDescription: 'SAP Fiori Application',
    appId: 'z.myfioriapp',
    oDataService: 'ZMY_ODATA_SERVICE',
    entitySet: 'MyEntitySet',
    navigationProperty: 'ID',
    includeAnnotations: true,
    includeAnalytics: false,
    includeCustomActions: false,
    customActions: [
      { id: 1, name: 'export', label: 'Export', icon: 'sap-icon://excel-attachment' }
    ],
    includeDraftHandling: false,
    includeFlexibleColumnLayout: false,
    useSmartControls: true,
    addAuthentication: true,
    i18nSupport: true,
    supportedLanguages: ['EN', 'DE']
  });
  
  // Stato per le sezioni collassabili
  const [expandedSections, setExpandedSections] = useState({
    basic: true,
    odata: true,
    ui: false,
    advanced: false
  });
  
      // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['flower']) { // 'flower' è l'ID usato per SAP Fiori nel sistema
      setFormData(formState['flower']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('flower', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData(prevData => ({
      ...prevData,
      [name]: type === 'checkbox' ? checked : value
    }));
  };
  
  // Gestisce il cambiamento dei campi annidati
  const handleNestedChange = (section, field, value) => {
    setFormData(prevData => ({
      ...prevData,
      [section]: {
        ...prevData[section],
        [field]: value
      }
    }));
  };
  
  // Gestisce l'aggiunta di una custom action
  const handleAddCustomAction = () => {
    const newId = Math.max(0, ...formData.customActions.map(a => a.id)) + 1;
    setFormData(prevData => ({
      ...prevData,
      customActions: [
        ...prevData.customActions,
        { id: newId, name: `action${newId}`, label: `Action ${newId}`, icon: 'sap-icon://action' }
      ]
    }));
  };
  
  // Gestisce la rimozione di una custom action
  const handleRemoveCustomAction = (id) => {
    setFormData(prevData => ({
      ...prevData,
      customActions: prevData.customActions.filter(a => a.id !== id)
    }));
  };
  
  // Gestisce il cambiamento di una custom action
  const handleCustomActionChange = (id, field, value) => {
    setFormData(prevData => ({
      ...prevData,
      customActions: prevData.customActions.map(action => 
        action.id === id ? { ...action, [field]: value } : action
      )
    }));
  };
  
  // Toggle espansione/collasso sezioni
  const toggleSection = (section) => {
    setExpandedSections(prevState => ({
      ...prevState,
      [section]: !prevState[section]
    }));
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('flower', formData);
    }
  };
  
  return (
    <FormContainer>
      <FormHeader>
        <h3>Generatore di Applicazioni SAP Fiori</h3>
        <FormHint>Configura le opzioni per generare un'applicazione SAP Fiori basata su UI5</FormHint>
      </FormHeader>
      
      {/* Sezione Informazioni Base */}
      <SectionHeader onClick={() => toggleSection('basic')}>
        <h4>Informazioni di Base</h4>
        {expandedSections.basic ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.basic && (
        <SectionContent>
          <FormGroup label="Tipo di Applicazione:">
            <select
              name="appType"
              value={formData.appType}
              onChange={handleChange}
            >
              <option value="transactional">Transazionale (CRUD)</option>
              <option value="analytical">Analitica</option>
              <option value="factsheet">Scheda Informativa</option>
              <option value="list">Elenco</option>
            </select>
          </FormGroup>

          <FormGroup label="Titolo Applicazione:">
            <ControlledInput
              type="text"
              name="appTitle"
              value={formData.appTitle}
              onChange={handleChange}
              placeholder="Titolo visualizzato nell'app"
            />
          </FormGroup>
          
          <FormGroup label="Descrizione:">
            <ControlledInput
              type="text"
              name="appDescription"
              value={formData.appDescription}
              onChange={handleChange}
              placeholder="Breve descrizione dell'applicazione"
            />
          </FormGroup>
          
          <FormGroup label="ID Applicazione:">
            <ControlledInput
              type="text"
              name="appId"
              value={formData.appId}
              onChange={handleChange}
              placeholder="es. z.myfioriapp"
            />
          </FormGroup>
        </SectionContent>
      )}
      
      {/* Sezione OData */}
      <SectionHeader onClick={() => toggleSection('odata')}>
        <h4>Configurazione OData</h4>
        {expandedSections.odata ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.odata && (
        <SectionContent>
          <FormGroup label="Servizio OData:">
            <ControlledInput
              type="text"
              name="oDataService"
              value={formData.oDataService}
              onChange={handleChange}
              placeholder="es. ZMY_ODATA_SERVICE"
            />
          </FormGroup>
          
          <FormGroup label="EntitySet:">
            <ControlledInput
              type="text"
              name="entitySet"
              value={formData.entitySet}
              onChange={handleChange}
              placeholder="es. Products"
            />
          </FormGroup>
          
          <FormGroup label="Proprietà di Navigazione (per dettaglio):">
            <ControlledInput
              type="text"
              name="navigationProperty"
              value={formData.navigationProperty}
              onChange={handleChange}
              placeholder="es. ID o ProductID"
            />
            <FormHint>Lascia vuoto se non è necessaria la navigazione al dettaglio</FormHint>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeAnnotations"
              checked={formData.includeAnnotations}
              onChange={handleChange}
              id="includeAnnotations"
            />
            <label htmlFor="includeAnnotations">Includi Annotazioni OData</label>
            <FormHint>Le annotazioni definiscono aspetti di visualizzazione UI</FormHint>
          </FormGroup>
        </SectionContent>
      )}

      {/* Sezione UI */}
      <SectionHeader onClick={() => toggleSection('ui')}>
        <h4>Configurazione UI</h4>
        {expandedSections.ui ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.ui && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useSmartControls"
              checked={formData.useSmartControls}
              onChange={handleChange}
              id="useSmartControls"
            />
            <label htmlFor="useSmartControls">Usa Smart Controls (SmartTable, SmartForm)</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeFlexibleColumnLayout"
              checked={formData.includeFlexibleColumnLayout}
              onChange={handleChange}
              id="includeFlexibleColumnLayout"
            />
            <label htmlFor="includeFlexibleColumnLayout">Usa Flexible Column Layout</label>
            <FormHint>Layout a colonne flessibili in stile SAP Fiori 3</FormHint>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeAnalytics"
              checked={formData.includeAnalytics}
              onChange={handleChange}
              id="includeAnalytics"
            />
            <label htmlFor="includeAnalytics">Includi Componenti Analitici</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeCustomActions"
              checked={formData.includeCustomActions}
              onChange={handleChange}
              id="includeCustomActions"
            />
            <label htmlFor="includeCustomActions">Aggiungi Azioni Personalizzate</label>
          </FormGroup>
          
          {formData.includeCustomActions && (
            <ActionsContainer>
              {formData.customActions.map(action => (
                <ActionItem key={action.id}>
                  <ActionHeader>
                    <ActionTitle>Azione {action.id}</ActionTitle>
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveCustomAction(action.id)}
                    />
                  </ActionHeader>
                  
                  <ActionGrid>
                    <FormGroup label="Nome:">
                      <ControlledInput
                        type="text"
                        value={action.name}
                        onChange={(e) => handleCustomActionChange(action.id, 'name', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Etichetta:">
                      <ControlledInput
                        type="text"
                        value={action.label}
                        onChange={(e) => handleCustomActionChange(action.id, 'label', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Icona:">
                      <ControlledInput
                        type="text"
                        value={action.icon}
                        onChange={(e) => handleCustomActionChange(action.id, 'icon', e.target.value)}
                        placeholder="es. sap-icon://action"
                      />
                    </FormGroup>
                  </ActionGrid>
                </ActionItem>
              ))}
              
              <Button
                variant="outline"
                size="small"
                icon={<FiPlus />}
                onClick={handleAddCustomAction}
              >
                Aggiungi Azione
              </Button>
            </ActionsContainer>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Avanzata */}
      <SectionHeader onClick={() => toggleSection('advanced')}>
        <h4>Configurazione Avanzata</h4>
        {expandedSections.advanced ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.advanced && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeDraftHandling"
              checked={formData.includeDraftHandling}
              onChange={handleChange}
              id="includeDraftHandling"
            />
            <label htmlFor="includeDraftHandling">Includi Gestione Bozze</label>
            <FormHint>Per abilitare il salvataggio di bozze dei dati</FormHint>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="addAuthentication"
              checked={formData.addAuthentication}
              onChange={handleChange}
              id="addAuthentication"
            />
            <label htmlFor="addAuthentication">Aggiungi Autenticazione</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="i18nSupport"
              checked={formData.i18nSupport}
              onChange={handleChange}
              id="i18nSupport"
            />
            <label htmlFor="i18nSupport">Supporto Internazionalizzazione (i18n)</label>
          </FormGroup>
          
          {formData.i18nSupport && (
            <FormGroup label="Lingue Supportate:">
              <ControlledInput
                type="text"
                value={formData.supportedLanguages.join(', ')}
                onChange={(e) => setFormData({
                  ...formData,
                  supportedLanguages: e.target.value.split(',').map(lang => lang.trim())
                })}
                placeholder="es. EN, DE, IT, FR"
              />
            </FormGroup>
          )}
        </SectionContent>
      )}
      
      <ButtonContainer>
        <Button 
          variant="primary" 
          onClick={handleGenerate}
          fullWidth
        >
          Genera Codice SAP Fiori
        </Button>
      </ButtonContainer>
    </FormContainer>
  );
};

// Stili del componente
const FormContainer = styled.div`
  padding: 15px;
  max-width: 100%;
`;

const FormHeader = styled.div`
  margin-bottom: 20px;
  
  h3 {
    margin-top: 0;
    margin-bottom: 8px;
    color: #0066cc;
  }
`;

const FormHint = styled.div`
  color: #666;
  font-size: 12px;
  margin-top: 3px;
  font-style: italic;
`;

const SectionHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 15px;
  background: #f0f4f8;
  border: 1px solid #ddd;
  border-radius: 6px;
  margin-bottom: 10px;
  cursor: pointer;
  transition: background-color 0.2s;
  
  &:hover {
    background: #e6eff7;
  }
  
  h4 {
    margin: 0;
    font-size: 16px;
    color: #333;
  }
`;

const SectionContent = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 0 0 6px 6px;
  padding: 15px;
  margin-top: -10px;
  margin-bottom: 15px;
  border-top: none;
  animation: fadeIn 0.3s ease;
  
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(-10px); }
    to { opacity: 1; transform: translateY(0); }
  }
`;

const ActionGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 15px;
`;

const ActionsContainer = styled.div`
  margin-top: 15px;
  padding: 10px;
  background: #f0f4f8;
  border-radius: 6px;
  
  > button {
    margin-top: 10px;
  }
`;

const ActionItem = styled.div`
  background: #fff;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 10px;
`;

const ActionHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const ActionTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default FioriForm;