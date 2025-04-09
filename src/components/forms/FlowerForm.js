import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiChevronDown, FiChevronUp, FiCopy } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';
import ControlledSelect from '../common/ControlledSelect';

/**
 * Form per la generazione di applicazioni SAP Fiori
 * Permette la configurazione di vari aspetti dell'applicazione:
 * - Informazioni base (titolo, tipo, ID)
 * - Configurazione OData (servizio, entity set, navigazione)
 * - Interfaccia utente (layout, controlli, azioni)
 * - Impostazioni avanzate (bozze, autenticazione, internazionalizzazione)
 * - Generazione di modelli di codice standard
 */
const FioriForm = ({ onGenerate }) => {
  // Stato locale del form con valori predefiniti migliorati
  const [formData, setFormData] = useState({
    // Informazioni base
    appType: 'transactional',
    appTitle: 'My SAP Fiori App',
    appDescription: 'SAP Fiori Application for Business Process',
    appId: 'z.myfioriapp',
    namespace: 'com.mycompany',
    
    // Configurazione OData
    oDataService: 'ZMY_ODATA_SERVICE',
    oDataVersion: 'v2',
    entitySet: 'MyEntitySet',
    navigationProperty: 'ID',
    includeAnnotations: true,
    annotationsSource: 'LOCAL_ANNOTATIONS',
    
    // Funzionalità UI
    includeAnalytics: false,
    includeCustomActions: false,
    customActions: [
      { id: 1, name: 'export', label: 'Export', icon: 'sap-icon://excel-attachment' }
    ],
    includeDraftHandling: false,
    includeFlexibleColumnLayout: true,
    useSmartControls: true,
    smartFields: ['ID', 'Title', 'Description', 'CreatedAt', 'Status'],
    
    // Impostazioni avanzate
    addAuthentication: true,
    authenticationMethod: 'SAML2',
    i18nSupport: true,
    supportedLanguages: ['EN', 'DE', 'IT', 'FR'],
    includePersonalization: true,
    includeOfflineCapabilities: false,
    
    // Componenti di codice
    generateController: true,
    generateView: true,
    generateI18n: true,
    generateManifest: true,
    generateComponent: true,
    
    // Template specifici
    templateType: 'standard',
    
    // Script di compilazione
    includeBuildScripts: true,
    useUi5Tooling: true
  }); // <-- Chiudi correttamente l'oggetto qui
  
const TwoColumnsLayout = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 15px;
  
  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
`;

const ComponentsGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
  gap: 10px;
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
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
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

const ChipContainer = styled.div`
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-bottom: 10px;
  
  button {
    margin-left: 5px;
  }
`;

const Chip = styled.div`
  display: flex;
  align-items: center;
  background: #e6eff7;
  border-radius: 16px;
  padding: 5px 10px;
  font-size: 14px;
  
  input {
    background: transparent;
    border: none;
    padding: 0;
    margin: 0;
    width: auto;
    max-width: 100px;
    
    &:focus {
      outline: none;
      box-shadow: none;
    }
  }
  
  .chip-button {
    padding: 2px;
    color: #666;
    
    &:hover {
      color: #cc0000;
    }
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
  display: flex;
  justify-content: center;
`;
  
  // Stato per le sezioni collassabili
  const [expandedSections, setExpandedSections] = useState({
    basic: true,
    odata: true,
    ui: false,
    components: false,
    advanced: false,
    templates: false,
    build: false
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['fiori']) {
      setFormData(formState['fiori']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('fiori', formData);
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
    const newId = formData.customActions.length > 0 
      ? Math.max(...formData.customActions.map(a => a.id)) + 1 
      : 1;
    
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
  
  // Gestisce l'aggiunta di un linguaggio
  const handleAddLanguage = () => {
    setFormData(prevData => ({
      ...prevData,
      supportedLanguages: [...new Set([...prevData.supportedLanguages, 'ES'])]
    }));
  };
  
  // Gestisce la rimozione di un linguaggio
  const handleRemoveLanguage = (lang) => {
    setFormData(prevData => ({
      ...prevData,
      supportedLanguages: prevData.supportedLanguages.filter(l => l !== lang)
    }));
  };
  
  // Gestisce l'aggiunta di un smart field
  const handleAddSmartField = () => {
    setFormData(prevData => ({
      ...prevData,
      smartFields: [...new Set([...prevData.smartFields, 'NewField'])]
    }));
  };
  
  // Gestisce la rimozione di un smart field
  const handleRemoveSmartField = (field) => {
    setFormData(prevData => ({
      ...prevData,
      smartFields: prevData.smartFields.filter(f => f !== field)
    }));
  };
  
  // Gestisce il cambiamento di un smart field
  const handleSmartFieldChange = (index, value) => {
    setFormData(prevData => {
      const newFields = [...prevData.smartFields];
      newFields[index] = value;
      return {
        ...prevData,
        smartFields: newFields
      };
    });
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
      onGenerate('fiori', formData);
    }
  };
  
  // Template di applicazioni predefiniti
  const appTemplates = [
    { value: 'standard', label: 'Standard (CRUD Application)' },
    { value: 'analytical', label: 'Analitica (Dashboard)' },
    { value: 'worklist', label: 'Lista di Lavoro' },
    { value: 'masterDetail', label: 'Master-Detail' },
    { value: 'overview', label: 'Panoramica' },
    { value: 'processFLow', label: 'Flusso di Processo' },
    { value: 'approval', label: 'Approvazione' }
  ];
  
  // OData versions
  const oDataVersions = [
    { value: 'v2', label: 'OData V2' },
    { value: 'v4', label: 'OData V4' }
  ];
  
  // Lista di icone comuni per le azioni
  const commonIcons = [
    { value: 'sap-icon://excel-attachment', label: 'Export Excel' },
    { value: 'sap-icon://pdf-attachment', label: 'Export PDF' },
    { value: 'sap-icon://email', label: 'Email' },
    { value: 'sap-icon://action', label: 'Action' },
    { value: 'sap-icon://add', label: 'Add' },
    { value: 'sap-icon://delete', label: 'Delete' },
    { value: 'sap-icon://edit', label: 'Edit' },
    { value: 'sap-icon://save', label: 'Save' },
    { value: 'sap-icon://print', label: 'Print' },
    { value: 'sap-icon://refresh', label: 'Refresh' }
  ];
  
  return (
    <FormContainer>
      <FormHeader>
        <h3>Generatore di Applicazioni SAP Fiori</h3>
        <FormHint>Configura le opzioni per generare il codice di un'applicazione SAP Fiori basata su UI5</FormHint>
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
              className="form-select"
            >
              <option value="transactional">Transazionale (CRUD)</option>
              <option value="analytical">Analitica (Dashboard)</option>
              <option value="factsheet">Scheda Informativa</option>
              <option value="list">Lista di Record</option>
              <option value="masterDetail">Master-Detail</option>
              <option value="processFlow">Flusso di Processo</option>
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
          
          <TwoColumnsLayout>
            <FormGroup label="Namespace:">
              <ControlledInput
                type="text"
                name="namespace"
                value={formData.namespace}
                onChange={handleChange}
                placeholder="es. com.mycompany"
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
          </TwoColumnsLayout>
        </SectionContent>
      )}
      
      {/* Sezione OData */}
      <SectionHeader onClick={() => toggleSection('odata')}>
        <h4>Configurazione OData</h4>
        {expandedSections.odata ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.odata && (
        <SectionContent>
          <TwoColumnsLayout>
            <FormGroup label="Servizio OData:">
              <ControlledInput
                type="text"
                name="oDataService"
                value={formData.oDataService}
                onChange={handleChange}
                placeholder="es. ZMY_ODATA_SERVICE"
              />
            </FormGroup>
            
            <FormGroup label="Versione OData:">
              <select 
                name="oDataVersion"
                value={formData.oDataVersion}
                onChange={handleChange}
                className="form-select"
              >
                {oDataVersions.map(version => (
                  <option key={version.value} value={version.value}>
                    {version.label}
                  </option>
                ))}
              </select>
            </FormGroup>
          </TwoColumnsLayout>
          
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
            <FormHint>Proprietà usata per la navigazione al dettaglio (lascia vuoto se non necessaria)</FormHint>
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
          
          {formData.includeAnnotations && (
            <FormGroup label="Origine Annotazioni:">
              <select
                name="annotationsSource"
                value={formData.annotationsSource}
                onChange={handleChange}
                className="form-select"
              >
                <option value="LOCAL_ANNOTATIONS">File locale (annotations.xml)</option>
                <option value="METADATA_ANNOTATIONS">Metadata del servizio</option>
                <option value="SAP_GATEWAY_ANNOTATIONS">SAP Gateway</option>
              </select>
            </FormGroup>
          )}
        </SectionContent>
      )}

      {/* Sezione UI */}
      <SectionHeader onClick={() => toggleSection('ui')}>
        <h4>Configurazione UI</h4>
        {expandedSections.ui ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.ui && (
        <SectionContent>
          <TwoColumnsLayout>
            <FormGroup inline>
              <input
                type="checkbox"
                name="useSmartControls"
                checked={formData.useSmartControls}
                onChange={handleChange}
                id="useSmartControls"
              />
              <label htmlFor="useSmartControls">Usa Smart Controls</label>
              <FormHint>SmartTable, SmartForm, ecc.</FormHint>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="includeFlexibleColumnLayout"
                checked={formData.includeFlexibleColumnLayout}
                onChange={handleChange}
                id="includeFlexibleColumnLayout"
              />
              <label htmlFor="includeFlexibleColumnLayout">Flexible Column Layout</label>
              <FormHint>Layout a colonne flessibili (Fiori 3)</FormHint>
            </FormGroup>
          </TwoColumnsLayout>
          
          {formData.useSmartControls && (
            <FormGroup label="Smart Fields:">
              <ChipContainer>
                {formData.smartFields.map((field, index) => (
                  <Chip key={index}>
                    <ControlledInput
                      type="text"
                      value={field}
                      onChange={(e) => handleSmartFieldChange(index, e.target.value)}
                      className="chip-input"
                    />
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveSmartField(field)}
                    />
                  </Chip>
                ))}
                <Button
                  variant="text"
                  size="small"
                  icon={<FiPlus />}
                  onClick={handleAddSmartField}
                >
                  Aggiungi Campo
                </Button>
              </ChipContainer>
              <FormHint>Campi da visualizzare come SmartFields nell'app</FormHint>
            </FormGroup>
          )}
          
          <TwoColumnsLayout>
            <FormGroup inline>
              <input
                type="checkbox"
                name="includeAnalytics"
                checked={formData.includeAnalytics}
                onChange={handleChange}
                id="includeAnalytics"
              />
              <label htmlFor="includeAnalytics">Componenti Analitici</label>
              <FormHint>Grafici, KPI, dashboard</FormHint>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="includeCustomActions"
                checked={formData.includeCustomActions}
                onChange={handleChange}
                id="includeCustomActions"
              />
              <label htmlFor="includeCustomActions">Azioni Personalizzate</label>
            </FormGroup>
          </TwoColumnsLayout>
          
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
                    <FormGroup label="Nome Funzione:">
                      <ControlledInput
                        type="text"
                        value={action.name}
                        onChange={(e) => handleCustomActionChange(action.id, 'name', e.target.value)}
                        placeholder="es. exportToExcel"
                      />
                    </FormGroup>
                    
                    <FormGroup label="Etichetta UI:">
                      <ControlledInput
                        type="text"
                        value={action.label}
                        onChange={(e) => handleCustomActionChange(action.id, 'label', e.target.value)}
                        placeholder="es. Esporta in Excel"
                      />
                    </FormGroup>
                    
                    <FormGroup label="Icona:">
                      <select
                        value={action.icon}
                        onChange={(e) => handleCustomActionChange(action.id, 'icon', e.target.value)}
                        className="form-select"
                      >
                        {commonIcons.map(icon => (
                          <option key={icon.value} value={icon.value}>
                            {icon.label} ({icon.value})
                          </option>
                        ))}
                      </select>
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
      
      {/* Sezione Componenti */}
      <SectionHeader onClick={() => toggleSection('components')}>
        <h4>Componenti di Codice</h4>
        {expandedSections.components ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.components && (
        <SectionContent>
          <ComponentsGrid>
            <FormGroup inline>
              <input
                type="checkbox"
                name="generateController"
                checked={formData.generateController}
                onChange={handleChange}
                id="generateController"
              />
              <label htmlFor="generateController">Controller</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="generateView"
                checked={formData.generateView}
                onChange={handleChange}
                id="generateView"
              />
              <label htmlFor="generateView">View XML</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="generateI18n"
                checked={formData.generateI18n}
                onChange={handleChange}
                id="generateI18n"
              />
              <label htmlFor="generateI18n">File i18n</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="generateManifest"
                checked={formData.generateManifest}
                onChange={handleChange}
                id="generateManifest"
              />
              <label htmlFor="generateManifest">Manifest.json</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="generateComponent"
                checked={formData.generateComponent}
                onChange={handleChange}
                id="generateComponent"
              />
              <label htmlFor="generateComponent">Component.js</label>
            </FormGroup>
          </ComponentsGrid>
        </SectionContent>
      )}
      
      {/* Sezione Template */}
      <SectionHeader onClick={() => toggleSection('templates')}>
        <h4>Template Predefiniti</h4>
        {expandedSections.templates ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.templates && (
        <SectionContent>
          <FormGroup label="Seleziona Template:">
            <select
              name="templateType"
              value={formData.templateType}
              onChange={handleChange}
              className="form-select"
            >
              {appTemplates.map(template => (
                <option key={template.value} value={template.value}>
                  {template.label}
                </option>
              ))}
            </select>
            <FormHint>Template predefiniti con struttura e funzionalità comuni</FormHint>
          </FormGroup>
        </SectionContent>
      )}
      
      {/* Sezione Build */}
      <SectionHeader onClick={() => toggleSection('build')}>
        <h4>Script di Compilazione</h4>
        {expandedSections.build ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.build && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeBuildScripts"
              checked={formData.includeBuildScripts}
              onChange={handleChange}
              id="includeBuildScripts"
            />
            <label htmlFor="includeBuildScripts">Includi Script di Build</label>
            <FormHint>Generazione di package.json con script npm</FormHint>
          </FormGroup>
          
          {formData.includeBuildScripts && (
            <FormGroup inline>
              <input
                type="checkbox"
                name="useUi5Tooling"
                checked={formData.useUi5Tooling}
                onChange={handleChange}
                id="useUi5Tooling"
              />
              <label htmlFor="useUi5Tooling">Usa UI5 Tooling</label>
              <FormHint>Utilizza l'UI5 Tooling moderno anziché Grunt</FormHint>
            </FormGroup>
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
          <TwoColumnsLayout>
            <FormGroup inline>
              <input
                type="checkbox"
                name="includeDraftHandling"
                checked={formData.includeDraftHandling}
                onChange={handleChange}
                id="includeDraftHandling"
              />
              <label htmlFor="includeDraftHandling">Gestione Bozze</label>
              <FormHint>Salvataggio di bozze dei dati</FormHint>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="addAuthentication"
                checked={formData.addAuthentication}
                onChange={handleChange}
                id="addAuthentication"
              />
              <label htmlFor="addAuthentication">Autenticazione</label>
            </FormGroup>
          </TwoColumnsLayout>
          
          {formData.addAuthentication && (
            <FormGroup label="Metodo di Autenticazione:">
              <select
                name="authenticationMethod"
                value={formData.authenticationMethod}
                onChange={handleChange}
                className="form-select"
              >
                <option value="BASIC">Basic Authentication</option>
                <option value="SAML2">SAML2</option>
                <option value="OAUTH">OAuth</option>
                <option value="X509">X.509 Certificate</option>
              </select>
            </FormGroup>
          )}
          
          <TwoColumnsLayout>
            <FormGroup inline>
              <input
                type="checkbox"
                name="i18nSupport"
                checked={formData.i18nSupport}
                onChange={handleChange}
                id="i18nSupport"
              />
              <label htmlFor="i18nSupport">Internazionalizzazione (i18n)</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="includePersonalization"
                checked={formData.includePersonalization}
                onChange={handleChange}
                id="includePersonalization"
              />
              <label htmlFor="includePersonalization">Personalizzazione UI</label>
              <FormHint>Salvataggio preferenze utente</FormHint>
            </FormGroup>
          </TwoColumnsLayout>
          
          {formData.i18nSupport && (
            <FormGroup label="Lingue Supportate:">
              <ChipContainer>
                {formData.supportedLanguages.map(lang => (
                  <Chip key={lang}>
                    {lang}
                    <Button
                      variant="text"
                      size="small"
                      icon={<FiTrash2 />}
                      onClick={() => handleRemoveLanguage(lang)}
                      className="chip-button"
                    />
                  </Chip>
                ))}
                <Button
                  variant="text"
                  size="small"
                  icon={<FiPlus />}
                  onClick={handleAddLanguage}
                >
                  Aggiungi Lingua
                </Button>
              </ChipContainer>
            </FormGroup>
          )}
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="includeOfflineCapabilities"
              checked={formData.includeOfflineCapabilities}
              onChange={handleChange}
              id="includeOfflineCapabilities"
            />
            <label htmlFor="includeOfflineCapabilities">Funzionalità Offline</label>
            <FormHint>Supporto per utilizzo offline dell'applicazione</FormHint>
          </FormGroup>
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

export default FioriForm;
