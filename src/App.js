import React, { useState } from 'react';
import styled from 'styled-components';
import { FiCode, FiList, FiHelpCircle, FiSettings, FiBriefcase } from 'react-icons/fi';

// Componenti di layout
import Header from './components/layout/Header';
import SelectContainer from './components/layout/SelectContainer';

// Componenti comuni
import Select from './components/common/Select';
import Button from './components/common/Button'; // Corretto il percorso di importazione
import Tabs from './components/common/Tabs';
import ToastManager from './components/common/ToastManager';
import Breadcrumbs from './components/navigation/Breadcrumbs';

// Componenti dei form
import IfElseForm from './components/forms/IfElseForm';
import CaseForm from './components/forms/CaseForm';
import LoopAtForm from './components/forms/LoopAtForm';
import SelectForm from './components/forms/SelectForm';
import GenericForm from './components/forms/GenericForm';
import DoEndDoForm from './components/forms/DoEndDoForm';
import WhileForm from './components/forms/WhileForm';
import UpdateForm from './components/forms/UpdateForm';
import InsertForm from './components/forms/InsertForm';
import ModifyForm from './components/forms/ModifyForm';
import DeleteForm from './components/forms/DeleteForm';
import FormForm from './components/forms/FormForm';
import StructureForm from './components/forms/StructureForm';
import FieldSymbolForm from './components/forms/FieldSymbolForm';
import InternalTableForm from './components/forms/InternalTableForm';
import DataDeclarationForm from './components/forms/DataDeclarationForm';
import TryCatchForm from './components/forms/TryCatchForm';
import RaiseExceptionForm from './components/forms/RaiseExceptionForm';
import MessageForm from './components/forms/MessageForm';
import ClassForm from './components/forms/ClassForm';
import InterfaceForm from './components/forms/InterfaceForm';
import MethodChainForm from './components/forms/MethodChainForm';
import MethodDefinitionForm from './components/forms/MethodDefinitionForm';
import AlvGridForm from './components/forms/AlvGridForm';
import BapiCallForm from './components/forms/BapiCallForm';
import BreakpointAnalyzer from './components/debug/BreakpointAnalyzer';
import SelectionScreenForm from './components/forms/SelectionScreenForm';
import AdvancedAlvForm from './components/forms/AdvancedAlvForm';

// Componenti di anteprima
import CodePreview from './components/preview/CodePreview';

// Componenti template e aiuto
import SearchBar from './components/search/SearchBar';
import GenerationHistory from './components/history/GenerationHistory';
import Documentation from './components/help/Documentation';

// Context e utility
import { AbapProvider, useAbap } from './context/AbapContext';
import useCodeGenerator from './hooks/useCodeGenerator';
import useToast from './hooks/useToast';
import useGenerationHistory from './hooks/useGenerationHistory';
import constructTypes, { getConstructNameById } from './data/constructTypes';

// Componente principale dell'applicazione
const AppContent = () => {
  // Utilizzo del context ABAP
  const { 
    selectedConstructType, 
    setSelectedConstructType,
    generatedCode, 
    setGeneratedCode,
    settings,
    updateSettings,
    activeTab, 
    setActiveTab,
    updateFormState
  } = useAbap();
  
  // State per gestione content display (generatore vs analyzer)
  const [contentDisplay, setContentDisplay] = useState('generator');
  
  // Utilizzo del generatore di codice
  const { generateCode } = useCodeGenerator();
  
  // Utilizzo del sistema di notifiche toast
  const { toasts, removeToast, showSuccess, showError, showInfo } = useToast();
  
  // Utilizzo della cronologia delle generazioni
  const { 
    history, 
    addToHistory, 
    clearHistory,
    searchHistory
  } = useGenerationHistory();
  
  // Costruzione delle opzioni del dropdown per i tipi di costrutti
  const constructOptions = constructTypes.map(group => ({
    label: group.group,
    options: group.items
  }));
  
  // Opzioni per i tabs (uso FiBriefcase invece di FiBug che non esiste)
  const tabOptions = [
    { id: 'standard', label: 'Standard', icon: <FiCode /> },
    { id: 'debug', label: 'Debug', icon: <FiBriefcase /> },
    { id: 'history', label: 'Cronologia', icon: <FiList /> },
    { id: 'help', label: 'Aiuto', icon: <FiHelpCircle /> },
    { id: 'settings', label: 'Impostazioni', icon: <FiSettings /> }
  ];
  
  // Opzioni per content display nel tab standard
  const contentDisplayOptions = [
    { id: 'generator', label: 'Generatore' },
    { id: 'analyzer', label: 'Analyzer' }
  ];
  
  // Resto del codice rimane invariato...
  // ...
  // Gestione del cambio di tipo di costrutto
  const handleConstructTypeChange = (e) => {
    setSelectedConstructType(e.target.value);
  };
  
  // Gestione della generazione del codice
  const handleGenerateCode = (constructType, formData) => {
    try {
      const code = generateCode(constructType, formData, {
        autoFormat: settings.autoFormat
      });
      
      if (code) {
        setGeneratedCode(code);
        
        // Aggiungi alla cronologia
        addToHistory({
          constructType,
          description: getConstructNameById(constructType),
          generatedCode: code,
          formData
        });
        
        showSuccess('Codice generato con successo!');
      }
    } catch (error) {
      showError(`Errore nella generazione del codice: ${error.message}`);
    }
  };
  
  // Funzione per la gestione della cronologia
  const handleSelectFromHistory = (historyItem) => {
    setSelectedConstructType(historyItem.constructType);
    setGeneratedCode(historyItem.generatedCode);
    
    // Se ci sono dati del form, caricali
    if (historyItem.formData) {
      updateFormState(historyItem.constructType, historyItem.formData);
    }
    
    // Torna alla tab standard
    setActiveTab('standard');
    setContentDisplay('generator');
  };
  
  // Funzione helper per ottenere il gruppo di un costrutto
  const getConstructGroupById = (id) => {
    for (const group of constructTypes) {
      if (group.items.some(item => item.id === id)) {
        return group.group;
      }
    }
    return 'Altro';
  };
  
  // Rendering del form appropriato in base al tipo di costrutto selezionato
  const renderForm = () => {
    let FormComponent;
    
    switch (selectedConstructType) {
      case 'if-else':
        FormComponent = IfElseForm;
        break;
      case 'case':
        FormComponent = CaseForm;
        break;
      case 'loop-at':
        FormComponent = LoopAtForm;
        break;
      case 'select':
        FormComponent = SelectForm;
        break;
      case 'do-enddo':
        FormComponent = DoEndDoForm;
        break;
      case 'while':
        FormComponent = WhileForm;
        break;
      case 'update':
        FormComponent = UpdateForm;
        break;
      case 'insert':
        FormComponent = InsertForm;
        break;
      case 'modify':
        FormComponent = ModifyForm;
        break;
      case 'delete':
        FormComponent = DeleteForm;
        break;
      case 'form':
        FormComponent = FormForm;
        break;
      case 'structure':
        FormComponent = StructureForm;
        break;
      case 'field-symbol':
        FormComponent = FieldSymbolForm;
        break;
      case 'internal-table':
        FormComponent = InternalTableForm;
        break;
      case 'data-declaration':
        FormComponent = DataDeclarationForm;
        break;
      case 'try-catch':
        FormComponent = TryCatchForm;
        break;
      case 'raise':
        FormComponent = RaiseExceptionForm;
        break;
      case 'message':
        FormComponent = MessageForm;
        break;
      case 'class':
        FormComponent = ClassForm;
        break;
      case 'interface':
        FormComponent = InterfaceForm;
        break;
      case 'method-chain':
        FormComponent = MethodChainForm;
        break;
      case 'method-definition':
        FormComponent = MethodDefinitionForm;
        break;
      case 'alv-grid':
        FormComponent = AlvGridForm;
        break;
      case 'bapi-call':
        FormComponent = BapiCallForm;
        break;
      default:
        FormComponent = GenericForm;
        break;
      case 'selection-screen':
        FormComponent = SelectionScreenForm;
        break;
      case 'advanced-alv':
        FormComponent = AdvancedAlvForm;
        break;
      }
    
    return (
      <>
        <h3>{getConstructNameById(selectedConstructType)}</h3>
        <FormComponent onGenerate={handleGenerateCode} />
      </>
    );
  };
  
  // Rendering della tab selezionata
  const renderTabContent = () => {
    switch (activeTab) {
      case 'standard':
        return (
          <div>
            <DisplayTabs>
              {contentDisplayOptions.map(option => (
                <DisplayTab 
                  key={option.id}
                  active={contentDisplay === option.id}
                  onClick={() => setContentDisplay(option.id)}
                >
                  {option.label}
                </DisplayTab>
              ))}
            </DisplayTabs>
            
            {contentDisplay === 'generator' ? (
              <>
                <SelectContainer>
                  <label htmlFor="constructType">Seleziona tipo di costrutto:</label>
                  <div className="select-wrapper">
                    <Select
                      id="constructType"
                      value={selectedConstructType}
                      onChange={handleConstructTypeChange}
                      options={constructOptions}
                      placeholder="Seleziona tipo di costrutto"
                    />
                  </div>
                </SelectContainer>
                
                <Breadcrumbs 
                  items={[
                    { label: 'Home' },
                    { label: getConstructGroupById(selectedConstructType) },
                    { label: getConstructNameById(selectedConstructType) }
                  ]}
                />
                
                {renderForm()}
              </>
            ) : (
              <BreakpointAnalyzer />
            )}
          </div>
        );
      case 'debug':
        return (
          <div>
            <h3>Analisi Breakpoint</h3>
            <BreakpointAnalyzer />
          </div>
        );
      case 'history':
        return (
          <div>
            <h3>Cronologia generazioni</h3>
            <GenerationHistory
              history={history}
              onSelect={handleSelectFromHistory}
              onClear={clearHistory}
              oonCopy={(item) => {
                navigator.clipboard.writeText(item.generatedCode);
                showSuccess('Codice copiato negli appunti!');
              }}
            />
          </div>
        );
      case 'help':
        return <Documentation />;
      case 'settings':
        return (
          <SettingsPanel>
            <h3>Impostazioni</h3>
            <SettingItem>
              <input
                type="checkbox"
                id="autoFormat"
                checked={settings.autoFormat}
                onChange={() => updateSettings({ autoFormat: !settings.autoFormat })}
              />
              <label htmlFor="autoFormat">Formattazione automatica</label>
            </SettingItem>
            <SettingItem>
              <input
                type="checkbox"
                id="showLineNumbers"
                checked={settings.showLineNumbers}
                onChange={() => updateSettings({ showLineNumbers: !settings.showLineNumbers })}
              />
              <label htmlFor="showLineNumbers">Mostra numeri di riga</label>
            </SettingItem>
            <SettingItem>
              <input
                type="checkbox"
                id="syntaxHighlighting"
                checked={settings.syntaxHighlighting}
                onChange={() => updateSettings({ syntaxHighlighting: !settings.syntaxHighlighting })}
              />
              <label htmlFor="syntaxHighlighting">Evidenziazione sintassi</label>
            </SettingItem>
            <SettingItem>
              <span>Tema:</span>
              <select
                value={settings.theme}
                onChange={(e) => updateSettings({ theme: e.target.value })}
              >
                <option value="light">Chiaro</option>
                <option value="dark">Scuro</option>
              </select>
            </SettingItem>
          </SettingsPanel>
        );
      default:
        return (
          <div>
            <h3>Seleziona una tab</h3>
            <p>Utilizza le tab in alto per scegliere una funzionalità</p>
          </div>
        );
    }
  };
  
  return (
    <AppContainer>
      <Header />
      
      <MainContent>
        <OptionsPanel>
          <Tabs 
            tabs={tabOptions} 
            activeTab={activeTab} 
            onChange={setActiveTab} 
          />
          {renderTabContent()}
        </OptionsPanel>
        
        <OutputPanel>
          <CodePreview 
            code={generatedCode} 
          />
        </OutputPanel>
      </MainContent>
      
      <ToastManager toasts={toasts} onClose={removeToast} />
    </AppContainer>
  );
};

// Componente App principale che fornisce il context
const App = () => {
  return (
    <AbapProvider>
      <AppContent />
    </AbapProvider>
  );
};

// Stili dell'applicazione
const AppContainer = styled.div`
  width: 100%;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
`;

const MainContent = styled.div`
  display: flex;
  gap: 20px;
  height: calc(100vh - 200px);
  min-height: 500px;
  margin-top: 20px;
  
  @media (max-width: 768px) {
    flex-direction: column;
    height: auto;
  }
`;

const OptionsPanel = styled.div`
  flex: 1;
  background: #f5f5f5;
  border-radius: 8px;
  padding: 20px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  overflow-y: auto;
`;

const OutputPanel = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
`;

const SettingsPanel = styled.div`
  display: flex;
  flex-direction: column;
  gap: 15px;
`;

const SettingItem = styled.div`
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
  
  input[type="checkbox"] {
    width: auto;
    margin-right: 10px;
  }
  
  select {
    padding: 5px 10px;
    border-radius: 4px;
    border: 1px solid #ddd;
  }
`;

const DisplayTabs = styled.div`
  display: flex;
  margin-bottom: 20px;
  border-bottom: 1px solid #ddd;
`;

const DisplayTab = styled.div`
  padding: 8px 16px;
  cursor: pointer;
  border-bottom: 2px solid ${props => props.active ? '#0066cc' : 'transparent'};
  color: ${props => props.active ? '#0066cc' : '#333'};
  font-weight: ${props => props.active ? 'bold' : 'normal'};
  
  &:hover {
    background-color: #f9f9f9;
  }
`;

export default App;