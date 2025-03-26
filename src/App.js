import React from 'react';
import styled from 'styled-components';
import { FiSettings, FiStar, FiCode, FiClock } from 'react-icons/fi';

// Componenti di layout
import Header from './components/layout/Header';
import SelectContainer from './components/layout/SelectContainer';

// Componenti comuni
import Select from './components/common/Select';
import Button from './components/common/Button';
import Tabs from './components/common/Tabs';

// Componenti dei form
import IfElseForm from './components/forms/IfElseForm';
import CaseForm from './components/forms/CaseForm';
import LoopAtForm from './components/forms/LoopAtForm';
import SelectForm from './components/forms/SelectForm';
import GenericForm from './components/forms/GenericForm';

// Componenti di anteprima
import CodePreview from './components/preview/CodePreview';

// Componenti template
import TemplatesPanel from './components/templates/TemplatesPanel';

// Context e utility
import { AbapProvider, useAbap } from './context/AbapContext';
import useCodeGenerator from './hooks/useCodeGenerator';
import constructTypes, { getConstructNameById } from './data/constructTypes';

// Componente principale dell'applicazione
const AppContent = () => {
  // Utilizzo del context ABAP
  const { 
    selectedConstructType, 
    setSelectedConstructType, 
    favorites, 
    addToFavorites, 
    removeFromFavorites, 
    generatedCode, 
    setGeneratedCode,
    settings,
    updateSettings,
    activeTab, 
    setActiveTab,
    updateFormState,
    savedTemplates
  } = useAbap();
  
  // Utilizzo del generatore di codice
  const { generateCode } = useCodeGenerator();
  
  // Costruzione delle opzioni del dropdown per i tipi di costrutti
  const constructOptions = constructTypes.map(group => ({
    label: group.group,
    options: group.items
  }));
  
  // Opzioni per i tabs
  const tabOptions = [
    { id: 'standard', label: 'Standard', icon: <FiCode /> },
    { id: 'favorites', label: 'Preferiti', icon: <FiStar /> },
    { id: 'templates', label: 'Template', icon: <FiClock /> },
    { id: 'settings', label: 'Impostazioni', icon: <FiSettings /> }
  ];
  
  // Gestione del cambio di tipo di costrutto
  const handleConstructTypeChange = (e) => {
    setSelectedConstructType(e.target.value);
  };
  
  // Gestione della generazione del codice
  const handleGenerateCode = (constructType, formData) => {
    const code = generateCode(constructType, formData, {
      autoFormat: settings.autoFormat
    });
    
    if (code) {
      setGeneratedCode(code);
    }
  };
  
  // Rendering del form appropriato in base al tipo di costrutto selezionato
  const renderForm = () => {
    switch (selectedConstructType) {
      case 'if-else':
        return <IfElseForm onGenerate={handleGenerateCode} />;
      case 'case':
        return <CaseForm onGenerate={handleGenerateCode} />;
      case 'loop-at':
        return <LoopAtForm onGenerate={handleGenerateCode} />;
      case 'select':
        return <SelectForm onGenerate={handleGenerateCode} />;
      default:
        return <GenericForm constructType={selectedConstructType} onGenerate={handleGenerateCode} />;
    }
  };
  
  // Rendering della tab selezionata
  const renderTabContent = () => {
    switch (activeTab) {
      case 'favorites':
        return (
          <div>
            <h3>I tuoi costrutti preferiti</h3>
            {favorites.length > 0 ? (
              <FavoritesList>
                {favorites.map(fav => (
                  <FavoriteItem key={fav.id} onClick={() => setSelectedConstructType(fav.id)}>
                    {fav.name}
                  </FavoriteItem>
                ))}
              </FavoritesList>
            ) : (
              <p>Nessun costrutto preferito. Aggiungi dei costrutti ai preferiti!</p>
            )}
          </div>
        );
      case 'templates':
        return (
          <div>
            <h3>Template salvati</h3>
            <TemplatesPanel
              onSelectTemplate={(template) => {
                setSelectedConstructType(template.constructType);
                // Qui dovremmo anche caricare i dati del form specifico
                updateFormState(template.constructType, template.formData);
              }}
            />
          </div>
        );
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
                <FavoriteButton
                  variant="outline"
                  size="small"
                  onClick={() => {
                    const selectedConstruct = constructTypes.flatMap(group => group.items)
                      .find(item => item.id === selectedConstructType);
                    if (selectedConstruct) {
                      if (favorites.some(fav => fav.id === selectedConstruct.id)) {
                        removeFromFavorites(selectedConstruct.id);
                      } else {
                        addToFavorites(selectedConstruct);
                      }
                    }
                  }}
                >
                  {favorites.some(fav => fav.id === selectedConstructType) ? 'Rimuovi dai preferiti' : 'Aggiungi ai preferiti'}
                </FavoriteButton>
              </div>
            </SelectContainer>
            <h3>{getConstructNameById(selectedConstructType)}</h3>
            {renderForm()}
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
          <CodePreview code={generatedCode} />
        </OutputPanel>
      </MainContent>
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

const FavoriteButton = styled(Button)`
  white-space: nowrap;
`;

const FavoritesList = styled.div`
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin-top: 15px;
`;

const FavoriteItem = styled.div`
  padding: 10px 15px;
  background: white;
  border-radius: 4px;
  border: 1px solid #ddd;
  cursor: pointer;
  transition: all 0.2s ease;
  
  &:hover {
    background: #f0f4f8;
    border-color: #0066cc;
  }
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

export default App;