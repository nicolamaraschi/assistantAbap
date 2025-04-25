import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';


// Stili del componente
const FormContainer = styled.div`
  padding: 1rem;
  
  input[type="text"],
  select {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    font-family: 'Courier New', monospace;
    transition: border-color 0.3s, box-shadow 0.3s;
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
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
`;

const LayoutOption = styled.div`
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 10px;
`;

const VariantOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
`;

const EventOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-left: 1rem;
`;

const EventOption = styled.div`
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 10px;
`;

const ButtonContainer = styled.div`
  margin-top: 1rem;
`;

const ColumnSection = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
`;

const TabContainer = styled.div`
  margin-bottom: 1rem;
`;

const TabButton = styled.button`
  padding: 0.5rem 1rem;
  background-color: ${props => props.active ? '#4285f4' : '#f1f1f1'};
  color: ${props => props.active ? 'white' : 'black'};
  border: none;
  cursor: pointer;
  margin-right: 0.5rem;
  border-radius: 4px;

  &:hover {
    background-color: ${props => props.active ? '#4285f4' : '#e0e0e0'};
  }
`;



const AlvGridForm = ({ onGenerate }) => {
  // Aggiunto sistema di tab per organizzare meglio le opzioni
  const [activeTab, setActiveTab] = useState('basic');
  
  // Stato locale del form ampliato con nuove opzioni
  const [formData, setFormData] = useState({
    tableName: 'lt_data',
    fieldCatalog: 'SFLIGHT',
    alvType: 'grid', // grid o list o oo_alv (object oriented)
    layout: {
      zebra: true,
      cwidth_opt: true,
      sel_mode: 'A',
      grid_title: 'ALV Grid Title',
      no_toolbar: false,
      no_headers: false,
      no_hgridln: false,
      no_vgridln: false,
      smalltitle: false,
      col_opt: true,
      totals_only: false,
      web_display: false,
      cell_merge: false
    },
    variant: {
      report: 'ZPROGRAM',
      handle: 'DEFAULT',
      username: 'SY-UNAME',
      save: 'A', // A = Tutte le varianti, U = Solo utente, X = Nessuna variante
      defaultVariant: '',
      transportable: false
    },
    events: {
      addEvents: false,
      topOfPage: false,
      endOfPage: false,
      userCommand: false,
      hotspot: false,
      doubleClick: false,
      dataChanged: false,
      toolbar: false,
      beforeUserCommand: false,
      afterUserCommand: false
    },
    sorting: {
      addSorting: false,
      fields: [
        { field: '', sequence: 'ASCENDING', subtotal: false }
      ]
    },
    filtering: {
      addFiltering: false,
      fields: [
        { field: '', operator: 'EQ', low: '', high: '' }
      ]
    },
    fieldCatalogCustom: {
      useCustom: false,
      fields: [
        { 
          fieldname: '', 
          seltext: '', 
          outputlen: '10',
          key: false,
          hotspot: false,
          emphasize: '',
          checkbox: false,
          icon: false,
          roundfield: false,
          noOut: false,
          doSum: false,
          fixedCol: false
        }
      ]
    },
    extendedOptions: {
      addTopOfPageArea: false,
      topOfPageHeight: '5',
      addEndOfPageArea: false,
      endOfPageHeight: '5',
      addUserButtons: false,
      userButtons: [
        { function: 'MYBTN', icon: 'ICON_DISPLAY', text: 'Custom Button', tooltip: 'Custom function' }
      ],
      callbackProgram: '',
      hideStandardFunctions: [],
      optimizeColumns: true
    }
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
  
  // Gestisce il cambiamento dei campi di base
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
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      variant: {
        ...formData.variant,
        [name]: type === 'checkbox' ? checked : value
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
  
  // Gestisce il cambiamento delle opzioni estese
  const handleExtendedOptionsChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      extendedOptions: {
        ...formData.extendedOptions,
        [name]: type === 'checkbox' ? checked : value
      }
    });
  };
  
  // Gestisce l'aggiunta di un campo di ordinamento
  const handleAddSortField = () => {
    setFormData({
      ...formData,
      sorting: {
        ...formData.sorting,
        fields: [
          ...formData.sorting.fields,
          { field: '', sequence: 'ASCENDING', subtotal: false }
        ]
      }
    });
  };
  
  // Gestisce la rimozione di un campo di ordinamento
  const handleRemoveSortField = (index) => {
    const newFields = [...formData.sorting.fields];
    newFields.splice(index, 1);
    setFormData({
      ...formData,
      sorting: {
        ...formData.sorting,
        fields: newFields
      }
    });
  };
  
  // Gestisce il cambiamento di un campo di ordinamento
  const handleSortFieldChange = (index, field, value) => {
    const newFields = [...formData.sorting.fields];
    newFields[index] = {
      ...newFields[index],
      [field]: field === 'subtotal' ? value === true : value
    };
    setFormData({
      ...formData,
      sorting: {
        ...formData.sorting,
        fields: newFields
      }
    });
  };
  
  // Gestisce l'aggiunta di un campo di filtro
  const handleAddFilterField = () => {
    setFormData({
      ...formData,
      filtering: {
        ...formData.filtering,
        fields: [
          ...formData.filtering.fields,
          { field: '', operator: 'EQ', low: '', high: '' }
        ]
      }
    });
  };
  
  // Gestisce la rimozione di un campo di filtro
  const handleRemoveFilterField = (index) => {
    const newFields = [...formData.filtering.fields];
    newFields.splice(index, 1);
    setFormData({
      ...formData,
      filtering: {
        ...formData.filtering,
        fields: newFields
      }
    });
  };
  
  // Gestisce il cambiamento di un campo di filtro
  const handleFilterFieldChange = (index, field, value) => {
    const newFields = [...formData.filtering.fields];
    newFields[index] = {
      ...newFields[index],
      [field]: value
    };
    setFormData({
      ...formData,
      filtering: {
        ...formData.filtering,
        fields: newFields
      }
    });
  };
  
  // Gestisce l'aggiunta di un campo del field catalog personalizzato
  const handleAddFieldCatalogField = () => {
    setFormData({
      ...formData,
      fieldCatalogCustom: {
        ...formData.fieldCatalogCustom,
        fields: [
          ...formData.fieldCatalogCustom.fields,
          { 
            fieldname: '', 
            seltext: '', 
            outputlen: '10', 
            key: false,
            hotspot: false,
            emphasize: '',
            checkbox: false,
            icon: false,
            roundfield: false,
            noOut: false,
            doSum: false,
            fixedCol: false
          }
        ]
      }
    });
  };
  
  // Gestisce la rimozione di un campo del field catalog
  const handleRemoveFieldCatalogField = (index) => {
    const newFields = [...formData.fieldCatalogCustom.fields];
    newFields.splice(index, 1);
    setFormData({
      ...formData,
      fieldCatalogCustom: {
        ...formData.fieldCatalogCustom,
        fields: newFields
      }
    });
  };
  
  // Gestisce il cambiamento di un campo del field catalog
  const handleFieldCatalogChange = (index, field, value) => {
    const newFields = [...formData.fieldCatalogCustom.fields];
    newFields[index] = {
      ...newFields[index],
      [field]: typeof value === 'boolean' ? value : value
    };
    setFormData({
      ...formData,
      fieldCatalogCustom: {
        ...formData.fieldCatalogCustom,
        fields: newFields
      }
    });
  };
  
  // Gestisce l'aggiunta di un pulsante personalizzato
  const handleAddUserButton = () => {
    setFormData({
      ...formData,
      extendedOptions: {
        ...formData.extendedOptions,
        userButtons: [
          ...formData.extendedOptions.userButtons,
          { function: '', icon: '', text: '', tooltip: '' }
        ]
      }
    });
  };
  
  // Gestisce la rimozione di un pulsante personalizzato
  const handleRemoveUserButton = (index) => {
    const newButtons = [...formData.extendedOptions.userButtons];
    newButtons.splice(index, 1);
    setFormData({
      ...formData,
      extendedOptions: {
        ...formData.extendedOptions,
        userButtons: newButtons
      }
    });
  };
  
  // Gestisce il cambiamento di un pulsante personalizzato
  const handleUserButtonChange = (index, field, value) => {
    const newButtons = [...formData.extendedOptions.userButtons];
    newButtons[index] = {
      ...newButtons[index],
      [field]: value
    };
    setFormData({
      ...formData,
      extendedOptions: {
        ...formData.extendedOptions,
        userButtons: newButtons
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
      {/* Tabs per navigare tra le sezioni */}
      <TabContainer>
        <TabButton 
          active={activeTab === 'basic'} 
          onClick={() => setActiveTab('basic')}
        >
          Base
        </TabButton>
        <TabButton 
          active={activeTab === 'layout'} 
          onClick={() => setActiveTab('layout')}
        >
          Layout
        </TabButton>
        <TabButton 
          active={activeTab === 'variant'} 
          onClick={() => setActiveTab('variant')}
        >
          Varianti
        </TabButton>
        <TabButton 
          active={activeTab === 'events'} 
          onClick={() => setActiveTab('events')}
        >
          Eventi
        </TabButton>
        <TabButton 
          active={activeTab === 'sorting'} 
          onClick={() => setActiveTab('sorting')}
        >
          Ordinamento
        </TabButton>
        <TabButton 
          active={activeTab === 'filtering'} 
          onClick={() => setActiveTab('filtering')}
        >
          Filtri
        </TabButton>
        <TabButton 
          active={activeTab === 'fieldcatalog'} 
          onClick={() => setActiveTab('fieldcatalog')}
        >
          Field Catalog
        </TabButton>
        <TabButton 
          active={activeTab === 'extended'} 
          onClick={() => setActiveTab('extended')}
        >
          Opzioni avanzate
        </TabButton>
      </TabContainer>
      
      {/* Tab di base */}
      {activeTab === 'basic' && (
        <>
          <FormGroup label="Tipo di ALV:">
            <select
              name="alvType"
              value={formData.alvType}
              onChange={handleChange}
            >
              <option value="grid">ALV Grid (Function Module)</option>
              <option value="list">ALV List (Function Module)</option>
              <option value="oo_alv">Object Oriented ALV (CL_SALV_TABLE)</option>
            </select>
          </FormGroup>
          
          <FormGroup label="Nome tabella interna:">
            <ControlledInput 
              type="text"
              name="tableName"
              value={formData.tableName}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="Struttura per field catalog:">
            <ControlledInput 
              type="text"
              name="fieldCatalog"
              value={formData.fieldCatalog}
              onChange={handleChange}
              placeholder="es. SFLIGHT o una struttura personalizzata"
            />
          </FormGroup>
        </>
      )}
      
      {/* Tab di layout */}
      {activeTab === 'layout' && (
        <>
          <FormGroup label="Opzioni di layout:">
            <LayoutOptions>
              <ColumnSection>
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="zebra"
                    checked={formData.layout.zebra}
                    onChange={handleLayoutChange}
                    id="zebraStripes"
                  />
                  <label htmlFor="zebraStripes">Righe zebrate</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="cwidth_opt"
                    checked={formData.layout.cwidth_opt}
                    onChange={handleLayoutChange}
                    id="optimizeWidth"
                  />
                  <label htmlFor="optimizeWidth">Ottimizza larghezze colonne</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="no_toolbar"
                    checked={formData.layout.no_toolbar}
                    onChange={handleLayoutChange}
                    id="noToolbar"
                  />
                  <label htmlFor="noToolbar">Nascondi toolbar</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="no_headers"
                    checked={formData.layout.no_headers}
                    onChange={handleLayoutChange}
                    id="noHeaders"
                  />
                  <label htmlFor="noHeaders">Nascondi intestazioni</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="no_hgridln"
                    checked={formData.layout.no_hgridln}
                    onChange={handleLayoutChange}
                    id="noHorizontalLines"
                  />
                  <label htmlFor="noHorizontalLines">Nascondi linee orizzontali</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="no_vgridln"
                    checked={formData.layout.no_vgridln}
                    onChange={handleLayoutChange}
                    id="noVerticalLines"
                  />
                  <label htmlFor="noVerticalLines">Nascondi linee verticali</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="smalltitle"
                    checked={formData.layout.smalltitle}
                    onChange={handleLayoutChange}
                    id="smallTitle"
                  />
                  <label htmlFor="smallTitle">Titolo piccolo</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="col_opt"
                    checked={formData.layout.col_opt}
                    onChange={handleLayoutChange}
                    id="colOpt"
                  />
                  <label htmlFor="colOpt">Ottimizzazione colonne</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="totals_only"
                    checked={formData.layout.totals_only}
                    onChange={handleLayoutChange}
                    id="totalsOnly"
                  />
                  <label htmlFor="totalsOnly">Solo totali</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="web_display"
                    checked={formData.layout.web_display}
                    onChange={handleLayoutChange}
                    id="webDisplay"
                  />
                  <label htmlFor="webDisplay">Visualizzazione web</label>
                </LayoutOption>
                
                <LayoutOption>
                  <ControlledInput 
                    type="checkbox"
                    name="cell_merge"
                    checked={formData.layout.cell_merge}
                    onChange={handleLayoutChange}
                    id="cellMerge"
                  />
                  <label htmlFor="cellMerge">Unione celle</label>
                </LayoutOption>
              </ColumnSection>
              
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
                  <option value="SPACE">Nessuna selezione</option>
                </select>
              </FormGroup>
              
              <FormGroup label="Titolo grid:">
                <ControlledInput 
                  type="text"
                  name="grid_title"
                  value={formData.layout.grid_title}
                  onChange={handleLayoutChange}
                  placeholder="Inserisci titolo della grid"
                />
              </FormGroup>
            </LayoutOptions>
          </FormGroup>
        </>
      )}
      
      {/* Tab varianti */}
      {activeTab === 'variant' && (
        <>
          <FormGroup label="Gestione varianti:">
            <VariantOptions>
              <FormGroup label="Salvataggio varianti:">
                <select
                  name="save"
                  value={formData.variant.save}
                  onChange={handleVariantChange}
                >
                  <option value="A">A - Tutte le varianti (utente e globali)</option>
                  <option value="U">U - Solo varianti utente</option>
                  <option value="X">X - Nessun salvataggio varianti</option>
                  <option value="S">S - Solo varianti standard</option>
                </select>
              </FormGroup>
              
              <FormGroup label="Report:">
                <ControlledInput 
                  type="text"
                  name="report"
                  value={formData.variant.report}
                  onChange={handleVariantChange}
                  placeholder="es. ZPROGRAM o SY-REPID"
                />
              </FormGroup>
              
              <FormGroup label="Handle:">
                <ControlledInput 
                  type="text"
                  name="handle"
                  value={formData.variant.handle}
                  onChange={handleVariantChange}
                  placeholder="es. DEFAULT"
                />
              </FormGroup>
              
              <FormGroup label="Username:">
                <ControlledInput 
                  type="text"
                  name="username"
                  value={formData.variant.username}
                  onChange={handleVariantChange}
                  placeholder="es. SY-UNAME o spazio vuoto"
                />
              </FormGroup>
              
              <FormGroup label="Variante predefinita:">
                <ControlledInput 
                  type="text"
                  name="defaultVariant"
                  value={formData.variant.defaultVariant}
                  onChange={handleVariantChange}
                  placeholder="Nome variante predefinita"
                />
              </FormGroup>
              
              <LayoutOption>
                <ControlledInput 
                  type="checkbox"
                  name="transportable"
                  checked={formData.variant.transportable}
                  onChange={handleVariantChange}
                  id="transportable"
                />
                <label htmlFor="transportable">Variante trasportabile</label>
              </LayoutOption>
            </VariantOptions>
          </FormGroup>
        </>
      )}
      
      {/* Tab eventi */}
      {activeTab === 'events' && (
        <>
          <FormGroup label="Eventi:">
            <EventOptions>
              <EventOption>
                <ControlledInput 
                  type="checkbox"
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
                    <ControlledInput 
                      type="checkbox"
                      name="topOfPage"
                      checked={formData.events.topOfPage}
                      onChange={handleEventChange}
                      id="topOfPage"
                    />
                    <label htmlFor="topOfPage">TOP_OF_PAGE</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="endOfPage"
                      checked={formData.events.endOfPage}
                      onChange={handleEventChange}
                      id="endOfPage"
                    />
                    <label htmlFor="endOfPage">END_OF_PAGE</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="userCommand"
                      checked={formData.events.userCommand}
                      onChange={handleEventChange}
                      id="userCommand"
                    />
                    <label htmlFor="userCommand">USER_COMMAND</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="hotspot"
                      checked={formData.events.hotspot}
                      onChange={handleEventChange}
                      id="hotspot"
                    />
                    <label htmlFor="hotspot">HOTSPOT_CLICK</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="doubleClick"
                      checked={formData.events.doubleClick}
                      onChange={handleEventChange}
                      id="doubleClick"
                    />
                    <label htmlFor="doubleClick">DOUBLE_CLICK</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="dataChanged"
                      checked={formData.events.dataChanged}
                      onChange={handleEventChange}
                      id="dataChanged"
                    />
                    <label htmlFor="dataChanged">DATA_CHANGED</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="toolbar"
                      checked={formData.events.toolbar}
                      onChange={handleEventChange}
                      id="toolbar"
                    />
                    <label htmlFor="toolbar">TOOLBAR</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="beforeUserCommand"
                      checked={formData.events.beforeUserCommand}
                      onChange={handleEventChange}
                      id="beforeUserCommand"
                    />
                    <label htmlFor="beforeUserCommand">BEFORE_USER_COMMAND</label>
                  </EventOption>
                  
                  <EventOption>
                    <ControlledInput 
                      type="checkbox"
                      name="afterUserCommand"
                      checked={formData.events.afterUserCommand}
                      onChange={handleEventChange}
                      id="afterUserCommand"
                    />
                    <label htmlFor="afterUserCommand">AFTER_USER_COMMAND</label>
                  </EventOption>
                </>
              )}
            </EventOptions>
          </FormGroup>
        </>
      )}
      
      {/* Tab ordinamento */}
      {activeTab === 'sorting' && (
        <>
          <FormGroup inline>
            <ControlledInput 
              type="checkbox"
              name="addSorting"
              checked={formData.sorting.addSorting}
              onChange={(e) => setFormData({...formData, sorting: {...formData.sorting, addSorting: e.target.checked}})}
              id="addSorting"
            />
            <label htmlFor="addSorting">Aggiungi ordinamento personalizzato</label>
          </FormGroup>
          
          {formData.sorting.addSorting && (
            <>
              {formData.sorting.fields.map((field, index) => (
                <div key={index} style={{ marginBottom: '1rem', padding: '0.5rem', border: '1px solid #ddd', borderRadius: '4px' }}>
                  <FormGroup label={`Campo di ordinamento ${index + 1}:`}>
                    <ControlledInput 
                      type="text"
                      value={field.field}
                      onChange={(e) => handleSortFieldChange(index, 'field', e.target.value)}
                      placeholder="Nome campo"
                    />
                  </FormGroup>
                  
                  <FormGroup label="Sequenza:">
                    <select
                      value={field.sequence}
                      onChange={(e) => handleSortFieldChange(index, 'sequence', e.target.value)}
                    >
                      <option value="ASCENDING">Ascendente</option>
                      <option value="DESCENDING">Discendente</option>
                    </select>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <ControlledInput 
                      type="checkbox"
                      checked={field.subtotal}
                      onChange={(e) => handleSortFieldChange(index, 'subtotal', e.target.checked)}
                      id={`subtotal-${index}`}
                    />
                    <label htmlFor={`subtotal-${index}`}>Calcola subtotali</label>
                  </FormGroup>
                  
                  <Button 
                    variant="danger" 
                    onClick={() => handleRemoveSortField(index)}
                    size="small"
                  >
                    Rimuovi campo
                  </Button>
                </div>
              ))}
              
              <Button 
                variant="secondary" 
                onClick={handleAddSortField}
                size="small"
              >
                Aggiungi campo di ordinamento
              </Button>
            </>
          )}
        </>
      )}
      
      {/* Tab filtri */}
      {activeTab === 'filtering' && (
        <>
          <FormGroup inline>
            <ControlledInput 
              type="checkbox"
              name="addFiltering"
              checked={formData.filtering.addFiltering}
              onChange={(e) => setFormData({...formData, filtering: {...formData.filtering, addFiltering: e.target.checked}})}
              id="addFiltering"
            />
            <label htmlFor="addFiltering">Aggiungi filtri personalizzati</label>
          </FormGroup>
          
          {formData.filtering.addFiltering && (
            <>
              {formData.filtering.fields.map((field, index) => (
                <div key={index} style={{ marginBottom: '1rem', padding: '0.5rem', border: '1px solid #ddd', borderRadius: '4px' }}>
                  <FormGroup label={`Campo di filtro ${index + 1}:`}>
                    <ControlledInput 
                      type="text"
                      value={field.field}
                      onChange={(e) => handleFilterFieldChange(index, 'field', e.target.value)}
                      placeholder="Nome campo"
                    />
                  </FormGroup>
                  
                  <FormGroup label="Operatore:">
                    <select
                      value={field.operator}
                      onChange={(e) => handleFilterFieldChange(index, 'operator', e.target.value)}
                    >
                      <option value="EQ">Uguale a (EQ)</option>
                      <option value="NE">Diverso da (NE)</option>
                      <option value="LT">Minore di (LT)</option>
                      <option value="LE">Minore o uguale (LE)</option>
                      <option value="GT">Maggiore di (GT)</option>
                      <option value="GE">Maggiore o uguale (GE)</option>
                      <option value="BT">Compreso tra (BT)</option>
                      <option value="CP">Contiene pattern (CP)</option>
                      <option value="NP">Non contiene pattern (NP)</option>
                    </select>
                  </FormGroup>
                  
                  <FormGroup label="Valore basso:">
                    <ControlledInput 
                      type="text"
                      value={field.low}
                      onChange={(e) => handleFilterFieldChange(index, 'low', e.target.value)}
                      placeholder="Valore basso o singolo"
                    />
                  </FormGroup>
                  
                  {field.operator === 'BT' && (
                    <FormGroup label="Valore alto:">
                      <ControlledInput 
                        type="text"
                        value={field.high}
                        onChange={(e) => handleFilterFieldChange(index, 'high', e.target.value)}
                        placeholder="Valore alto"
                      />
                    </FormGroup>
                  )}
                  
                  <Button 
                    variant="danger" 
                    onClick={() => handleRemoveFilterField(index)}
                    size="small"
                  >
                    Rimuovi filtro
                  </Button>
                </div>
              ))}
              
              <Button 
                variant="secondary" 
                onClick={handleAddFilterField}
                size="small"
              >
                Aggiungi campo di filtro
              </Button>
            </>
          )}
        </>
      )}
      
      {/* Tab Field Catalog */}
      {activeTab === 'fieldcatalog' && (
        <>
          <FormGroup inline>
            <ControlledInput 
              type="checkbox"
              name="useCustom"
              checked={formData.fieldCatalogCustom.useCustom}
              onChange={(e) => setFormData({
                ...formData, 
                fieldCatalogCustom: {
                  ...formData.fieldCatalogCustom, 
                  useCustom: e.target.checked
                }
              })}
              id="useCustomFieldCatalog"
            />
            <label htmlFor="useCustomFieldCatalog">Personalizza field catalog</label>
          </FormGroup>
          
          {formData.fieldCatalogCustom.useCustom && (
            <>
              {formData.fieldCatalogCustom.fields.map((field, index) => (
                <div key={index} style={{ marginBottom: '1rem', padding: '0.5rem', border: '1px solid #ddd', borderRadius: '4px' }}>
                  <FormGroup label={`Campo ${index + 1}:`}>
                    <ControlledInput 
                      type="text"
                      value={field.fieldname}
                      onChange={(e) => handleFieldCatalogChange(index, 'fieldname', e.target.value)}
                      placeholder="Nome campo"
                    />
                  </FormGroup>
                  
                  <FormGroup label="Intestazione colonna:">
                    <ControlledInput 
                      type="text"
                      value={field.seltext}
                      onChange={(e) => handleFieldCatalogChange(index, 'seltext', e.target.value)}
                      placeholder="Testo visualizzato"
                    />
                  </FormGroup>
                  
                  <FormGroup label="Lunghezza output:">
                    <ControlledInput 
                      type="text"
                      value={field.outputlen}
                      onChange={(e) => handleFieldCatalogChange(index, 'outputlen', e.target.value)}
                      placeholder="Lunghezza in caratteri"
                    />
                  </FormGroup>
                  
                  <FormGroup label="Evidenziazione:">
                    <select
                      value={field.emphasize}
                      onChange={(e) => handleFieldCatalogChange(index, 'emphasize', e.target.value)}
                    >
                      <option value="">Nessuna</option>
                      <option value="C100">Colore C100 (verde chiaro)</option>
                      <option value="C200">Colore C200 (giallo)</option>
                      <option value="C300">Colore C300 (rosso)</option>
                      <option value="C400">Colore C400 (blu)</option>
                      <option value="C500">Colore C500 (rosa)</option>
                      <option value="C600">Colore C600 (verde scuro)</option>
                      <option value="C700">Colore C700 (arancione)</option>
                    </select>
                  </FormGroup>
                  
                  <ColumnSection>
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.key}
                        onChange={(e) => handleFieldCatalogChange(index, 'key', e.target.checked)}
                        id={`key-${index}`}
                      />
                      <label htmlFor={`key-${index}`}>Campo chiave</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.hotspot}
                        onChange={(e) => handleFieldCatalogChange(index, 'hotspot', e.target.checked)}
                        id={`hotspot-${index}`}
                      />
                      <label htmlFor={`hotspot-${index}`}>Hotspot</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.checkbox}
                        onChange={(e) => handleFieldCatalogChange(index, 'checkbox', e.target.checked)}
                        id={`checkbox-${index}`}
                      />
                      <label htmlFor={`checkbox-${index}`}>Checkbox</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.icon}
                        onChange={(e) => handleFieldCatalogChange(index, 'icon', e.target.checked)}
                        id={`icon-${index}`}
                      />
                      <label htmlFor={`icon-${index}`}>Icona</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.roundfield}
                        onChange={(e) => handleFieldCatalogChange(index, 'roundfield', e.target.checked)}
                        id={`roundfield-${index}`}
                      />
                      <label htmlFor={`roundfield-${index}`}>Arrotonda decimali</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.noOut}
                        onChange={(e) => handleFieldCatalogChange(index, 'noOut', e.target.checked)}
                        id={`noOut-${index}`}
                      />
                      <label htmlFor={`noOut-${index}`}>Nascondi colonna</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.doSum}
                        onChange={(e) => handleFieldCatalogChange(index, 'doSum', e.target.checked)}
                        id={`doSum-${index}`}
                      />
                      <label htmlFor={`doSum-${index}`}>Calcola somma</label>
                    </FormGroup>
                    
                    <FormGroup inline>
                      <ControlledInput 
                        type="checkbox"
                        checked={field.fixedCol}
                        onChange={(e) => handleFieldCatalogChange(index, 'fixedCol', e.target.checked)}
                        id={`fixedCol-${index}`}
                      />
                      <label htmlFor={`fixedCol-${index}`}>Fissa colonna</label>
                    </FormGroup>
                  </ColumnSection>
                  
                  <Button 
                    variant="danger" 
                    onClick={() => handleRemoveFieldCatalogField(index)}
                    size="small"
                  >
                    Rimuovi campo
                  </Button>
                </div>
              ))}
              
              <Button 
                variant="secondary" 
                onClick={handleAddFieldCatalogField}
                size="small"
              >
                Aggiungi campo al field catalog
              </Button>
            </>
          )}
        </>
      )}
      
      {/* Tab opzioni avanzate */}
      {activeTab === 'extended' && (
        <>
          <FormGroup label="Opzioni avanzate:">
            <LayoutOptions>
              <FormGroup inline>
                <ControlledInput 
                  type="checkbox"
                  name="addTopOfPageArea"
                  checked={formData.extendedOptions.addTopOfPageArea}
                  onChange={handleExtendedOptionsChange}
                  id="addTopOfPageArea"
                />
                <label htmlFor="addTopOfPageArea">Aggiungi area TOP_OF_PAGE</label>
              </FormGroup>
              
              {formData.extendedOptions.addTopOfPageArea && (
                <FormGroup label="Altezza TOP_OF_PAGE (linee):">
                  <ControlledInput 
                    type="text"
                    name="topOfPageHeight"
                    value={formData.extendedOptions.topOfPageHeight}
                    onChange={handleExtendedOptionsChange}
                    placeholder="Numero di linee"
                  />
                </FormGroup>
              )}
              
              <FormGroup inline>
                <ControlledInput 
                  type="checkbox"
                  name="addEndOfPageArea"
                  checked={formData.extendedOptions.addEndOfPageArea}
                  onChange={handleExtendedOptionsChange}
                  id="addEndOfPageArea"
                />
                <label htmlFor="addEndOfPageArea">Aggiungi area END_OF_PAGE</label>
              </FormGroup>
              
              {formData.extendedOptions.addEndOfPageArea && (
                <FormGroup label="Altezza END_OF_PAGE (linee):">
                  <ControlledInput 
                    type="text"
                    name="endOfPageHeight"
                    value={formData.extendedOptions.endOfPageHeight}
                    onChange={handleExtendedOptionsChange}
                    placeholder="Numero di linee"
                  />
                </FormGroup>
              )}
              
              <FormGroup inline>
                <ControlledInput 
                  type="checkbox"
                  name="addUserButtons"
                  checked={formData.extendedOptions.addUserButtons}
                  onChange={handleExtendedOptionsChange}
                  id="addUserButtons"
                />
                <label htmlFor="addUserButtons">Aggiungi pulsanti personalizzati</label>
              </FormGroup>
              
              {formData.extendedOptions.addUserButtons && (
                <>
                  {formData.extendedOptions.userButtons.map((button, index) => (
                    <div key={index} style={{ marginBottom: '1rem', padding: '0.5rem', border: '1px solid #ddd', borderRadius: '4px' }}>
                      <FormGroup label={`Pulsante ${index + 1}:`}>
                        <FormGroup label="Funzione:">
                          <ControlledInput 
                            type="text"
                            value={button.function}
                            onChange={(e) => handleUserButtonChange(index, 'function', e.target.value)}
                            placeholder="es. ZFUNC"
                          />
                        </FormGroup>
                        
                        <FormGroup label="Testo pulsante:">
                          <ControlledInput 
                            type="text"
                            value={button.text}
                            onChange={(e) => handleUserButtonChange(index, 'text', e.target.value)}
                            placeholder="Testo visualizzato"
                          />
                        </FormGroup>
                        
                        <FormGroup label="Icona:">
                          <ControlledInput 
                            type="text"
                            value={button.icon}
                            onChange={(e) => handleUserButtonChange(index, 'icon', e.target.value)}
                            placeholder="es. ICON_DISPLAY"
                          />
                        </FormGroup>
                        
                        <FormGroup label="Tooltip:">
                          <ControlledInput 
                            type="text"
                            value={button.tooltip}
                            onChange={(e) => handleUserButtonChange(index, 'tooltip', e.target.value)}
                            placeholder="Testo suggerimento"
                          />
                        </FormGroup>
                      </FormGroup>
                      
                      <Button 
                        variant="danger" 
                        onClick={() => handleRemoveUserButton(index)}
                        size="small"
                      >
                        Rimuovi pulsante
                      </Button>
                    </div>
                  ))}
                  
                  <Button 
                    variant="secondary" 
                    onClick={handleAddUserButton}
                    size="small"
                  >
                    Aggiungi pulsante
                  </Button>
                </>
              )}
              
              <FormGroup label="Callback program:">
                <ControlledInput 
                  type="text"
                  name="callbackProgram"
                  value={formData.extendedOptions.callbackProgram}
                  onChange={handleExtendedOptionsChange}
                  placeholder="Nome programma o SY-REPID"
                />
              </FormGroup>
              
              <FormGroup inline>
                <ControlledInput 
                  type="checkbox"
                  name="optimizeColumns"
                  checked={formData.extendedOptions.optimizeColumns}
                  onChange={handleExtendedOptionsChange}
                  id="optimizeColumns"
                />
                <label htmlFor="optimizeColumns">Ottimizza automaticamente colonne</label>
              </FormGroup>
            </LayoutOptions>
          </FormGroup>
        </>
      )}
      
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

export default AlvGridForm;


