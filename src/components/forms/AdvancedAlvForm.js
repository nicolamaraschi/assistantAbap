import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiArrowUp, FiArrowDown, FiEdit, FiChevronDown, FiChevronUp, FiEye } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const AdvancedAlvForm = ({ onGenerate }) => {
  // Stato locale del form con tutte le opzioni per ALV avanzato
  const [formData, setFormData] = useState({
    // Informazioni base ALV
    alvType: 'GRID',               // GRID, TREE, HIERSEQ
    tableName: 'gt_data',
    structName: 'gty_data',
    fieldCatalog: [
      { 
        id: 1, 
        fieldname: 'MATNR', 
        seltext: 'Materiale', 
        datatype: 'CHAR', 
        outputlen: '18',
        hotspot: false,
        checkbox: false,
        editable: false,
        key: false,
        emphasize: false,
        noOut: false,
        tech: false,
        coltext: '',
        tipText: ''
      },
      {
        id: 2, 
        fieldname: 'MAKTX', 
        seltext: 'Descrizione', 
        datatype: 'CHAR', 
        outputlen: '40',
        hotspot: false,
        checkbox: false,
        editable: false,
        key: false,
        emphasize: false,
        noOut: false,
        tech: false,
        coltext: '',
        tipText: ''
      }
    ],
    
    // Layout
    layout: {
      zebra: true,
      colwdOpt: true,
      gridTitle: 'ALV Grid Report',
      selMode: 'A',           // A, B, C, D
      cellEdit: false,
      noHeaders: false,
      noMerging: false,
      sglClick: false,
      noRowMark: false,
      smallTitle: false,
      noRowIns: false,
      noRowResize: false,
      styleName: '',
      excelDisp: false,
      webDisp: false
    },
    
    // Eventi
    events: {
      topOfPage: false,
      endOfPage: false,
      userCommand: false,
      hotspotClick: false,
      doubleClick: false,
      dataChanged: false,
      toolbar: false,
      beforeUserCommand: false,
      afterUserCommand: false,
      buttonClick: false,
      menuButton: false,
      link: false,
      print: false
    },
    
    // Sorting
    sortingEnabled: false,
    sorting: [
      { id: 1, fieldname: 'MATNR', sequence: 'ASCENDING' }
    ],
    
    // Filtering
    filteringEnabled: false,
    filtering: [
      { id: 1, fieldname: 'MATNR', sign: 'I', option: 'EQ', low: '', high: '' }
    ],
    
    // Subtotals
    subtotalsEnabled: false,
    subtotals: [
      { id: 1, fieldname: 'MATNR' }
    ],
    
    // Toolbar / Menu Buttons
    toolbarEnabled: false,
    toolbarButtons: [
      { id: 1, function: 'DETAIL', icon: '@15@', text: 'Dettaglio', quickinfo: 'Visualizza dettaglio' }
    ],
    
    // Editable Grid
    editModeEnabled: false,
    registerEnterPressed: false,
    checkChanges: false,
    displayChanges: false,
    
    // Salvataggio varianti
    variantEnabled: true,
    variantHandle: 'DEFAULT',
    variantReport: 'SY-REPID',
    variantUsername: 'SY-UNAME',
    
    // Opzioni OO (metodo/classe)
    isOO: false,
    className: 'LCL_ALV_HANDLER',
    eventMethod: 'HANDLE_USER_COMMAND',
    
    // Output 
    containerName: '',
    popupEnabled: false,
    popupStartCol: '5',
    popupEndCol: '100',
    popupStartLine: '5',
    popupEndLine: '20',
    
    // Builder pattern
    useBuilder: false, // Usa OO Builder pattern anziché FM
    
    // Tipo di output (GRID, GRID_DISPLAY, HIERSEQ_DISPLAY...)
    outputType: 'GRID'
  });
  
  // Stato per le sezioni collassabili
  const [expandedSections, setExpandedSections] = useState({
    basic: true,
    fieldCatalog: true,
    layout: false,
    events: false,
    sorting: false,
    filtering: false,
    toolbar: false,
    variant: false,
    extras: false,
    preview: false
  });
  
  // ID per nuovi elementi
  const [idCounter, setIdCounter] = useState({
    fieldCatalog: 3,
    sorting: 2,
    filtering: 2,
    subtotals: 2,
    toolbarButtons: 2
  });
  
  // Per il campo attivo in fieldCatalog quando si edita
  const [editingField, setEditingField] = useState(null);
  
  // Anteprima del codice generato
  const [previewCode, setPreviewCode] = useState('');
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['advanced-alv']) {
      setFormData(formState['advanced-alv']);
      
      // Aggiorna anche i contatori ID
      const maxFieldCatId = Math.max(0, ...formState['advanced-alv'].fieldCatalog.map(f => f.id));
      const maxSortingId = Math.max(0, ...formState['advanced-alv'].sorting.map(s => s.id));
      const maxFilteringId = Math.max(0, ...formState['advanced-alv'].filtering.map(f => f.id));
      const maxSubtotalsId = formState['advanced-alv'].subtotals ? Math.max(0, ...formState['advanced-alv'].subtotals.map(s => s.id)) : 1;
      const maxToolbarId = formState['advanced-alv'].toolbarButtons ? Math.max(0, ...formState['advanced-alv'].toolbarButtons.map(t => t.id)) : 1;
      
      setIdCounter({
        fieldCatalog: maxFieldCatId + 1,
        sorting: maxSortingId + 1,
        filtering: maxFilteringId + 1,
        subtotals: maxSubtotalsId + 1,
        toolbarButtons: maxToolbarId + 1
      });
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('advanced-alv', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi base
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData(prevData => {
      // Supporta path annidati come "layout.zebra"
      if (name.includes('.')) {
        const [parentKey, childKey] = name.split('.');
        return {
          ...prevData,
          [parentKey]: {
            ...prevData[parentKey],
            [childKey]: type === 'checkbox' ? checked : value
          }
        };
      } else {
        return {
          ...prevData,
          [name]: type === 'checkbox' ? checked : value
        };
      }
    });
  };
  
  // Toggle espansione/collasso sezioni
  const toggleSection = (section) => {
    setExpandedSections({
      ...expandedSections,
      [section]: !expandedSections[section]
    });
  };
  
  // GESTIONE FIELD CATALOG
  
  // Aggiunge un nuovo campo al field catalog
  const handleAddField = () => {
    const newField = { 
      id: idCounter.fieldCatalog, 
      fieldname: `FIELD${idCounter.fieldCatalog}`, 
      seltext: `Campo ${idCounter.fieldCatalog}`, 
      datatype: 'CHAR', 
      outputlen: '10',
      hotspot: false,
      checkbox: false,
      editable: false,
      key: false,
      emphasize: false,
      noOut: false,
      tech: false,
      coltext: '',
      tipText: ''
    };
    
    setFormData({
      ...formData,
      fieldCatalog: [...formData.fieldCatalog, newField]
    });
    
    setIdCounter({
      ...idCounter,
      fieldCatalog: idCounter.fieldCatalog + 1
    });
  };
  
  // Rimuove un campo dal field catalog
  const handleRemoveField = (id) => {
    setFormData({
      ...formData,
      fieldCatalog: formData.fieldCatalog.filter(f => f.id !== id)
    });
  };
  
  // Gestisce il cambiamento di un campo nel field catalog
  const handleFieldChange = (id, fieldName, value, isCheckbox = false) => {
    setFormData({
      ...formData,
      fieldCatalog: formData.fieldCatalog.map(field => 
        field.id === id ? { 
          ...field, 
          [fieldName]: isCheckbox ? !field[fieldName] : value 
        } : field
      )
    });
  };
  
  // Sposta un campo su/giù nel field catalog
  const handleMoveField = (id, direction) => {
    const fieldIndex = formData.fieldCatalog.findIndex(f => f.id === id);
    if (fieldIndex === -1) return;
    
    // Se stiamo spostando in alto e siamo già in cima, o
    // se stiamo spostando in basso e siamo già in fondo, non fare nulla
    if ((direction === 'up' && fieldIndex === 0) || 
        (direction === 'down' && fieldIndex === formData.fieldCatalog.length - 1)) {
      return;
    }
    
    const newFields = [...formData.fieldCatalog];
    const targetIndex = direction === 'up' ? fieldIndex - 1 : fieldIndex + 1;
    
    // Scambia le posizioni
    [newFields[fieldIndex], newFields[targetIndex]] = [newFields[targetIndex], newFields[fieldIndex]];
    
    setFormData({
      ...formData,
      fieldCatalog: newFields
    });
  };
  
  // Imposta il campo in modalità modifica
  const handleEditField = (id) => {
    setEditingField(id);
  };
  
  // GESTIONE SORTING
  
  // Aggiunge un nuovo ordinamento
  const handleAddSorting = () => {
    const newSorting = { 
      id: idCounter.sorting, 
      fieldname: formData.fieldCatalog[0]?.fieldname || 'FIELD', 
      sequence: 'ASCENDING' 
    };
    
    setFormData({
      ...formData,
      sorting: [...formData.sorting, newSorting]
    });
    
    setIdCounter({
      ...idCounter,
      sorting: idCounter.sorting + 1
    });
  };
  
  // Rimuove un ordinamento
  const handleRemoveSorting = (id) => {
    setFormData({
      ...formData,
      sorting: formData.sorting.filter(s => s.id !== id)
    });
  };
  
  // Gestisce il cambiamento di un ordinamento
  const handleSortingChange = (id, field, value) => {
    setFormData({
      ...formData,
      sorting: formData.sorting.map(sort => 
        sort.id === id ? { ...sort, [field]: value } : sort
      )
    });
  };
  
  // GESTIONE FILTERING
  
  // Aggiunge un nuovo filtro
  const handleAddFiltering = () => {
    const newFiltering = { 
      id: idCounter.filtering, 
      fieldname: formData.fieldCatalog[0]?.fieldname || 'FIELD', 
      sign: 'I', 
      option: 'EQ',
      low: '',
      high: ''
    };
    
    setFormData({
      ...formData,
      filtering: [...formData.filtering, newFiltering]
    });
    
    setIdCounter({
      ...idCounter,
      filtering: idCounter.filtering + 1
    });
  };
  
  // Rimuove un filtro
  const handleRemoveFiltering = (id) => {
    setFormData({
      ...formData,
      filtering: formData.filtering.filter(f => f.id !== id)
    });
  };
  
  // Gestisce il cambiamento di un filtro
  const handleFilteringChange = (id, field, value) => {
    setFormData({
      ...formData,
      filtering: formData.filtering.map(filter => 
        filter.id === id ? { ...filter, [field]: value } : filter
      )
    });
  };
  
  // GESTIONE SUBTOTALS
  
  // Aggiunge un nuovo subtotal
  const handleAddSubtotal = () => {
    const newSubtotal = { 
      id: idCounter.subtotals, 
      fieldname: formData.fieldCatalog[0]?.fieldname || 'FIELD'
    };
    
    setFormData({
      ...formData,
      subtotals: [...(formData.subtotals || []), newSubtotal]
    });
    
    setIdCounter({
      ...idCounter,
      subtotals: idCounter.subtotals + 1
    });
  };
  
  // Rimuove un subtotal
  const handleRemoveSubtotal = (id) => {
    setFormData({
      ...formData,
      subtotals: formData.subtotals.filter(s => s.id !== id)
    });
  };
  
  // Gestisce il cambiamento di un subtotal
  const handleSubtotalChange = (id, field, value) => {
    setFormData({
      ...formData,
      subtotals: formData.subtotals.map(subtotal => 
        subtotal.id === id ? { ...subtotal, [field]: value } : subtotal
      )
    });
  };
  
  // GESTIONE TOOLBAR BUTTONS
  
  // Aggiunge un nuovo pulsante toolbar
  const handleAddToolbarButton = () => {
    const newButton = { 
      id: idCounter.toolbarButtons, 
      function: `BTN${idCounter.toolbarButtons}`, 
      icon: '@15@', 
      text: `Pulsante ${idCounter.toolbarButtons}`,
      quickinfo: `Descrizione pulsante ${idCounter.toolbarButtons}`
    };
    
    setFormData({
      ...formData,
      toolbarButtons: [...(formData.toolbarButtons || []), newButton]
    });
    
    setIdCounter({
      ...idCounter,
      toolbarButtons: idCounter.toolbarButtons + 1
    });
  };
  
  // Rimuove un pulsante toolbar
  const handleRemoveToolbarButton = (id) => {
    setFormData({
      ...formData,
      toolbarButtons: formData.toolbarButtons.filter(t => t.id !== id)
    });
  };
  
  // Gestisce il cambiamento di un pulsante toolbar
  const handleToolbarButtonChange = (id, field, value) => {
    setFormData({
      ...formData,
      toolbarButtons: formData.toolbarButtons.map(button => 
        button.id === id ? { ...button, [field]: value } : button
      )
    });
  };
  
  // GESTIONE EVENTI
  
  // Gestisce il cambiamento degli eventi
  const handleEventChange = (eventName, checked) => {
    setFormData({
      ...formData,
      events: {
        ...formData.events,
        [eventName]: checked
      }
    });
  };
  
  // Genera anteprima del codice
  const handleGeneratePreview = () => {
    // Implementazione della generazione dell'anteprima
    // Questo sarebbe fatto nella funzione di generatore di codice reale
    setPreviewCode('* Anteprima del codice ALV...\n\n' + 
                  `DATA: gt_fieldcat TYPE lvc_t_fcat,\n` +
                  `      gs_layout   TYPE lvc_s_layo,\n` +
                  `      gt_sort     TYPE lvc_t_sort,\n` +
                  `      gt_filt     TYPE lvc_t_filt.\n\n` +
                  `* Composizione field catalog...\n` +
                  `* Chiamata alla funzione...\n`);
    
    // Apri la sezione di anteprima
    setExpandedSections({
      ...expandedSections,
      preview: true
    });
  };
  
  // Gestisce la generazione del codice finale
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('advanced-alv', formData);
    }
  };
  
  return (
    <FormContainer>
      {/* Sezione Base */}
      <SectionHeader onClick={() => toggleSection('basic')}>
        <h4>Informazioni Base</h4>
        {expandedSections.basic ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.basic && (
        <SectionContent>
          <FormGroup label="Tipo ALV:">
            <select
              name="alvType"
              value={formData.alvType}
              onChange={handleChange}
            >
              <option value="GRID">Grid standard</option>
              <option value="TREE">Tree (Albero)</option>
              <option value="HIERSEQ">Hierarchical Sequential</option>
            </select>
          </FormGroup>
          
          <FormGroup label="Nome tabella interna:">
            <ControlledInput
              type="text"
              name="tableName"
              value={formData.tableName}
              onChange={handleChange}
              placeholder="es. gt_data"
            />
          </FormGroup>
          
          <FormGroup label="Nome struttura:">
            <ControlledInput
              type="text"
              name="structName"
              value={formData.structName}
              onChange={handleChange}
              placeholder="es. gty_data"
            />
          </FormGroup>
          
          <FormGroup label="Tipo di output:">
            <select
              name="outputType"
              value={formData.outputType}
              onChange={handleChange}
            >
              <option value="GRID">REUSE_ALV_GRID_DISPLAY</option>
              <option value="GRID_DISPLAY">REUSE_ALV_GRID_DISPLAY_LVC</option>
              <option value="LIST">REUSE_ALV_LIST_DISPLAY</option>
              <option value="HIERSEQ_DISPLAY">REUSE_ALV_HIERSEQ_LIST_DISPLAY</option>
              <option value="BLOCK">REUSE_ALV_BLOCK_LIST_APPEND</option>
              {formData.isOO && (
                <>
                  <option value="CL_GUI_ALV_GRID">CL_GUI_ALV_GRID (OO)</option>
                  <option value="CL_SALV_TABLE">CL_SALV_TABLE (OO)</option>
                </>
              )}
            </select>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="isOO"
              checked={formData.isOO}
              onChange={handleChange}
              id="isOO"
            />
            <label htmlFor="isOO">Usa approccio Object Oriented</label>
          </FormGroup>
          
          {formData.isOO && (
            <>
              <FormGroup label="Nome classe:">
                <ControlledInput
                  type="text"
                  name="className"
                  value={formData.className}
                  onChange={handleChange}
                  placeholder="es. LCL_ALV_HANDLER"
                />
              </FormGroup>
              
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="useBuilder"
                  checked={formData.useBuilder}
                  onChange={handleChange}
                  id="useBuilder"
                />
                <label htmlFor="useBuilder">Usa pattern Builder (fluent interface)</label>
              </FormGroup>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Field Catalog */}
      <SectionHeader onClick={() => toggleSection('fieldCatalog')}>
        <h4>Field Catalog</h4>
        {expandedSections.fieldCatalog ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.fieldCatalog && (
        <SectionContent>
          <FieldTable>
            <FieldTableHeader>
              <div>Nome Campo</div>
              <div>Etichetta</div>
              <div>Tipo</div>
              <div>Lunghezza</div>
              <div>Azioni</div>
            </FieldTableHeader>
            
            {formData.fieldCatalog.map(field => (
              <FieldTableRow key={field.id}>
                {editingField === field.id ? (
                  // Modalità di modifica completa
                  <FieldEditForm>
                    <FormGroup label="Nome campo:">
                      <ControlledInput
                        type="text"
                        value={field.fieldname}
                        onChange={(e) => handleFieldChange(field.id, 'fieldname', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Etichetta:">
                      <ControlledInput
                        type="text"
                        value={field.seltext}
                        onChange={(e) => handleFieldChange(field.id, 'seltext', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Tipo dati:">
                      <select
                        value={field.datatype}
                        onChange={(e) => handleFieldChange(field.id, 'datatype', e.target.value)}
                      >
                        <option value="CHAR">CHAR</option>
                        <option value="NUMC">NUMC</option>
                        <option value="DEC">DEC</option>
                        <option value="INT">INT</option>
                        <option value="CURR">CURR</option>
                        <option value="QUAN">QUAN</option>
                        <option value="DATS">DATS</option>
                        <option value="TIMS">TIMS</option>
                        <option value="ACCP">ACCP</option>
                        <option value="RAW">RAW</option>
                        <option value="STRG">STRG</option>
                      </select>
                    </FormGroup>
                    
                    <FormGroup label="Lunghezza Output:">
                      <ControlledInput
                        type="text"
                        value={field.outputlen}
                        onChange={(e) => handleFieldChange(field.id, 'outputlen', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Titolo Colonna (opzionale):">
                      <ControlledInput
                        type="text"
                        value={field.coltext}
                        onChange={(e) => handleFieldChange(field.id, 'coltext', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FormGroup label="Testo Tooltip (opzionale):">
                      <ControlledInput
                        type="text"
                        value={field.tipText}
                        onChange={(e) => handleFieldChange(field.id, 'tipText', e.target.value)}
                      />
                    </FormGroup>
                    
                    <FieldOptionsGrid>
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.hotspot}
                          onChange={() => handleFieldChange(field.id, 'hotspot', null, true)}
                          id={`hotspot_${field.id}`}
                        />
                        <label htmlFor={`hotspot_${field.id}`}>Hotspot</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.checkbox}
                          onChange={() => handleFieldChange(field.id, 'checkbox', null, true)}
                          id={`checkbox_${field.id}`}
                        />
                        <label htmlFor={`checkbox_${field.id}`}>Checkbox</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.editable}
                          onChange={() => handleFieldChange(field.id, 'editable', null, true)}
                          id={`editable_${field.id}`}
                        />
                        <label htmlFor={`editable_${field.id}`}>Editable</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.key}
                          onChange={() => handleFieldChange(field.id, 'key', null, true)}
                          id={`key_${field.id}`}
                        />
                        <label htmlFor={`key_${field.id}`}>Key</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.emphasize}
                          onChange={() => handleFieldChange(field.id, 'emphasize', null, true)}
                          id={`emphasize_${field.id}`}
                        />
                        <label htmlFor={`emphasize_${field.id}`}>Emphasize</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.noOut}
                          onChange={() => handleFieldChange(field.id, 'noOut', null, true)}
                          id={`noOut_${field.id}`}
                        />
                        <label htmlFor={`noOut_${field.id}`}>No Output</label>
                      </FormGroup>
                      
                      <FormGroup inline>
                        <input
                          type="checkbox"
                          checked={field.tech}
                          onChange={() => handleFieldChange(field.id, 'tech', null, true)}
                          id={`tech_${field.id}`}
                        />
                        <label htmlFor={`tech_${field.id}`}>Technical</label>
                      </FormGroup>
                    </FieldOptionsGrid>
                    
                    <Button
                      variant="primary"
                      size="small"
                      onClick={() => setEditingField(null)}
                    >
                      Conferma
                    </Button>
                  </FieldEditForm>
                ) : (
                  // Visualizzazione normale
                  <>
                    <div>{field.fieldname}</div>
                    <div>{field.seltext}</div>
                    <div>{field.datatype}</div>
                    <div>{field.outputlen}</div>
                    <FieldActions>
                      <IconButton onClick={() => handleEditField(field.id)} title="Modifica">
                        <FiEdit size={16} />
                      </IconButton>
                      <IconButton onClick={() => handleMoveField(field.id, 'up')} title="Sposta su">
                        <FiArrowUp size={16} />
                      </IconButton>
                      <IconButton onClick={() => handleMoveField(field.id, 'down')} title="Sposta giù">
                        <FiArrowDown size={16} />
                      </IconButton>
                      <IconButton onClick={() => handleRemoveField(field.id)} title="Rimuovi">
                        <FiTrash2 size={16} />
                      </IconButton>
                    </FieldActions>
                  </>
                )}
              </FieldTableRow>
            ))}
          </FieldTable>
          
          <ActionButton onClick={handleAddField}>
            <FiPlus size={16} /> Aggiungi Campo
          </ActionButton>
        </SectionContent>
      )}
      
      {/* Sezione Layout */}
      <SectionHeader onClick={() => toggleSection('layout')}>
        <h4>Layout</h4>
        {expandedSections.layout ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.layout && (
        <SectionContent>
          <FormGroup label="Titolo Grid:">
            <ControlledInput
              type="text"
              name="layout.gridTitle"
              value={formData.layout.gridTitle}
              onChange={handleChange}
              placeholder="es. Report ALV"
            />
          </FormGroup>
          
          <FormGroup label="Modalità Selezione:">
            <select
              name="layout.selMode"
              value={formData.layout.selMode}
              onChange={handleChange}
            >
              <option value="A">A - Multiple with Marker</option>
              <option value="B">B - Multiple w/o Marker</option>
              <option value="C">C - Single with Marker</option>
              <option value="D">D - Single w/o Marker</option>
            </select>
          </FormGroup>
          
          <LayoutOptionsGrid>
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.zebra"
                checked={formData.layout.zebra}
                onChange={handleChange}
                id="zebra"
              />
              <label htmlFor="zebra">Righe zebrate</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.colwdOpt"
                checked={formData.layout.colwdOpt}
                onChange={handleChange}
                id="colwdOpt"
              />
              <label htmlFor="colwdOpt">Ottimizza larghezza</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.cellEdit"
                checked={formData.layout.cellEdit}
                onChange={handleChange}
                id="cellEdit"
              />
              <label htmlFor="cellEdit">Edit celle</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.noHeaders"
                checked={formData.layout.noHeaders}
                onChange={handleChange}
                id="noHeaders"
              />
              <label htmlFor="noHeaders">Nascondi intestazioni</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.noMerging"
                checked={formData.layout.noMerging}
                onChange={handleChange}
                id="noMerging"
              />
              <label htmlFor="noMerging">No merging celle</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.sglClick"
                checked={formData.layout.sglClick}
                onChange={handleChange}
                id="sglClick"
              />
              <label htmlFor="sglClick">Singolo click</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.noRowMark"
                checked={formData.layout.noRowMark}
                onChange={handleChange}
                id="noRowMark"
              />
              <label htmlFor="noRowMark">No marcatura riga</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.smallTitle"
                checked={formData.layout.smallTitle}
                onChange={handleChange}
                id="smallTitle"
              />
              <label htmlFor="smallTitle">Titolo piccolo</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.noRowIns"
                checked={formData.layout.noRowIns}
                onChange={handleChange}
                id="noRowIns"
              />
              <label htmlFor="noRowIns">No inserimento righe</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="layout.noRowResize"
                checked={formData.layout.noRowResize}
                onChange={handleChange}
                id="noRowResize"
              />
              <label htmlFor="noRowResize">No resize righe</label>
            </FormGroup>
          </LayoutOptionsGrid>
          
          <FormGroup label="Nome Style (opzionale):">
            <ControlledInput
              type="text"
              name="layout.styleName"
              value={formData.layout.styleName}
              onChange={handleChange}
              placeholder="es. ALV_GRID"
            />
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="layout.excelDisp"
              checked={formData.layout.excelDisp}
              onChange={handleChange}
              id="excelDisp"
            />
            <label htmlFor="excelDisp">Abilita export Excel</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="layout.webDisp"
              checked={formData.layout.webDisp}
              onChange={handleChange}
              id="webDisp"
            />
            <label htmlFor="webDisp">Abilita visualizzazione web</label>
          </FormGroup>
        </SectionContent>
      )}
      
      {/* Sezione Eventi */}
      <SectionHeader onClick={() => toggleSection('events')}>
        <h4>Eventi</h4>
        {expandedSections.events ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.events && (
        <SectionContent>
          <EventsGrid>
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.topOfPage}
                onChange={(e) => handleEventChange('topOfPage', e.target.checked)}
                id="topOfPage"
              />
              <label htmlFor="topOfPage">TOP_OF_PAGE</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.endOfPage}
                onChange={(e) => handleEventChange('endOfPage', e.target.checked)}
                id="endOfPage"
              />
              <label htmlFor="endOfPage">END_OF_PAGE</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.userCommand}
                onChange={(e) => handleEventChange('userCommand', e.target.checked)}
                id="userCommand"
              />
              <label htmlFor="userCommand">USER_COMMAND</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.hotspotClick}
                onChange={(e) => handleEventChange('hotspotClick', e.target.checked)}
                id="hotspotClick"
              />
              <label htmlFor="hotspotClick">HOTSPOT_CLICK</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.doubleClick}
                onChange={(e) => handleEventChange('doubleClick', e.target.checked)}
                id="doubleClick"
              />
              <label htmlFor="doubleClick">DOUBLE_CLICK</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.dataChanged}
                onChange={(e) => handleEventChange('dataChanged', e.target.checked)}
                id="dataChanged"
              />
              <label htmlFor="dataChanged">DATA_CHANGED</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.toolbar}
                onChange={(e) => handleEventChange('toolbar', e.target.checked)}
                id="toolbar"
              />
              <label htmlFor="toolbar">TOOLBAR</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.beforeUserCommand}
                onChange={(e) => handleEventChange('beforeUserCommand', e.target.checked)}
                id="beforeUserCommand"
              />
              <label htmlFor="beforeUserCommand">BEFORE_USER_COMMAND</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.afterUserCommand}
                onChange={(e) => handleEventChange('afterUserCommand', e.target.checked)}
                id="afterUserCommand"
              />
              <label htmlFor="afterUserCommand">AFTER_USER_COMMAND</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.buttonClick}
                onChange={(e) => handleEventChange('buttonClick', e.target.checked)}
                id="buttonClick"
              />
              <label htmlFor="buttonClick">BUTTON_CLICK</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.menuButton}
                onChange={(e) => handleEventChange('menuButton', e.target.checked)}
                id="menuButton"
              />
              <label htmlFor="menuButton">MENU_BUTTON</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.link}
                onChange={(e) => handleEventChange('link', e.target.checked)}
                id="link"
              />
              <label htmlFor="link">LINK_CLICK</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.events.print}
                onChange={(e) => handleEventChange('print', e.target.checked)}
                id="print"
              />
              <label htmlFor="print">PRINT</label>
            </FormGroup>
          </EventsGrid>
        </SectionContent>
      )}
      
      {/* Sezione Sorting */}
      <SectionHeader onClick={() => toggleSection('sorting')}>
        <h4>Ordinamento</h4>
        {expandedSections.sorting ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.sorting && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="sortingEnabled"
              checked={formData.sortingEnabled}
              onChange={handleChange}
              id="sortingEnabled"
            />
            <label htmlFor="sortingEnabled">Abilita ordinamento predefinito</label>
          </FormGroup>
          
          {formData.sortingEnabled && (
            <>
              <SortTable>
                <SortTableHeader>
                  <div>Campo</div>
                  <div>Sequenza</div>
                  <div>Azioni</div>
                </SortTableHeader>
                
                {formData.sorting.map(sort => (
                  <SortTableRow key={sort.id}>
                    <div>
                      <select
                        value={sort.fieldname}
                        onChange={(e) => handleSortingChange(sort.id, 'fieldname', e.target.value)}
                      >
                        {formData.fieldCatalog.map(field => (
                          <option key={field.id} value={field.fieldname}>{field.fieldname}</option>
                        ))}
                      </select>
                    </div>
                    <div>
                      <select
                        value={sort.sequence}
                        onChange={(e) => handleSortingChange(sort.id, 'sequence', e.target.value)}
                      >
                        <option value="ASCENDING">Ascendente</option>
                        <option value="DESCENDING">Discendente</option>
                      </select>
                    </div>
                    <SortActions>
                      <IconButton onClick={() => handleRemoveSorting(sort.id)} title="Rimuovi">
                        <FiTrash2 size={16} />
                      </IconButton>
                    </SortActions>
                  </SortTableRow>
                ))}
              </SortTable>
              
              <ActionButton onClick={handleAddSorting}>
                <FiPlus size={16} /> Aggiungi Ordinamento
              </ActionButton>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Filtering */}
      <SectionHeader onClick={() => toggleSection('filtering')}>
        <h4>Filtri</h4>
        {expandedSections.filtering ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.filtering && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="filteringEnabled"
              checked={formData.filteringEnabled}
              onChange={handleChange}
              id="filteringEnabled"
            />
            <label htmlFor="filteringEnabled">Abilita filtri predefiniti</label>
          </FormGroup>
          
          {formData.filteringEnabled && (
            <>
              <FilterTable>
                <FilterTableHeader>
                  <div>Campo</div>
                  <div>Segno</div>
                  <div>Opzione</div>
                  <div>Low</div>
                  <div>High</div>
                  <div>Azioni</div>
                </FilterTableHeader>
                
                {formData.filtering.map(filter => (
                  <FilterTableRow key={filter.id}>
                    <div>
                      <select
                        value={filter.fieldname}
                        onChange={(e) => handleFilteringChange(filter.id, 'fieldname', e.target.value)}
                      >
                        {formData.fieldCatalog.map(field => (
                          <option key={field.id} value={field.fieldname}>{field.fieldname}</option>
                        ))}
                      </select>
                    </div>
                    <div>
                      <select
                        value={filter.sign}
                        onChange={(e) => handleFilteringChange(filter.id, 'sign', e.target.value)}
                      >
                        <option value="I">Include</option>
                        <option value="E">Exclude</option>
                      </select>
                    </div>
                    <div>
                      <select
                        value={filter.option}
                        onChange={(e) => handleFilteringChange(filter.id, 'option', e.target.value)}
                      >
                        <option value="EQ">Equals (=)</option>
                        <option value="NE">Not Equals (&lt;&gt;)</option>
                        <option value="GT">Greater Than (&gt;)</option>
                        <option value="GE">Greater or Equal (&gt;=)</option>
                        <option value="LT">Less Than (&lt;)</option>
                        <option value="LE">Less or Equal (&lt;=)</option>
                        <option value="BT">Between</option>
                        <option value="NB">Not Between</option>
                        <option value="CP">Contains Pattern</option>
                        <option value="NP">Not Contains Pattern</option>
                      </select>
                    </div>
                    <div>
                      <ControlledInput
                        type="text"
                        value={filter.low}
                        onChange={(e) => handleFilteringChange(filter.id, 'low', e.target.value)}
                      />
                    </div>
                    <div>
                      <ControlledInput
                        type="text"
                        value={filter.high}
                        onChange={(e) => handleFilteringChange(filter.id, 'high', e.target.value)}
                        disabled={!['BT', 'NB'].includes(filter.option)}
                      />
                    </div>
                    <FilterActions>
                      <IconButton onClick={() => handleRemoveFiltering(filter.id)} title="Rimuovi">
                        <FiTrash2 size={16} />
                      </IconButton>
                    </FilterActions>
                  </FilterTableRow>
                ))}
              </FilterTable>
              
              <ActionButton onClick={handleAddFiltering}>
                <FiPlus size={16} /> Aggiungi Filtro
              </ActionButton>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Subtotals */}
      <SectionHeader onClick={() => toggleSection('subtotals')}>
        <h4>Subtotali e Aggregazioni</h4>
        {expandedSections.subtotals ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.subtotals && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="subtotalsEnabled"
              checked={formData.subtotalsEnabled}
              onChange={handleChange}
              id="subtotalsEnabled"
            />
            <label htmlFor="subtotalsEnabled">Abilita subtotali</label>
          </FormGroup>
          
          {formData.subtotalsEnabled && (
            <>
              <SubtotalTable>
                <SubtotalTableHeader>
                  <div>Campo</div>
                  <div>Azioni</div>
                </SubtotalTableHeader>
                
                {formData.subtotals.map(subtotal => (
                  <SubtotalTableRow key={subtotal.id}>
                    <div>
                      <select
                        value={subtotal.fieldname}
                        onChange={(e) => handleSubtotalChange(subtotal.id, 'fieldname', e.target.value)}
                      >
                        {formData.fieldCatalog.map(field => (
                          <option key={field.id} value={field.fieldname}>{field.fieldname}</option>
                        ))}
                      </select>
                    </div>
                    <SubtotalActions>
                      <IconButton onClick={() => handleRemoveSubtotal(subtotal.id)} title="Rimuovi">
                        <FiTrash2 size={16} />
                      </IconButton>
                    </SubtotalActions>
                  </SubtotalTableRow>
                ))}
              </SubtotalTable>
              
              <ActionButton onClick={handleAddSubtotal}>
                <FiPlus size={16} /> Aggiungi Subtotale
              </ActionButton>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Toolbar */}
      <SectionHeader onClick={() => toggleSection('toolbar')}>
        <h4>Toolbar e Menu Button</h4>
        {expandedSections.toolbar ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.toolbar && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="toolbarEnabled"
              checked={formData.toolbarEnabled}
              onChange={handleChange}
              id="toolbarEnabled"
            />
            <label htmlFor="toolbarEnabled">Abilita pulsanti toolbar personalizzati</label>
          </FormGroup>
          
          {formData.toolbarEnabled && (
            <>
              <ToolbarTable>
                <ToolbarTableHeader>
                  <div>Function</div>
                  <div>Icona</div>
                  <div>Testo</div>
                  <div>Quickinfo</div>
                  <div>Azioni</div>
                </ToolbarTableHeader>
                
                {formData.toolbarButtons.map(button => (
                  <ToolbarTableRow key={button.id}>
                    <div>
                      <ControlledInput
                        type="text"
                        value={button.function}
                        onChange={(e) => handleToolbarButtonChange(button.id, 'function', e.target.value)}
                      />
                    </div>
                    <div>
                      <ControlledInput
                        type="text"
                        value={button.icon}
                        onChange={(e) => handleToolbarButtonChange(button.id, 'icon', e.target.value)}
                      />
                    </div>
                    <div>
                      <ControlledInput
                        type="text"
                        value={button.text}
                        onChange={(e) => handleToolbarButtonChange(button.id, 'text', e.target.value)}
                      />
                    </div>
                    <div>
                      <ControlledInput
                        type="text"
                        value={button.quickinfo}
                        onChange={(e) => handleToolbarButtonChange(button.id, 'quickinfo', e.target.value)}
                      />
                    </div>
                    <ToolbarActions>
                      <IconButton onClick={() => handleRemoveToolbarButton(button.id)} title="Rimuovi">
                        <FiTrash2 size={16} />
                      </IconButton>
                    </ToolbarActions>
                  </ToolbarTableRow>
                ))}
              </ToolbarTable>
              
              <ActionButton onClick={handleAddToolbarButton}>
                <FiPlus size={16} /> Aggiungi Pulsante
              </ActionButton>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Varianti */}
      <SectionHeader onClick={() => toggleSection('variant')}>
        <h4>Varianti</h4>
        {expandedSections.variant ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.variant && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="variantEnabled"
              checked={formData.variantEnabled}
              onChange={handleChange}
              id="variantEnabled"
            />
            <label htmlFor="variantEnabled">Abilita salvataggio varianti</label>
          </FormGroup>
          
          {formData.variantEnabled && (
            <TwoColumnsGrid>
              <FormGroup label="Handle:">
                <ControlledInput
                  type="text"
                  name="variantHandle"
                  value={formData.variantHandle}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Report:">
                <ControlledInput
                  type="text"
                  name="variantReport"
                  value={formData.variantReport}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Username:">
                <ControlledInput
                  type="text"
                  name="variantUsername"
                  value={formData.variantUsername}
                  onChange={handleChange}
                />
              </FormGroup>
            </TwoColumnsGrid>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Extra */}
      <SectionHeader onClick={() => toggleSection('extras')}>
        <h4>Opzioni Extra</h4>
        {expandedSections.extras ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.extras && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="editModeEnabled"
              checked={formData.editModeEnabled}
              onChange={handleChange}
              id="editModeEnabled"
            />
            <label htmlFor="editModeEnabled">Abilita modalità editing</label>
          </FormGroup>
          
          {formData.editModeEnabled && (
            <>
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="registerEnterPressed"
                  checked={formData.registerEnterPressed}
                  onChange={handleChange}
                  id="registerEnterPressed"
                />
                <label htmlFor="registerEnterPressed">Registra pressione ENTER</label>
              </FormGroup>
              
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="checkChanges"
                  checked={formData.checkChanges}
                  onChange={handleChange}
                  id="checkChanges"
                />
                <label htmlFor="checkChanges">Controlla modifiche</label>
              </FormGroup>
              
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="displayChanges"
                  checked={formData.displayChanges}
                  onChange={handleChange}
                  id="displayChanges"
                />
                <label htmlFor="displayChanges">Visualizza modifiche</label>
              </FormGroup>
            </>
          )}
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="popupEnabled"
              checked={formData.popupEnabled}
              onChange={handleChange}
              id="popupEnabled"
            />
            <label htmlFor="popupEnabled">Visualizza in popup</label>
          </FormGroup>
          
          {formData.popupEnabled && (
            <TwoColumnsGrid>
              <FormGroup label="Colonna iniziale:">
                <ControlledInput
                  type="text"
                  name="popupStartCol"
                  value={formData.popupStartCol}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Colonna finale:">
                <ControlledInput
                  type="text"
                  name="popupEndCol"
                  value={formData.popupEndCol}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Riga iniziale:">
                <ControlledInput
                  type="text"
                  name="popupStartLine"
                  value={formData.popupStartLine}
                  onChange={handleChange}
                />
              </FormGroup>
              
              <FormGroup label="Riga finale:">
                <ControlledInput
                  type="text"
                  name="popupEndLine"
                  value={formData.popupEndLine}
                  onChange={handleChange}
                />
              </FormGroup>
            </TwoColumnsGrid>
          )}
          
          <FormGroup label="Container Name (per visualizzazione Custom Container):">
            <ControlledInput
              type="text"
              name="containerName"
              value={formData.containerName}
              onChange={handleChange}
              placeholder="es. CONTAINER_1"
            />
          </FormGroup>
        </SectionContent>
      )}
      
      {/* Anteprima Codice */}
      <SectionHeader onClick={() => toggleSection('preview')}>
        <h4>Anteprima Codice</h4>
        {expandedSections.preview ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.preview && (
        <SectionContent>
          <PreviewButton onClick={handleGeneratePreview}>
            <FiEye /> Aggiorna Anteprima
          </PreviewButton>
          
          <PreviewCode>
            <pre>{previewCode || 'Clicca su "Aggiorna Anteprima" per visualizzare il codice generato.'}</pre>
          </PreviewCode>
          
          <InfoNote>
            Nota: Questa è solo un'anteprima. Clicca su "Genera Codice" per generare il codice finale.
          </InfoNote>
        </SectionContent>
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

// Stili del componente
const FormContainer = styled.div`
  padding: 15px;
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

const TwoColumnsGrid = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 15px;
  
  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
`;

// Field Catalog styles
const FieldTable = styled.div`
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
`;

const FieldTableHeader = styled.div`
  display: grid;
  grid-template-columns: 2fr 2fr 1fr 1fr 1fr;
  background: #f0f0f0;
  padding: 10px;
  font-weight: bold;
  border-bottom: 1px solid #ddd;
  
  div {
    padding: 0 5px;
  }
`;

const FieldTableRow = styled.div`
  display: grid;
  grid-template-columns: 2fr 2fr 1fr 1fr 1fr;
  border-bottom: 1px solid #eee;
  padding: 10px;
  align-items: center;
  
  &:last-child {
    border-bottom: none;
  }
  
  &:nth-child(even) {
    background-color: #f9f9f9;
  }
  
  div {
    padding: 0 5px;
  }
`;

const FieldActions = styled.div`
  display: flex;
  gap: 5px;
  justify-content: flex-end;
`;

const FieldEditForm = styled.div`
  grid-column: 1 / -1;
  background-color: #f5f5f5;
  padding: 15px;
  border-radius: 4px;
  display: flex;
  flex-direction: column;
  gap: 10px;
`;

const FieldOptionsGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 10px;
  margin: 10px 0;
  
  @media (max-width: 768px) {
    grid-template-columns: repeat(2, 1fr);
  }
`;

const IconButton = styled.button`
  background: none;
  border: none;
  cursor: pointer;
  color: #0066cc;
  padding: 5px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 3px;
  
  &:hover {
    background: rgba(0, 102, 204, 0.1);
  }
`;

const ActionButton = styled.button`
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 8px 15px;
  background: #f0f0f0;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: pointer;
  color: #333;
  
  &:hover {
    background: #e0e0e0;
  }
`;

// Layout styles
const LayoutOptionsGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 15px;
  margin: 15px 0;
  
  @media (max-width: 768px) {
    grid-template-columns: repeat(2, 1fr);
  }
`;

// Events styles
const EventsGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 15px;
  
  @media (max-width: 768px) {
    grid-template-columns: repeat(2, 1fr);
  }
`;

// Sorting styles
const SortTable = styled.div`
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
`;

const SortTableHeader = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr auto;
  background: #f0f0f0;
  padding: 10px;
  font-weight: bold;
  border-bottom: 1px solid #ddd;
  
  div {
    padding: 0 5px;
  }
`;

const SortTableRow = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr auto;
  border-bottom: 1px solid #eee;
  padding: 10px;
  align-items: center;
  
  &:last-child {
    border-bottom: none;
  }
  
  select {
    width: 100%;
    padding: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
  }
  
  div {
    padding: 0 5px;
  }
`;

const SortActions = styled.div`
  display: flex;
  gap: 5px;
  justify-content: flex-end;
`;

// Filtering styles
const FilterTable = styled.div`
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
`;

const FilterTableHeader = styled.div`
  display: grid;
  grid-template-columns: 1fr 0.7fr 1fr 1fr 1fr auto;
  background: #f0f0f0;
  padding: 10px;
  font-weight: bold;
  border-bottom: 1px solid #ddd;
  
  div {
    padding: 0 5px;
  }
`;

const FilterTableRow = styled.div`
  display: grid;
  grid-template-columns: 1fr 0.7fr 1fr 1fr 1fr auto;
  border-bottom: 1px solid #eee;
  padding: 10px;
  align-items: center;
  
  &:last-child {
    border-bottom: none;
  }
  
  select, input {
    width: 100%;
    padding: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
  }
  
  div {
    padding: 0 5px;
  }
`;

const FilterActions = styled.div`
  display: flex;
  gap: 5px;
  justify-content: flex-end;
`;

// Subtotal styles
const SubtotalTable = styled.div`
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
`;

const SubtotalTableHeader = styled.div`
  display: grid;
  grid-template-columns: 1fr auto;
  background: #f0f0f0;
  padding: 10px;
  font-weight: bold;
  border-bottom: 1px solid #ddd;
  
  div {
    padding: 0 5px;
  }
`;

const SubtotalTableRow = styled.div`
  display: grid;
  grid-template-columns: 1fr auto;
  border-bottom: 1px solid #eee;
  padding: 10px;
  align-items: center;
  
  &:last-child {
    border-bottom: none;
  }
  
  select {
    width: 100%;
    padding: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
  }
  
  div {
    padding: 0 5px;
  }
`;

const SubtotalActions = styled.div`
  display: flex;
  gap: 5px;
  justify-content: flex-end;
`;

// Toolbar styles
const ToolbarTable = styled.div`
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
`;

const ToolbarTableHeader = styled.div`
  display: grid;
  grid-template-columns: 1fr 0.7fr 1fr 1fr auto;
  background: #f0f0f0;
  padding: 10px;
  font-weight: bold;
  border-bottom: 1px solid #ddd;
  
  div {
    padding: 0 5px;
  }
`;

const ToolbarTableRow = styled.div`
  display: grid;
  grid-template-columns: 1fr 0.7fr 1fr 1fr auto;
  border-bottom: 1px solid #eee;
  padding: 10px;
  align-items: center;
  
  &:last-child {
    border-bottom: none;
  }
  
  input {
    width: 100%;
    padding: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
  }
  
  div {
    padding: 0 5px;
  }
`;

const ToolbarActions = styled.div`
  display: flex;
  gap: 5px;
  justify-content: flex-end;
`;

// Preview styles
const PreviewButton = styled.button`
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 12px;
  background: #007bff;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 14px;
  margin-bottom: 10px;
  
  &:hover {
    background: #0069d9;
  }
`;

const PreviewCode = styled.div`
  background: #f0f0f0;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 10px;
  max-height: 300px;
  overflow-y: auto;
  
  pre {
    margin: 0;
    font-family: 'Fira Code', 'Courier New', monospace;
    font-size: 14px;
    line-height: 1.5;
  }
`;

const InfoNote = styled.div`
  margin-top: 10px;
  padding: 8px 12px;
  background: #e8f4fd;
  border-left: 4px solid #0099ff;
  font-size: 13px;
  color: #333;
  border-radius: 2px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default AdvancedAlvForm;