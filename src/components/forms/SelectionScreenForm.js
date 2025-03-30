import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiMove, FiChevronDown, FiChevronUp } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const SelectionScreenForm = ({ onGenerate }) => {
  // Stato locale del form con tutte le opzioni per selection screen
  const [formData, setFormData] = useState({
    reportTitle: 'ZREPORT_CUSTOM',
    reportDescription: 'Custom Report',
    parameters: [
      { 
        id: 1, 
        name: 'p_date', 
        type: 'sy-datum', 
        defaultValue: 'sy-datum', 
        obligatory: false, 
        label: 'Data di riferimento', 
        lowerCase: false,
        radioButton: false,
        radioGroup: '',
        checkBox: false,
        visibleLength: '',
        listBox: false,
        matchcode: ''
      }
    ],
    selectOptions: [
      { 
        id: 1, 
        name: 's_matnr', 
        type: 'matnr', 
        label: 'Materiale', 
        multiple: true, 
        noExtension: false, 
        noIntervals: false,
        obligatory: false,
        matchcode: 'MAT1'
      }
    ],
    blocks: [
      {
        id: 1,
        title: 'Selezione Principale',
        frameTitle: 'Parametri principali',
        useFrame: true,
        parameters: [1],
        selectOptions: [1]
      }
    ],
    includeVariants: true,
    useSelectionScreen: true,
    addAtSelection: false,
    atSelectionText: ''
  });
  
  // Stato per le sezioni collassabili
  const [expandedSections, setExpandedSections] = useState({
    parameters: true,
    selectOptions: true,
    blocks: true,
    options: false
  });
  
  // ID per nuovi elementi
  const [idCounter, setIdCounter] = useState({
    parameters: 2,
    selectOptions: 2,
    blocks: 2
  });
  
  // Per visualizzare/modificare un blocco specifico
  const [activeBlockId, setActiveBlockId] = useState(1);
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['selection-screen']) {
      setFormData(formState['selection-screen']);
      
      // Aggiorna anche i contatori ID
      const maxParamId = Math.max(0, ...formState['selection-screen'].parameters.map(p => p.id));
      const maxSelOptId = Math.max(0, ...formState['selection-screen'].selectOptions.map(s => s.id));
      const maxBlockId = Math.max(0, ...formState['selection-screen'].blocks.map(b => b.id));
      
      setIdCounter({
        parameters: maxParamId + 1,
        selectOptions: maxSelOptId + 1,
        blocks: maxBlockId + 1
      });
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('selection-screen', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi base
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Toggle espansione/collasso sezioni
  const toggleSection = (section) => {
    setExpandedSections({
      ...expandedSections,
      [section]: !expandedSections[section]
    });
  };
  
  // Aggiunge un nuovo parametro
  const handleAddParameter = () => {
    const newParam = { 
      id: idCounter.parameters, 
      name: `p_param${idCounter.parameters}`, 
      type: 'string', 
      defaultValue: '', 
      obligatory: false, 
      label: `Parametro ${idCounter.parameters}`,
      lowerCase: false,
      radioButton: false,
      radioGroup: '',
      checkBox: false,
      visibleLength: '',
      listBox: false,
      matchcode: ''
    };
    
    setFormData({
      ...formData,
      parameters: [...formData.parameters, newParam]
    });
    
    setIdCounter({
      ...idCounter,
      parameters: idCounter.parameters + 1
    });
  };
  
  // Rimuove un parametro
  const handleRemoveParameter = (id) => {
    setFormData({
      ...formData,
      parameters: formData.parameters.filter(p => p.id !== id),
      // Aggiorna anche i blocchi per rimuovere i riferimenti al parametro rimosso
      blocks: formData.blocks.map(block => ({
        ...block,
        parameters: block.parameters.filter(paramId => paramId !== id)
      }))
    });
  };
  
  // Gestisce il cambiamento di un parametro
  const handleParameterChange = (id, field, value, isCheckbox = false) => {
    setFormData({
      ...formData,
      parameters: formData.parameters.map(param => 
        param.id === id ? { 
          ...param, 
          [field]: isCheckbox ? !param[field] : value 
        } : param
      )
    });
  };
  
  // Aggiunge un nuovo select-options
  const handleAddSelectOption = () => {
    const newSelectOpt = { 
      id: idCounter.selectOptions, 
      name: `s_selopt${idCounter.selectOptions}`, 
      type: 'string', 
      label: `Select Option ${idCounter.selectOptions}`,
      multiple: true,
      noExtension: false,
      noIntervals: false,
      obligatory: false,
      matchcode: ''
    };
    
    setFormData({
      ...formData,
      selectOptions: [...formData.selectOptions, newSelectOpt]
    });
    
    setIdCounter({
      ...idCounter,
      selectOptions: idCounter.selectOptions + 1
    });
  };
  
  // Rimuove un select-options
  const handleRemoveSelectOption = (id) => {
    setFormData({
      ...formData,
      selectOptions: formData.selectOptions.filter(s => s.id !== id),
      // Aggiorna anche i blocchi per rimuovere i riferimenti al select-option rimosso
      blocks: formData.blocks.map(block => ({
        ...block,
        selectOptions: block.selectOptions.filter(selOptId => selOptId !== id)
      }))
    });
  };
  
  // Gestisce il cambiamento di un select-options
  const handleSelectOptionChange = (id, field, value, isCheckbox = false) => {
    setFormData({
      ...formData,
      selectOptions: formData.selectOptions.map(selOpt => 
        selOpt.id === id ? { 
          ...selOpt, 
          [field]: isCheckbox ? !selOpt[field] : value 
        } : selOpt
      )
    });
  };
  
  // Aggiunge un nuovo blocco
  const handleAddBlock = () => {
    const newBlock = {
      id: idCounter.blocks,
      title: `Blocco ${idCounter.blocks}`,
      frameTitle: `Parametri blocco ${idCounter.blocks}`,
      useFrame: true,
      parameters: [],
      selectOptions: []
    };
    
    setFormData({
      ...formData,
      blocks: [...formData.blocks, newBlock]
    });
    
    setIdCounter({
      ...idCounter,
      blocks: idCounter.blocks + 1
    });
    
    // Imposta automaticamente il nuovo blocco come attivo
    setActiveBlockId(idCounter.blocks);
  };
  
  // Rimuove un blocco
  const handleRemoveBlock = (id) => {
    // Se stiamo rimuovendo l'ultimo blocco, non permetterlo
    if (formData.blocks.length <= 1) {
      return;
    }
    
    setFormData({
      ...formData,
      blocks: formData.blocks.filter(b => b.id !== id)
    });
    
    // Se il blocco attivo è quello che stiamo rimuovendo, imposta il primo blocco come attivo
    if (activeBlockId === id) {
      const firstRemainingBlock = formData.blocks.find(b => b.id !== id);
      if (firstRemainingBlock) {
        setActiveBlockId(firstRemainingBlock.id);
      }
    }
  };
  
  // Gestisce il cambiamento di un blocco
  const handleBlockChange = (id, field, value, isCheckbox = false) => {
    setFormData({
      ...formData,
      blocks: formData.blocks.map(block => 
        block.id === id ? { 
          ...block, 
          [field]: isCheckbox ? !block[field] : value 
        } : block
      )
    });
  };
  
  // Aggiunge un parametro al blocco attivo
  const handleAddParameterToBlock = (paramId) => {
    const activeBlock = formData.blocks.find(b => b.id === activeBlockId);
    if (!activeBlock) return;
    
    // Aggiungi solo se non è già presente
    if (!activeBlock.parameters.includes(paramId)) {
      setFormData({
        ...formData,
        blocks: formData.blocks.map(block => 
          block.id === activeBlockId ? {
            ...block,
            parameters: [...block.parameters, paramId]
          } : block
        )
      });
    }
  };
  
  // Rimuove un parametro dal blocco attivo
  const handleRemoveParameterFromBlock = (paramId) => {
    setFormData({
      ...formData,
      blocks: formData.blocks.map(block => 
        block.id === activeBlockId ? {
          ...block,
          parameters: block.parameters.filter(id => id !== paramId)
        } : block
      )
    });
  };
  
  // Aggiunge un select-option al blocco attivo
  const handleAddSelectOptionToBlock = (selOptId) => {
    const activeBlock = formData.blocks.find(b => b.id === activeBlockId);
    if (!activeBlock) return;
    
    // Aggiungi solo se non è già presente
    if (!activeBlock.selectOptions.includes(selOptId)) {
      setFormData({
        ...formData,
        blocks: formData.blocks.map(block => 
          block.id === activeBlockId ? {
            ...block,
            selectOptions: [...block.selectOptions, selOptId]
          } : block
        )
      });
    }
  };
  
  // Rimuove un select-option dal blocco attivo
  const handleRemoveSelectOptionFromBlock = (selOptId) => {
    setFormData({
      ...formData,
      blocks: formData.blocks.map(block => 
        block.id === activeBlockId ? {
          ...block,
          selectOptions: block.selectOptions.filter(id => id !== selOptId)
        } : block
      )
    });
  };
  
  // Genera anteprima del codice
  const handleGeneratePreview = () => {
    // Implementazione della generazione dell'anteprima
    // ...
  };
  
  // Gestisce la generazione del codice finale
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('selection-screen', formData);
    }
  };
  
  // Ottiene l'oggetto blocco attivo
  const getActiveBlock = () => {
    return formData.blocks.find(b => b.id === activeBlockId) || formData.blocks[0];
  };
  
  // Ottieni i parametri disponibili (non ancora assegnati al blocco attivo)
  const getAvailableParameters = () => {
    const activeBlock = getActiveBlock();
    return formData.parameters.filter(param => !activeBlock.parameters.includes(param.id));
  };
  
  // Ottieni i select-options disponibili (non ancora assegnati al blocco attivo)
  const getAvailableSelectOptions = () => {
    const activeBlock = getActiveBlock();
    return formData.selectOptions.filter(selOpt => !activeBlock.selectOptions.includes(selOpt.id));
  };
  
  // Ottieni i parametri assegnati al blocco attivo
  const getAssignedParameters = () => {
    const activeBlock = getActiveBlock();
    return formData.parameters.filter(param => activeBlock.parameters.includes(param.id));
  };
  
  // Ottieni i select-options assegnati al blocco attivo
  const getAssignedSelectOptions = () => {
    const activeBlock = getActiveBlock();
    return formData.selectOptions.filter(selOpt => activeBlock.selectOptions.includes(selOpt.id));
  };
  
  return (
    <FormContainer>
      <FormGroup label="Titolo del Report:">
        <ControlledInput
          type="text"
          name="reportTitle"
          value={formData.reportTitle}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Descrizione:">
        <ControlledInput
          type="text"
          name="reportDescription"
          value={formData.reportDescription}
          onChange={handleChange}
        />
      </FormGroup>
      
      {/* Sezione Parametri */}
      <SectionHeader onClick={() => toggleSection('parameters')}>
        <h4>Parametri (PARAMETERS)</h4>
        {expandedSections.parameters ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.parameters && (
        <SectionContent>
          {formData.parameters.length === 0 ? (
            <EmptyState>Nessun parametro definito. Aggiungi un parametro con il pulsante sotto.</EmptyState>
          ) : (
            formData.parameters.map(param => (
              <ParameterItem key={param.id}>
                <ParameterHeader>
                  <ParameterTitle>{param.name} ({param.type})</ParameterTitle>
                  <Button
                    variant="text"
                    size="small"
                    icon={<FiTrash2 />}
                    onClick={() => handleRemoveParameter(param.id)}
                  />
                </ParameterHeader>
                
                <TwoColumnsGrid>
                  <FormGroup label="Nome:">
                    <ControlledInput
                      type="text"
                      value={param.name}
                      onChange={(e) => handleParameterChange(param.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Tipo:">
                    <ControlledInput
                      type="text"
                      value={param.type}
                      onChange={(e) => handleParameterChange(param.id, 'type', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Etichetta:">
                    <ControlledInput
                      type="text"
                      value={param.label}
                      onChange={(e) => handleParameterChange(param.id, 'label', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Valore default:">
                    <ControlledInput
                      type="text"
                      value={param.defaultValue}
                      onChange={(e) => handleParameterChange(param.id, 'defaultValue', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Matchcode:">
                    <ControlledInput
                      type="text"
                      value={param.matchcode}
                      onChange={(e) => handleParameterChange(param.id, 'matchcode', e.target.value)}
                      placeholder="Es. MAT1, F4_MATERIAL..."
                    />
                  </FormGroup>
                  
                  <FormGroup label="Lunghezza visualizzata:">
                    <ControlledInput
                      type="text"
                      value={param.visibleLength}
                      onChange={(e) => handleParameterChange(param.id, 'visibleLength', e.target.value)}
                      placeholder="Es. 30, LENGTH 40..."
                    />
                  </FormGroup>
                </TwoColumnsGrid>
                
                <OptionsFlex>
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={param.obligatory}
                      onChange={() => handleParameterChange(param.id, 'obligatory', null, true)}
                      id={`obligatory_${param.id}`}
                    />
                    <label htmlFor={`obligatory_${param.id}`}>Obbligatorio</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={param.lowerCase}
                      onChange={() => handleParameterChange(param.id, 'lowerCase', null, true)}
                      id={`lowercase_${param.id}`}
                    />
                    <label htmlFor={`lowercase_${param.id}`}>Lower Case</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={param.radioButton}
                      onChange={() => handleParameterChange(param.id, 'radioButton', null, true)}
                      id={`radiobutton_${param.id}`}
                    />
                    <label htmlFor={`radiobutton_${param.id}`}>Radio Button</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={param.checkBox}
                      onChange={() => handleParameterChange(param.id, 'checkBox', null, true)}
                      id={`checkbox_${param.id}`}
                    />
                    <label htmlFor={`checkbox_${param.id}`}>Check Box</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={param.listBox}
                      onChange={() => handleParameterChange(param.id, 'listBox', null, true)}
                      id={`listbox_${param.id}`}
                    />
                    <label htmlFor={`listbox_${param.id}`}>List Box</label>
                  </FormGroup>
                </OptionsFlex>
                
                {param.radioButton && (
                  <FormGroup label="Gruppo Radio Button:">
                    <ControlledInput
                      type="text"
                      value={param.radioGroup}
                      onChange={(e) => handleParameterChange(param.id, 'radioGroup', e.target.value)}
                      placeholder="Es. GROUP1"
                    />
                  </FormGroup>
                )}
              </ParameterItem>
            ))
          )}
          
          <Button
            variant="outline"
            size="small"
            icon={<FiPlus />}
            onClick={handleAddParameter}
          >
            Aggiungi Parametro
          </Button>
        </SectionContent>
      )}
      
      {/* Sezione Select-Options */}
      <SectionHeader onClick={() => toggleSection('selectOptions')}>
        <h4>Select-Options</h4>
        {expandedSections.selectOptions ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.selectOptions && (
        <SectionContent>
          {formData.selectOptions.length === 0 ? (
            <EmptyState>Nessun select-options definito. Aggiungi un select-options con il pulsante sotto.</EmptyState>
          ) : (
            formData.selectOptions.map(selOpt => (
              <ParameterItem key={selOpt.id}>
                <ParameterHeader>
                  <ParameterTitle>{selOpt.name} ({selOpt.type})</ParameterTitle>
                  <Button
                    variant="text"
                    size="small"
                    icon={<FiTrash2 />}
                    onClick={() => handleRemoveSelectOption(selOpt.id)}
                  />
                </ParameterHeader>
                
                <TwoColumnsGrid>
                  <FormGroup label="Nome:">
                    <ControlledInput
                      type="text"
                      value={selOpt.name}
                      onChange={(e) => handleSelectOptionChange(selOpt.id, 'name', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Tipo:">
                    <ControlledInput
                      type="text"
                      value={selOpt.type}
                      onChange={(e) => handleSelectOptionChange(selOpt.id, 'type', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Etichetta:">
                    <ControlledInput
                      type="text"
                      value={selOpt.label}
                      onChange={(e) => handleSelectOptionChange(selOpt.id, 'label', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup label="Matchcode:">
                    <ControlledInput
                      type="text"
                      value={selOpt.matchcode}
                      onChange={(e) => handleSelectOptionChange(selOpt.id, 'matchcode', e.target.value)}
                      placeholder="Es. MAT1, F4_MATERIAL..."
                    />
                  </FormGroup>
                </TwoColumnsGrid>
                
                <OptionsFlex>
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={selOpt.multiple}
                      onChange={() => handleSelectOptionChange(selOpt.id, 'multiple', null, true)}
                      id={`multiple_${selOpt.id}`}
                    />
                    <label htmlFor={`multiple_${selOpt.id}`}>Selezione Multipla</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={selOpt.noExtension}
                      onChange={() => handleSelectOptionChange(selOpt.id, 'noExtension', null, true)}
                      id={`noextension_${selOpt.id}`}
                    />
                    <label htmlFor={`noextension_${selOpt.id}`}>No Extension</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={selOpt.noIntervals}
                      onChange={() => handleSelectOptionChange(selOpt.id, 'noIntervals', null, true)}
                      id={`nointervals_${selOpt.id}`}
                    />
                    <label htmlFor={`nointervals_${selOpt.id}`}>No Intervals</label>
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={selOpt.obligatory}
                      onChange={() => handleSelectOptionChange(selOpt.id, 'obligatory', null, true)}
                      id={`oblig_${selOpt.id}`}
                    />
                    <label htmlFor={`oblig_${selOpt.id}`}>Obbligatorio</label>
                  </FormGroup>
                </OptionsFlex>
              </ParameterItem>
            ))
          )}
          
          <Button
            variant="outline"
            size="small"
            icon={<FiPlus />}
            onClick={handleAddSelectOption}
          >
            Aggiungi Select-Options
          </Button>
        </SectionContent>
      )}
      
      {/* Sezione Blocchi */}
      <SectionHeader onClick={() => toggleSection('blocks')}>
        <h4>Blocchi di Selection Screen</h4>
        {expandedSections.blocks ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.blocks && (
        <SectionContent>
          <BlocksTabs>
            {formData.blocks.map(block => (
              <BlockTab 
                key={block.id}
                active={activeBlockId === block.id}
                onClick={() => setActiveBlockId(block.id)}
              >
                {block.title}
              </BlockTab>
            ))}
            <Button
              variant="text"
              size="small"
              icon={<FiPlus />}
              onClick={handleAddBlock}
            >
              Nuovo
            </Button>
          </BlocksTabs>
          
          {formData.blocks.length > 0 && (
            <BlockContent>
              <BlockHeader>
                <BlockOptions>
                  <FormGroup label="Titolo del blocco:">
                    <ControlledInput
                      type="text"
                      value={getActiveBlock().title}
                      onChange={(e) => handleBlockChange(activeBlockId, 'title', e.target.value)}
                    />
                  </FormGroup>
                  
                  <FormGroup inline>
                    <input
                      type="checkbox"
                      checked={getActiveBlock().useFrame}
                      onChange={() => handleBlockChange(activeBlockId, 'useFrame', null, true)}
                      id={`useframe_${activeBlockId}`}
                    />
                    <label htmlFor={`useframe_${activeBlockId}`}>Usa Frame</label>
                  </FormGroup>
                  
                  {getActiveBlock().useFrame && (
                    <FormGroup label="Titolo del frame:">
                      <ControlledInput
                        type="text"
                        value={getActiveBlock().frameTitle}
                        onChange={(e) => handleBlockChange(activeBlockId, 'frameTitle', e.target.value)}
                      />
                    </FormGroup>
                  )}
                  
                  <Button
                    variant="text"
                    size="small"
                    icon={<FiTrash2 />}
                    onClick={() => handleRemoveBlock(activeBlockId)}
                    disabled={formData.blocks.length <= 1}
                  >
                    Rimuovi Blocco
                  </Button>
                </BlockOptions>
                
                <BlockItemsContainer>
                  <BlockColumn>
                    <h5>Parametri Disponibili</h5>
                    {getAvailableParameters().length === 0 ? (
                      <EmptyBlockState>Tutti i parametri sono già assegnati a questo blocco</EmptyBlockState>
                    ) : (
                      getAvailableParameters().map(param => (
                        <BlockItemAvailable key={param.id} onClick={() => handleAddParameterToBlock(param.id)}>
                          {param.name} <small>({param.type})</small>
                        </BlockItemAvailable>
                      ))
                    )}
                  </BlockColumn>
                  
                  <BlockColumn>
                    <h5>Parametri nel Blocco</h5>
                    {getAssignedParameters().length === 0 ? (
                      <EmptyBlockState>Nessun parametro assegnato a questo blocco</EmptyBlockState>
                    ) : (
                      getAssignedParameters().map(param => (
                        <BlockItemAssigned key={param.id}>
                          {param.name} <small>({param.type})</small>
                          <button onClick={() => handleRemoveParameterFromBlock(param.id)}>
                            <FiTrash2 size={14} />
                          </button>
                        </BlockItemAssigned>
                      ))
                    )}
                  </BlockColumn>
                </BlockItemsContainer>
                
                <BlockItemsContainer>
                  <BlockColumn>
                    <h5>Select-Options Disponibili</h5>
                    {getAvailableSelectOptions().length === 0 ? (
                      <EmptyBlockState>Tutti i select-options sono già assegnati a questo blocco</EmptyBlockState>
                    ) : (
                      getAvailableSelectOptions().map(selOpt => (
                        <BlockItemAvailable key={selOpt.id} onClick={() => handleAddSelectOptionToBlock(selOpt.id)}>
                          {selOpt.name} <small>({selOpt.type})</small>
                        </BlockItemAvailable>
                      ))
                    )}
                  </BlockColumn>
                  
                  <BlockColumn>
                    <h5>Select-Options nel Blocco</h5>
                    {getAssignedSelectOptions().length === 0 ? (
                      <EmptyBlockState>Nessun select-options assegnato a questo blocco</EmptyBlockState>
                    ) : (
                      getAssignedSelectOptions().map(selOpt => (
                        <BlockItemAssigned key={selOpt.id}>
                          {selOpt.name} <small>({selOpt.type})</small>
                          <button onClick={() => handleRemoveSelectOptionFromBlock(selOpt.id)}>
                            <FiTrash2 size={14} />
                          </button>
                        </BlockItemAssigned>
                      ))
                    )}
                  </BlockColumn>
                </BlockItemsContainer>
              </BlockHeader>
            </BlockContent>
          )}
        </SectionContent>
      )}
      
      {/* Opzioni Aggiuntive */}
      <SectionHeader onClick={() => toggleSection('options')}>
        <h4>Opzioni Aggiuntive</h4>
        {expandedSections.options ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.options && (
        <SectionContent>
          <OptionsFlex>
            <FormGroup inline>
              <input
                type="checkbox"
                name="includeVariants"
                checked={formData.includeVariants}
                onChange={handleChange}
                id="includeVariants"
              />
              <label htmlFor="includeVariants">Includi supporto Varianti</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="useSelectionScreen"
                checked={formData.useSelectionScreen}
                onChange={handleChange}
                id="useSelectionScreen"
              />
              <label htmlFor="useSelectionScreen">Usa SELECTION-SCREEN</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="checkbox"
                name="addAtSelection"
                checked={formData.addAtSelection}
                onChange={handleChange}
                id="addAtSelection"
              />
              <label htmlFor="addAtSelection">Aggiungi AT SELECTION-SCREEN</label>
            </FormGroup>
          </OptionsFlex>
          
          {formData.addAtSelection && (
            <FormGroup label="Codice AT SELECTION-SCREEN:">
              <ControlledTextarea
                name="atSelectionText"
                value={formData.atSelectionText}
                onChange={handleChange}
                rows={4}
                placeholder="Inserisci il codice per la gestione AT SELECTION-SCREEN"
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

const OptionsFlex = styled.div`
  display: flex;
  flex-wrap: wrap;
  gap: 20px;
  margin: 10px 0;
`;

const ParameterItem = styled.div`
  background: white;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 15px;
  transition: box-shadow 0.2s;
  
  &:hover {
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }
`;

const ParameterHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
`;

const ParameterTitle = styled.h5`
  margin: 0;
  font-size: 15px;
  color: #333;
  font-weight: 600;
`;

const EmptyState = styled.div`
  padding: 20px;
  text-align: center;
  background: #f0f0f0;
  border-radius: 4px;
  color: #666;
  margin-bottom: 15px;
`;

const BlocksTabs = styled.div`
  display: flex;
  overflow-x: auto;
  gap: 5px;
  margin-bottom: 15px;
  padding-bottom: 5px;
  border-bottom: 1px solid #ddd;
`;

const BlockTab = styled.div`
  padding: 8px 12px;
  background: ${props => props.active ? '#0066cc' : '#f0f0f0'};
  color: ${props => props.active ? 'white' : '#333'};
  border-radius: 4px;
  cursor: pointer;
  white-space: nowrap;
  
  &:hover {
    background: ${props => props.active ? '#0055aa' : '#e0e0e0'};
  }
`;

const BlockContent = styled.div`
  background: white;
  border: 1px solid #ddd;
  border-radius: 6px;
  padding: 15px;
`;

const BlockHeader = styled.div`
  display: flex;
  flex-direction: column;
  gap: 15px;
`;

const BlockOptions = styled.div`
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 15px;
  align-items: center;
  
  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
`;

const BlockItemsContainer = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 20px;
  margin-top: 10px;
  
  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
`;

const BlockColumn = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 10px;
  
  h5 {
    margin: 0 0 10px 0;
    font-size: 14px;
    color: #555;
    text-align: center;
  }
`;

const BlockItemAvailable = styled.div`
  padding: 8px 10px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  margin-bottom: 5px;
  cursor: pointer;
  transition: background-color 0.2s;
  
  &:hover {
    background: #e6f7ff;
    border-color: #1890ff;
  }
  
  small {
    color: #888;
  }
`;

const BlockItemAssigned = styled.div`
  padding: 8px 10px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  margin-bottom: 5px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  
  small {
    color: #888;
  }
  
  button {
    background: none;
    border: none;
    color: #ff4d4f;
    cursor: pointer;
    padding: 2px;
    opacity: 0.7;
    
    &:hover {
      opacity: 1;
    }
  }
`;

const EmptyBlockState = styled.div`
  padding: 15px;
  text-align: center;
  color: #999;
  font-style: italic;
  font-size: 13px;
  background: white;
  border: 1px dashed #ddd;
  border-radius: 4px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default SelectionScreenForm;