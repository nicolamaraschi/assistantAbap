import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiEye, FiChevronDown, FiChevronUp } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Componente migliorato per il form LOOP AT
const LoopAtForm = ({ onGenerate }) => {
  // Stato locale del form con tutte le nuove opzioni
  const [formData, setFormData] = useState({
    table: 'lt_table',
    variable: 'ls_line',
    whereCondition: '',
    content: 'WRITE: / ls_line-field.',
    useAssigning: false,
    addIndex: false,
    indexFrom: '1',
    indexTo: '10',
    useGroupBy: false,
    groupByField: 'field',
    groupByOrder: 'ASCENDING',
    useReferenceInto: false,
    useTableMode: false,        // Nuovo: LOOP AT IN TABLE MODE
    useRange: false,            // Nuovo: LOOP AT con RANGE
    rangeVariable: 'lr_range',
    useFilter: false,           // Nuovo: LOOP AT con FILTER CONDITIONS
    filterCondition: 'field1 = lv_value1 and field2 = lv_value2',
    useTransporting: false,     // Nuovo: LOOP AT con TRANSPORTING
    transportingFields: 'field1 field2 field3',
    useNested: false,           // Nuovo: supporto per tabelle annidate
    nestedTable: 'ls_line-nested_table',
    nestedVariable: 'ls_nested',
    nestedContent: 'WRITE: / ls_nested-field.',
    useUpTo: false,             // Nuovo: UP TO n ROWS
    upToRows: '100',
  });
  
  // Stato per le sezioni collassabili
  const [expandedSections, setExpandedSections] = useState({
    basic: true,
    advanced: false,
    filtering: false,
    grouping: false,
    nested: false,
    preview: false
  });
  
  // Anteprima del codice generato
  const [previewCode, setPreviewCode] = useState('');
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['loop-at']) {
      setFormData(formState['loop-at']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('loop-at', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
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
  
  // Genera anteprima del codice
  const handlePreview = () => {
    const code = generateLoopAtCode(formData);
    setPreviewCode(code);
    setExpandedSections({
      ...expandedSections,
      preview: true
    });
  };
  
  // Genera il codice LOOP AT
  const generateLoopAtCode = (data) => {
    let code = `LOOP AT ${data.table}`;
    
    // Aggiungi IN TABLE MODE se selezionato
    if (data.useTableMode) {
      code += ` IN TABLE MODE`;
    }
    
    // Aggiungi FILTER se selezionato
    if (data.useFilter && data.filterCondition) {
      code += ` FILTER ( ${data.filterCondition} )`;
    }
    
    // Aggiungi RANGE se selezionato
    if (data.useRange && data.rangeVariable) {
      code += ` IN ${data.rangeVariable}`;
    }
    
    // Aggiungi WHERE se presente
    if (data.whereCondition && !data.useFilter && !data.useRange) {
      code += ` WHERE ${data.whereCondition}`;
    }
    
    // Aggiungi GROUP BY se richiesto
    if (data.useGroupBy && data.groupByField) {
      code += `\n  GROUP BY ${data.groupByField}`;
      if (data.groupByOrder) {
        code += `\n  ${data.groupByOrder}`;
      }
    }
    
    // Scegli tra INTO, REFERENCE INTO o ASSIGNING
    if (data.useAssigning) {
      code += ` ASSIGNING FIELD-SYMBOL(<${data.variable}>)`;
    } else if (data.useReferenceInto) {
      code += ` REFERENCE INTO DATA(${data.variable})`;
    } else {
      code += ` INTO ${data.variable}`;
    }
    
    // Aggiungi TRANSPORTING se richiesto
    if (data.useTransporting && data.transportingFields) {
      code += `\n  TRANSPORTING ${data.transportingFields}`;
    }
    
    // Aggiungi FROM/TO se richiesto
    if (data.addIndex) {
      code += ` FROM ${data.indexFrom} TO ${data.indexTo}`;
    }
    
    // Aggiungi UP TO n ROWS se richiesto
    if (data.useUpTo && data.upToRows) {
      code += `\n  UP TO ${data.upToRows} ROWS`;
    }
    
    code += `.`;
    
    // Aggiungi il contenuto del loop
    code += `\n  ${data.content}`;
    
    // Aggiungi un loop annidato se richiesto
    if (data.useNested && data.nestedTable && data.nestedVariable) {
      code += `\n\n  LOOP AT ${data.nestedTable} INTO ${data.nestedVariable}.`;
      code += `\n    ${data.nestedContent}`;
      code += `\n  ENDLOOP.`;
    }
    
    code += `\nENDLOOP.`;
    
    return code;
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('loop-at', formData);
    }
  };
  
  return (
    <FormContainer>
      {/* Sezione base */}
      <SectionHeader onClick={() => toggleSection('basic')}>
        <h4>Informazioni Base</h4>
        {expandedSections.basic ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.basic && (
        <SectionContent>
          <FormGroup label="Tabella:">
            <ControlledInput
              type="text"
              name="table"
              value={formData.table}
              onChange={handleChange}
              placeholder="es. lt_table"
            />
          </FormGroup>
          
          <AdvancedSection>
            <h5>Tipo di Loop</h5>
            
            <FormGroup inline>
              <input
                type="radio"
                name="loopType"
                id="loopInto"
                checked={!formData.useAssigning && !formData.useReferenceInto}
                onChange={() => setFormData({
                  ...formData,
                  useAssigning: false,
                  useReferenceInto: false
                })}
              />
              <label htmlFor="loopInto">INTO</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="radio"
                name="loopType"
                id="loopAssigning"
                checked={formData.useAssigning}
                onChange={() => setFormData({
                  ...formData,
                  useAssigning: true,
                  useReferenceInto: false
                })}
              />
              <label htmlFor="loopAssigning">ASSIGNING Field-Symbol</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="radio"
                name="loopType"
                id="loopReference"
                checked={formData.useReferenceInto}
                onChange={() => setFormData({
                  ...formData,
                  useAssigning: false,
                  useReferenceInto: true
                })}
              />
              <label htmlFor="loopReference">REFERENCE INTO</label>
            </FormGroup>
          </AdvancedSection>
          
          <FormGroup label="Variabile di destinazione:">
            <ControlledInput
              type="text"
              name="variable"
              value={formData.variable}
              onChange={handleChange}
              placeholder={formData.useAssigning ? "Nome field-symbol" : (formData.useReferenceInto ? "Nome reference" : "Nome variabile")}
            />
          </FormGroup>
          
          <FormGroup label="Contenuto del loop:">
            <ControlledTextarea
              name="content"
              value={formData.content}
              onChange={handleChange}
              rows={4}
            />
          </FormGroup>
        </SectionContent>
      )}
      
      {/* Sezione Filtri */}
      <SectionHeader onClick={() => toggleSection('filtering')}>
        <h4>Opzioni di Filtro</h4>
        {expandedSections.filtering ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.filtering && (
        <SectionContent>
          <FilterOptions>
            <FormGroup inline>
              <input
                type="radio"
                name="filterType"
                id="filterNone"
                checked={!formData.whereCondition && !formData.useFilter && !formData.useRange}
                onChange={() => setFormData({
                  ...formData,
                  whereCondition: '',
                  useFilter: false,
                  useRange: false
                })}
              />
              <label htmlFor="filterNone">Nessun filtro</label>
            </FormGroup>
            
            <FormGroup inline>
              <input
                type="radio"
                name="filterType"
                id="filterWhere"
                checked={!!formData.whereCondition && !formData.useFilter && !formData.useRange}
                onChange={() => setFormData({
                  ...formData,
                  useFilter: false,
                  useRange: false
                })}
              />
              <label htmlFor="filterWhere">Usa WHERE</label>
            </FormGroup>
            
            {!formData.useFilter && !formData.useRange && !!formData.whereCondition && (
              <FormGroup label="Condizione WHERE:">
                <ControlledInput
                  type="text"
                  name="whereCondition"
                  value={formData.whereCondition}
                  onChange={handleChange}
                  placeholder="es. field = 'value'"
                />
              </FormGroup>
            )}
            
            <FormGroup inline>
              <input
                type="radio"
                name="filterType"
                id="filterFilter"
                checked={formData.useFilter}
                onChange={() => setFormData({
                  ...formData,
                  useFilter: true,
                  useRange: false,
                  whereCondition: ''
                })}
              />
              <label htmlFor="filterFilter">Usa FILTER CONDITIONS</label>
            </FormGroup>
            
            {formData.useFilter && (
              <FormGroup label="Condizione FILTER:">
                <ControlledInput
                  type="text"
                  name="filterCondition"
                  value={formData.filterCondition}
                  onChange={handleChange}
                  placeholder="es. field1 = lv_value1 and field2 = lv_value2"
                />
              </FormGroup>
            )}
            
            <FormGroup inline>
              <input
                type="radio"
                name="filterType"
                id="filterRange"
                checked={formData.useRange}
                onChange={() => setFormData({
                  ...formData,
                  useFilter: false,
                  useRange: true,
                  whereCondition: ''
                })}
              />
              <label htmlFor="filterRange">Usa RANGE</label>
            </FormGroup>
            
            {formData.useRange && (
              <FormGroup label="Variabile RANGE:">
                <ControlledInput
                  type="text"
                  name="rangeVariable"
                  value={formData.rangeVariable}
                  onChange={handleChange}
                  placeholder="es. lr_range"
                />
              </FormGroup>
            )}
          </FilterOptions>
        </SectionContent>
      )}
      
      {/* Sezione Raggruppamento */}
      <SectionHeader onClick={() => toggleSection('grouping')}>
        <h4>Raggruppamento e Ordinamento</h4>
        {expandedSections.grouping ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.grouping && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useGroupBy"
              checked={formData.useGroupBy}
              onChange={handleChange}
              id="useGroupBy"
            />
            <label htmlFor="useGroupBy">Usa GROUP BY</label>
          </FormGroup>
          
          {formData.useGroupBy && (
            <>
              <FormGroup label="Campo di raggruppamento:">
                <ControlledInput
                  type="text"
                  name="groupByField"
                  value={formData.groupByField}
                  onChange={handleChange}
                  placeholder="es. field"
                />
              </FormGroup>
              
              <FormGroup label="Ordinamento:">
                <select
                  name="groupByOrder"
                  value={formData.groupByOrder}
                  onChange={handleChange}
                >
                  <option value="ASCENDING">ASCENDING</option>
                  <option value="DESCENDING">DESCENDING</option>
                </select>
              </FormGroup>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Sezione Avanzata */}
      <SectionHeader onClick={() => toggleSection('advanced')}>
        <h4>Opzioni Avanzate</h4>
        {expandedSections.advanced ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.advanced && (
        <SectionContent>
          <TwoColumnsSection>
            <div>
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="addIndex"
                  checked={formData.addIndex}
                  onChange={handleChange}
                  id="addIndex"
                />
                <label htmlFor="addIndex">Limita intervallo (FROM/TO)</label>
              </FormGroup>
              
              {formData.addIndex && (
                <IndexRange>
                  <FormGroup label="Da:">
                    <ControlledInput
                      type="text"
                      name="indexFrom"
                      value={formData.indexFrom}
                      onChange={handleChange}
                    />
                  </FormGroup>
                  
                  <FormGroup label="A:">
                    <ControlledInput
                      type="text"
                      name="indexTo"
                      value={formData.indexTo}
                      onChange={handleChange}
                    />
                  </FormGroup>
                </IndexRange>
              )}
              
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="useUpTo"
                  checked={formData.useUpTo}
                  onChange={handleChange}
                  id="useUpTo"
                />
                <label htmlFor="useUpTo">Limita numero di righe (UP TO)</label>
              </FormGroup>
              
              {formData.useUpTo && (
                <FormGroup label="Numero massimo di righe:">
                  <ControlledInput
                    type="text"
                    name="upToRows"
                    value={formData.upToRows}
                    onChange={handleChange}
                    placeholder="es. 100"
                  />
                </FormGroup>
              )}
            </div>
            
            <div>
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="useTableMode"
                  checked={formData.useTableMode}
                  onChange={handleChange}
                  id="useTableMode"
                />
                <label htmlFor="useTableMode">Usa TABLE MODE</label>
              </FormGroup>
              
              <FormGroup inline>
                <input
                  type="checkbox"
                  name="useTransporting"
                  checked={formData.useTransporting}
                  onChange={handleChange}
                  id="useTransporting"
                />
                <label htmlFor="useTransporting">Usa TRANSPORTING</label>
              </FormGroup>
              
              {formData.useTransporting && (
                <FormGroup label="Campi da trasportare:">
                  <ControlledInput
                    type="text"
                    name="transportingFields"
                    value={formData.transportingFields}
                    onChange={handleChange}
                    placeholder="es. field1 field2 field3"
                  />
                </FormGroup>
              )}
            </div>
          </TwoColumnsSection>
        </SectionContent>
      )}
      
      {/* Sezione Tabelle Annidate */}
      <SectionHeader onClick={() => toggleSection('nested')}>
        <h4>Gestione Tabelle Annidate</h4>
        {expandedSections.nested ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.nested && (
        <SectionContent>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useNested"
              checked={formData.useNested}
              onChange={handleChange}
              id="useNested"
            />
            <label htmlFor="useNested">Aggiungi loop annidato</label>
          </FormGroup>
          
          {formData.useNested && (
            <>
              <FormGroup label="Tabella anndiata:">
                <ControlledInput
                  type="text"
                  name="nestedTable"
                  value={formData.nestedTable}
                  onChange={handleChange}
                  placeholder="es. ls_line-nested_table"
                />
              </FormGroup>
              
              <FormGroup label="Variabile per il loop annidato:">
                <ControlledInput
                  type="text"
                  name="nestedVariable"
                  value={formData.nestedVariable}
                  onChange={handleChange}
                  placeholder="es. ls_nested"
                />
              </FormGroup>
              
              <FormGroup label="Contenuto del loop annidato:">
                <ControlledTextarea
                  name="nestedContent"
                  value={formData.nestedContent}
                  onChange={handleChange}
                  rows={3}
                />
              </FormGroup>
            </>
          )}
        </SectionContent>
      )}
      
      {/* Anteprima del codice */}
      <SectionHeader onClick={() => toggleSection('preview')}>
        <h4>Anteprima Codice</h4>
        {expandedSections.preview ? <FiChevronUp /> : <FiChevronDown />}
      </SectionHeader>
      
      {expandedSections.preview && (
        <SectionContent>
          <PreviewButton onClick={handlePreview}>
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

const TwoColumnsSection = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 20px;
  
  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
`;

const AdvancedSection = styled.div`
  background: #fff;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 12px;
  margin-bottom: 12px;
  
  h5 {
    margin-top: 0;
    margin-bottom: 10px;
    font-size: 14px;
    color: #444;
  }
`;

const FilterOptions = styled.div`
  display: flex;
  flex-direction: column;
  gap: 10px;
  
  input[type="radio"] {
    margin-right: 8px;
  }
`;

const IndexRange = styled.div`
  display: flex;
  gap: 15px;
  
  > div {
    flex: 1;
  }
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

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

export default LoopAtForm;