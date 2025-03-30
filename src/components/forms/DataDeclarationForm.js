import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2, FiCopy } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Tipi di dati ABAP comuni
const ABAP_DATA_TYPES = [
  { 
    category: 'Tipi Elementari', 
    types: [
      { id: 'STRING', name: 'STRING', description: 'Stringa di lunghezza variabile' },
      { id: 'XSTRING', name: 'XSTRING', description: 'Stringa binaria di lunghezza variabile' },
      { id: 'I', name: 'I (INTEGER)', description: 'Numero intero (4 byte)' },
      { id: 'INT4', name: 'INT4', description: 'Numero intero (4 byte)' },
      { id: 'INT8', name: 'INT8', description: 'Numero intero (8 byte)' },
      { id: 'INT1', name: 'INT1', description: 'Numero intero (1 byte)' },
      { id: 'INT2', name: 'INT2', description: 'Numero intero (2 byte)' },
      { id: 'F', name: 'F (FLOAT)', description: 'Numero a virgola mobile (8 byte)' },
      { id: 'D', name: 'D (DATE)', description: 'Data in formato AAAAMMGG' },
      { id: 'T', name: 'T (TIME)', description: 'Ora in formato HHMMSS' },
      { id: 'DECFLOAT16', name: 'DECFLOAT16', description: 'Decimale in virgola mobile (8 byte)' },
      { id: 'DECFLOAT34', name: 'DECFLOAT34', description: 'Decimale in virgola mobile (16 byte)' },
      { id: 'C', name: 'C (CHAR)', description: 'Stringa di caratteri a lunghezza fissa' },
      { id: 'N', name: 'N (NUMERIC)', description: 'Caratteri numerici a lunghezza fissa' },
      { id: 'X', name: 'X (HEXADECIMAL)', description: 'Dati binari a lunghezza fissa' },
      { id: 'P', name: 'P (PACKED)', description: 'Numero decimale impaccato a lunghezza fissa' }
    ]
  },
  { 
    category: 'Tipi di Riferimento', 
    types: [
      { id: 'DATA', name: 'DATA', description: 'Riferimento a un oggetto dati' },
      { id: 'OBJECT', name: 'OBJECT', description: 'Riferimento a un oggetto generico' },
      { id: 'REF TO', name: 'REF TO {classe}', description: 'Riferimento a un oggetto di classe specifica' },
      { id: 'REF TO DATA', name: 'REF TO DATA', description: 'Riferimento a un oggetto dati' },
      { id: 'REF TO OBJECT', name: 'REF TO OBJECT', description: 'Riferimento a un oggetto generico' }
    ]
  },
  { 
    category: 'Dichiarazioni Complesse', 
    types: [
      { id: 'STANDARD TABLE', name: 'STANDARD TABLE OF', description: 'Tabella standard' },
      { id: 'SORTED TABLE', name: 'SORTED TABLE OF', description: 'Tabella ordinata' },
      { id: 'HASHED TABLE', name: 'HASHED TABLE OF', description: 'Tabella con hash' },
      { id: 'RANGE', name: 'RANGE OF', description: 'Range di valori per selezione' },
      { id: 'LINE OF', name: 'LINE OF', description: 'Linea di una tabella' },
      { id: 'TYPE LINE OF', name: 'TYPE LINE OF', description: 'Tipo linea di una tabella' },
      { id: 'LIKE LINE OF', name: 'LIKE LINE OF', description: 'Come linea di una tabella' }
    ]
  },
  { 
    category: 'Tipi SAP', 
    types: [
      { id: 'DDIC', name: 'Tipo DDIC', description: 'Usa tipo del Dictionary' },
      { id: 'STRUCTURE', name: 'Structure (es. SFLIGHT)', description: 'Struttura definita nel Dictionary' },
      { id: 'TABLE', name: 'Table Type (es. SFLIGHT_TAB)', description: 'Tipo tabella definito nel Dictionary' }
    ]
  }
];

// Valori iniziali comuni
const COMMON_INITIAL_VALUES = [
  { value: 'space', display: 'SPACE (stringa vuota)' },
  { value: 'abap_true', display: 'ABAP_TRUE (X)' },
  { value: 'abap_false', display: 'ABAP_FALSE (spazio)' },
  { value: '0', display: '0 (zero)' },
  { value: 'sy-datum', display: 'SY-DATUM (data di sistema)' },
  { value: 'sy-uzeit', display: 'SY-UZEIT (ora di sistema)' },
  { value: 'sy-uname', display: 'SY-UNAME (nome utente)' },
  { value: 'sy-langu', display: 'SY-LANGU (lingua utente)' },
  { value: 'sy-mandt', display: 'SY-MANDT (client)' },
  { value: 'value #( )', display: 'VALUE #( ) (costruttore vuoto)' },
  { value: 'initial', display: 'IS INITIAL (valore iniziale)' }
];

// Prefissi comuni per variabili
const COMMON_PREFIXES = [
  { value: 'lv_', display: 'lv_ (variabile locale)' },
  { value: 'lt_', display: 'lt_ (tabella locale)' },
  { value: 'ls_', display: 'ls_ (struttura locale)' },
  { value: 'lr_', display: 'lr_ (riferimento locale)' },
  { value: 'lo_', display: 'lo_ (oggetto locale)' },
  { value: 'gv_', display: 'gv_ (variabile globale)' },
  { value: 'gt_', display: 'gt_ (tabella globale)' },
  { value: 'gs_', display: 'gs_ (struttura globale)' },
  { value: 'gr_', display: 'gr_ (riferimento globale)' },
  { value: 'go_', display: 'go_ (oggetto globale)' },
  { value: 'mv_', display: 'mv_ (attributo classe)' },
  { value: 'mt_', display: 'mt_ (tabella attributo classe)' },
  { value: 'ms_', display: 'ms_ (struttura attributo classe)' },
  { value: 'mr_', display: 'mr_ (riferimento attributo classe)' },
  { value: 'mo_', display: 'mo_ (oggetto attributo classe)' }
];

const DataDeclarationForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    variables: [
      { id: 1, name: 'lv_text', prefix: 'lv_', basename: 'text', type: 'STRING', 
        initialValue: '', isReference: false, length: '', decimals: '',
        isTypeComplete: true, typeSuffix: '', isStruct: false, isTable: false,
        tableType: 'STANDARD TABLE', keyType: 'DEFAULT KEY', tableOf: '' }
    ],
    useTypes: false,
    types: []
  });
  
  // Stato per filtrare i tipi
  const [typeFilter, setTypeFilter] = useState('');
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['data-declaration']) {
      setFormData(formState['data-declaration']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('data-declaration', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento delle variabili
  const handleVariableChange = (id, field, value) => {
    setFormData(prevData => {
      const updatedVariables = prevData.variables.map(v => {
        if (v.id === id) {
          const updatedVar = { ...v, [field]: value };
          
          // Gestione speciale per il nome: aggiorna prefix e basename
          if (field === 'name') {
            // Estrai il prefisso dal nome (prime lettere fino all'underscore)
            const prefixMatch = value.match(/^([a-z]+_)/);
            if (prefixMatch) {
              updatedVar.prefix = prefixMatch[1];
              updatedVar.basename = value.substring(prefixMatch[1].length);
            } else {
              updatedVar.prefix = '';
              updatedVar.basename = value;
            }
          }
          
          // Aggiornamento combinato prefix + basename
          if (field === 'prefix' || field === 'basename') {
            updatedVar.name = `${field === 'prefix' ? value : updatedVar.prefix}${field === 'basename' ? value : updatedVar.basename}`;
          }
          
          // Gestione per type che aggiorna anche isTable e isStruct
          if (field === 'type') {
            // Aggiorna isReference
            updatedVar.isReference = value.startsWith('REF TO');
            
            // Aggiorna isTable e isStruct
            updatedVar.isTable = value.includes('TABLE OF');
            updatedVar.isStruct = !updatedVar.isTable && !isPrimitiveType(value);
            
            // Estrai tableOf dal tipo se è una tabella
            if (updatedVar.isTable) {
              const tableOfMatch = value.match(/TABLE OF\s+(.+)/i);
              if (tableOfMatch) {
                updatedVar.tableOf = tableOfMatch[1].trim();
              }
            }
            
            // Aggiorna isTypeComplete
            updatedVar.isTypeComplete = isPrimitiveType(value) || 
                                      !value.includes('{') || 
                                      (value.includes('{') && value.includes('}'));
          }
          
          // Aggiorna isReference direttamente
          if (field === 'isReference') {
            if (value) {
              if (!updatedVar.type.startsWith('REF TO')) {
                updatedVar.type = `REF TO ${updatedVar.type}`;
              }
            } else {
              if (updatedVar.type.startsWith('REF TO')) {
                updatedVar.type = updatedVar.type.replace('REF TO ', '');
              }
            }
          }
          
          return updatedVar;
        }
        return v;
      });
      
      return { ...prevData, variables: updatedVariables };
    });
  };
  
  // Verifica se un tipo è primitivo
  const isPrimitiveType = (type) => {
    const primitiveTypes = [
      'STRING', 'XSTRING', 'I', 'INT4', 'INT8', 'INT1', 'INT2', 'F', 'D', 'T',
      'DECFLOAT16', 'DECFLOAT34', 'C', 'N', 'X', 'P', 'CHAR', 'NUMC'
    ];
    
    return primitiveTypes.some(primType => 
      type === primType || 
      type.startsWith(`${primType}(`) ||
      type.startsWith(`${primType} `)
    );
  };
  
  // Aggiunge una nuova variabile
  const handleAddVariable = () => {
    const newId = Math.max(0, ...formData.variables.map(v => v.id)) + 1;
    
    // Determina il prefisso migliore in base al tipo di variabile
    const defaultPrefix = 'lv_';
    
    setFormData({
      ...formData,
      variables: [
        ...formData.variables,
        { 
          id: newId, 
          name: `${defaultPrefix}var${newId}`, 
          prefix: defaultPrefix,
          basename: `var${newId}`,
          type: 'STRING', 
          initialValue: '', 
          isReference: false,
          length: '',
          decimals: '',
          isTypeComplete: true,
          typeSuffix: '',
          isStruct: false,
          isTable: false,
          tableType: 'STANDARD TABLE',
          keyType: 'DEFAULT KEY',
          tableOf: ''
        }
      ]
    });
  };
  
  // Clona una variabile esistente
  const handleCloneVariable = (id) => {
    const varToClone = formData.variables.find(v => v.id === id);
    if (!varToClone) return;
    
    const newId = Math.max(0, ...formData.variables.map(v => v.id)) + 1;
    const clonedVar = {
      ...varToClone,
      id: newId,
      name: `${varToClone.prefix}${varToClone.basename}${newId}`,
      basename: `${varToClone.basename}${newId}`
    };
    
    setFormData({
      ...formData,
      variables: [...formData.variables, clonedVar]
    });
  };
  
  // Rimuove una variabile
  const handleRemoveVariable = (id) => {
    setFormData({
      ...formData,
      variables: formData.variables.filter(v => v.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('data-declaration', formData);
    }
  };
  
  // Filtra i tipi in base al campo di ricerca
  const getFilteredTypes = () => {
    if (!typeFilter) return ABAP_DATA_TYPES;
    
    return ABAP_DATA_TYPES.map(category => ({
      ...category,
      types: category.types.filter(type => 
        type.name.toLowerCase().includes(typeFilter.toLowerCase()) ||
        type.description.toLowerCase().includes(typeFilter.toLowerCase())
      )
    })).filter(category => category.types.length > 0);
  };
  
  // Ottieni l'elenco filtrato dei tipi
  const filteredTypes = getFilteredTypes();
  
  return (
    <FormContainer>
      <FormHeader>
        <h3>Dichiarazioni DATA</h3>
        <FormActions>
          <Button
            variant="outline"
            size="small"
            icon={<FiPlus />}
            onClick={handleAddVariable}
          >
            Aggiungi variabile
          </Button>
        </FormActions>
      </FormHeader>
      
      <VariablesContainer>
        {formData.variables.map((variable, index) => (
          <VariableItem key={variable.id}>
            <VariableHeader>
              <VariableTitle>Variabile {index + 1}</VariableTitle>
              <ButtonGroup>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiCopy />}
                  onClick={() => handleCloneVariable(variable.id)}
                  title="Clona variabile"
                />
                <Button
                  variant="text"
                  size="small"
                  icon={<FiTrash2 />}
                  onClick={() => handleRemoveVariable(variable.id)}
                  title="Rimuovi variabile"
                />
              </ButtonGroup>
            </VariableHeader>
            
            <VariableGrid>
              <FormGroup label="Nome:">
                <PrefixInput>
                  <PrefixSelect
                    value={variable.prefix}
                    onChange={(e) => handleVariableChange(variable.id, 'prefix', e.target.value)}
                  >
                    {COMMON_PREFIXES.map(prefix => (
                      <option key={prefix.value} value={prefix.value}>
                        {prefix.display}
                      </option>
                    ))}
                    <option value="">-- Nessun prefisso --</option>
                  </PrefixSelect>
                  <ControlledInput 
                    type="text"
                    value={variable.basename}
                    onChange={(e) => handleVariableChange(variable.id, 'basename', e.target.value)}
                    placeholder="Nome variabile"
                  />
                </PrefixInput>
              </FormGroup>
              
              <FormGroup label="Tipo:">
                <TypeSelector>
                  <TypeAutocomplete>
                    <ControlledInput 
                      type="text"
                      value={variable.type}
                      onChange={(e) => handleVariableChange(variable.id, 'type', e.target.value)}
                      placeholder="es. STRING, INT4, SFLIGHT..."
                    />
                    <TypeDropdownTrigger
                      onClick={() => setTypeFilter('')}
                      title="Mostra tipi comuni"
                    >
                      ▼
                    </TypeDropdownTrigger>
                  </TypeAutocomplete>
                  
                  <TypeDropdown>
                    <TypeSearchInput
                      type="text"
                      value={typeFilter}
                      onChange={(e) => setTypeFilter(e.target.value)}
                      placeholder="Cerca tipo..."
                    />
                    <TypeList>
                      {filteredTypes.map(category => (
                        <TypeCategory key={category.category}>
                          <TypeCategoryHeader>{category.category}</TypeCategoryHeader>
                          {category.types.map(type => (
                            <TypeItem 
                              key={type.id}
                              onClick={() => {
                                handleVariableChange(variable.id, 'type', type.id);
                                setTypeFilter('');
                              }}
                              title={type.description}
                            >
                              <TypeName>{type.name}</TypeName>
                              <TypeDescription>{type.description}</TypeDescription>
                            </TypeItem>
                          ))}
                        </TypeCategory>
                      ))}
                    </TypeList>
                  </TypeDropdown>
                </TypeSelector>
              </FormGroup>
              
              {!isPrimitiveType(variable.type) && !variable.isReference && !variable.isTable && (
                <FormGroup label="Tabella?">
                  <SelectRow>
                    <Select
                      value={variable.isTable ? variable.tableType : "NON_TABLE"}
                      onChange={(e) => {
                        const value = e.target.value;
                        if (value === "NON_TABLE") {
                          handleVariableChange(variable.id, 'isTable', false);
                        } else {
                          handleVariableChange(variable.id, 'isTable', true);
                          handleVariableChange(variable.id, 'tableType', value);
                          // Se è una tabella, aggiorna anche il tipo 
                          // (es. da SFLIGHT a STANDARD TABLE OF SFLIGHT)
                          handleVariableChange(
                            variable.id, 
                            'type', 
                            `${value} OF ${variable.type}`
                          );
                          // Aggiorna tableOf
                          handleVariableChange(variable.id, 'tableOf', variable.type);
                        }
                      }}
                    >
                      <option value="NON_TABLE">No (variabile semplice)</option>
                      <option value="STANDARD TABLE">STANDARD TABLE</option>
                      <option value="SORTED TABLE">SORTED TABLE</option>
                      <option value="HASHED TABLE">HASHED TABLE</option>
                    </Select>
                  </SelectRow>
                </FormGroup>
              )}
              
              {variable.isTable && (
                <FormGroup label="Chiave tabella:">
                  <Select
                    value={variable.keyType}
                    onChange={(e) => handleVariableChange(variable.id, 'keyType', e.target.value)}
                  >
                    <option value="DEFAULT KEY">DEFAULT KEY</option>
                    <option value="STANDARD KEY">STANDARD KEY</option>
                    <option value="EMPTY KEY">EMPTY KEY</option>
                    <option value="NON-UNIQUE KEY">NON-UNIQUE KEY</option>
                    <option value="UNIQUE KEY">UNIQUE KEY</option>
                  </Select>
                </FormGroup>
              )}
              
              {(variable.type === 'C' || variable.type === 'N' || variable.type === 'P' || variable.type === 'X') && (
                <FormGroup label={variable.type === 'P' ? "Lunghezza/decimali:" : "Lunghezza:"}>
                  <LengthContainer>
                    <ControlledInput 
                      type="text"
                      value={variable.length}
                      onChange={(e) => handleVariableChange(variable.id, 'length', e.target.value)}
                      placeholder="Lunghezza"
                    />
                    {variable.type === 'P' && (
                      <>
                        <span>/</span>
                        <ControlledInput 
                          type="text"
                          value={variable.decimals}
                          onChange={(e) => handleVariableChange(variable.id, 'decimals', e.target.value)}
                          placeholder="Decimali"
                        />
                      </>
                    )}
                  </LengthContainer>
                </FormGroup>
              )}
              
              <FormGroup label="Valore iniziale (opzionale):">
                <SelectRow>
                  <InitialValueSelect
                    value=""
                    onChange={(e) => {
                      if (e.target.value) {
                        handleVariableChange(variable.id, 'initialValue', e.target.value);
                      }
                    }}
                  >
                    <option value="">-- Seleziona o inserisci --</option>
                    {COMMON_INITIAL_VALUES.map(val => (
                      <option key={val.value} value={val.value}>
                        {val.display}
                      </option>
                    ))}
                  </InitialValueSelect>
                  <ControlledInput 
                    type="text"
                    value={variable.initialValue}
                    onChange={(e) => handleVariableChange(variable.id, 'initialValue', e.target.value)}
                    placeholder="Valore iniziale"
                  />
                </SelectRow>
              </FormGroup>
              
              <FormGroup inline>
                <Checkbox
                  type="checkbox"
                  id={`isReference-${variable.id}`}
                  checked={variable.isReference}
                  onChange={(e) => handleVariableChange(variable.id, 'isReference', e.target.checked)}
                />
                <CheckboxLabel htmlFor={`isReference-${variable.id}`}>
                  Riferimento (REF TO)
                </CheckboxLabel>
              </FormGroup>
            </VariableGrid>
          </VariableItem>
        ))}
      </VariablesContainer>
      
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

const FormHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
  
  h3 {
    margin: 0;
  }
`;

const FormActions = styled.div`
  display: flex;
  gap: 10px;
`;

const VariablesContainer = styled.div`
  display: flex;
  flex-direction: column;
  gap: 20px;
  margin-bottom: 20px;
`;

const VariableItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 6px;
  padding: 15px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
  transition: box-shadow 0.3s ease;
  
  &:hover {
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }
`;

const VariableHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
`;

const VariableTitle = styled.div`
  font-weight: bold;
  color: #333;
  font-size: 16px;
`;

const ButtonGroup = styled.div`
  display: flex;
  gap: 5px;
`;

const VariableGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 15px;
`;

const PrefixInput = styled.div`
  display: flex;
  gap: 5px;
`;

const PrefixSelect = styled.select`
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-family: 'Courier New', monospace;
  min-width: 100px;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const TypeSelector = styled.div`
  position: relative;
  width: 100%;
`;

const TypeAutocomplete = styled.div`
  display: flex;
  width: 100%;
  
  input {
    flex: 1;
    border-radius: 4px 0 0 4px;
  }
`;

const TypeDropdownTrigger = styled.button`
  background: #f1f1f1;
  border: 1px solid #ddd;
  border-left: none;
  border-radius: 0 4px 4px 0;
  padding: 0 10px;
  cursor: pointer;
  transition: background-color 0.2s;
  
  &:hover {
    background: #e5e5e5;
  }
`;

const TypeDropdown = styled.div`
  position: absolute;
  top: 100%;
  left: 0;
  width: 100%;
  max-height: 300px;
  overflow-y: auto;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
  z-index: 10;
  display: none;
  
  ${TypeSelector}:hover & {
    display: block;
  }
`;

const TypeSearchInput = styled.input`
  width: 100%;
  padding: 10px;
  border: none;
  border-bottom: 1px solid #eee;
  font-size: 14px;
  
  &:focus {
    outline: none;
    background: #f9f9f9;
  }
`;

const TypeList = styled.div`
  max-height: 250px;
  overflow-y: auto;
`;

const TypeCategory = styled.div`
  margin-bottom: 5px;
`;

const TypeCategoryHeader = styled.div`
  padding: 5px 10px;
  background: #f5f5f5;
  font-weight: bold;
  font-size: 13px;
  color: #666;
  border-bottom: 1px solid #eee;
  border-top: 1px solid #eee;
`;

const TypeItem = styled.div`
  padding: 8px 10px;
  cursor: pointer;
  transition: background-color 0.2s;
  
  &:hover {
    background: #f0f7ff;
  }
`;

const TypeName = styled.div`
  font-weight: 500;
  font-size: 14px;
`;

const TypeDescription = styled.div`
  font-size: 12px;
  color: #666;
  margin-top: 2px;
`;

const SelectRow = styled.div`
  display: flex;
  gap: 10px;
  align-items: center;
  
  input, select {
    flex: 1;
  }
`;

const Select = styled.select`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: 'Courier New', monospace;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const InitialValueSelect = styled(Select)`
  max-width: 200px;
`;

const LengthContainer = styled.div`
  display: flex;
  gap: 10px;
  align-items: center;
  
  input {
    flex: 1;
  }
  
  span {
    color: #666;
    font-weight: bold;
  }
`;

const Checkbox = styled.input`
  margin: 0 8px 0 0;
`;

const CheckboxLabel = styled.label`
  display: flex;
  align-items: center;
  user-select: none;
  cursor: pointer;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default DataDeclarationForm;