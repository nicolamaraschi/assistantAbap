import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

const FlowerForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    operationType: 'alv-report', // Tipo di operazione ABAP
    flowerType: '', // Tipo di fiore specifico
    outputType: 'DISPLAY', // Tipo di output
    selectionOptions: [
      { 
        id: 1, 
        name: 's_color', 
        label: 'Colore Fiore', 
        type: 'color'
      }
    ],
    sortingOptions: [
      { 
        id: 1, 
        field: 'name', 
        direction: 'ASCENDING' 
      }
    ],
    reportTitle: 'ZFLOWER_REPORT',
    additionalFeatures: {
      includeStatistics: false,
      includeVariantSave: true,
      highlightRarePlants: false
    }
  });
  
  // Opzioni per il tipo di operazione
  const operationTypes = [
    { value: 'alv-report', label: 'Report ALV Fiori' },
    { value: 'select', label: 'Selezione Fiori' },
    { value: 'count', label: 'Conteggio Fiori' },
    { value: 'export', label: 'Esportazione Dati' }
  ];

  // Tipi di output
  const outputTypes = [
    { value: 'DISPLAY', label: 'Visualizzazione' },
    { value: 'DOWNLOAD', label: 'Download' },
    { value: 'EMAIL', label: 'Invio Email' }
  ];
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['flower']) {
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
  
  // Aggiunge una nuova opzione di selezione
  const handleAddSelectionOption = () => {
    const newId = Math.max(0, ...formData.selectionOptions.map(o => o.id)) + 1;
    setFormData(prevData => ({
      ...prevData,
      selectionOptions: [
        ...prevData.selectionOptions,
        { 
          id: newId, 
          name: `s_opt${newId}`, 
          label: `Opzione ${newId}`,
          type: 'color'
        }
      ]
    }));
  };
  
  // Rimuove un'opzione di selezione
  const handleRemoveSelectionOption = (id) => {
    setFormData(prevData => ({
      ...prevData,
      selectionOptions: prevData.selectionOptions.filter(o => o.id !== id)
    }));
  };
  
  // Modifica un'opzione di selezione
  const handleSelectionOptionChange = (id, field, value) => {
    setFormData(prevData => ({
      ...prevData,
      selectionOptions: prevData.selectionOptions.map(o => 
        o.id === id ? { ...o, [field]: value } : o
      )
    }));
  };
  
  // Aggiunge un'opzione di ordinamento
  const handleAddSortingOption = () => {
    const newId = Math.max(0, ...formData.sortingOptions.map(o => o.id)) + 1;
    setFormData(prevData => ({
      ...prevData,
      sortingOptions: [
        ...prevData.sortingOptions,
        { 
          id: newId, 
          field: 'name', 
          direction: 'ASCENDING' 
        }
      ]
    }));
  };
  
  // Rimuove un'opzione di ordinamento
  const handleRemoveSortingOption = (id) => {
    setFormData(prevData => ({
      ...prevData,
      sortingOptions: prevData.sortingOptions.filter(o => o.id !== id)
    }));
  };
  
  // Modifica un'opzione di ordinamento
  const handleSortingOptionChange = (id, field, value) => {
    setFormData(prevData => ({
      ...prevData,
      sortingOptions: prevData.sortingOptions.map(o => 
        o.id === id ? { ...o, [field]: value } : o
      )
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
      <FormGroup label="Tipo di Operazione:">
        <select
          name="operationType"
          value={formData.operationType}
          onChange={handleChange}
        >
          {operationTypes.map(type => (
            <option key={type.value} value={type.value}>
              {type.label}
            </option>
          ))}
        </select>
      </FormGroup>
      
      <FormGroup label="Tipo di Fiore:">
        <ControlledInput 
          type="text"
          name="flowerType"
          value={formData.flowerType}
          onChange={handleChange}
          placeholder="es. Rosa, Tulipano, Orchidea"
        />
      </FormGroup>
      
      <FormGroup label="Titolo Report:">
        <ControlledInput 
          type="text"
          name="reportTitle"
          value={formData.reportTitle}
          onChange={handleChange}
        />
      </FormGroup>
      
      <FormGroup label="Tipo di Output:">
        <select
          name="outputType"
          value={formData.outputType}
          onChange={handleChange}
        >
          {outputTypes.map(type => (
            <option key={type.value} value={type.value}>
              {type.label}
            </option>
          ))}
        </select>
      </FormGroup>
      
      <FormGroup label="Opzioni di Selezione:">
        {formData.selectionOptions.map(option => (
          <SelectionOptionItem key={option.id}>
            <OptionHeader>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveSelectionOption(option.id)}
                disabled={formData.selectionOptions.length <= 1}
              />
            </OptionHeader>
            
            <TwoColumnsGrid>
              <FormGroup label="Nome Opzione:">
                <ControlledInput 
                  type="text"
                  value={option.name}
                  onChange={(e) => handleSelectionOptionChange(option.id, 'name', e.target.value)}
                />
              </FormGroup>
              
              <FormGroup label="Etichetta:">
                <ControlledInput 
                  type="text"
                  value={option.label}
                  onChange={(e) => handleSelectionOptionChange(option.id, 'label', e.target.value)}
                />
              </FormGroup>
              
              <FormGroup label="Tipo:">
                <select
                  value={option.type}
                  onChange={(e) => handleSelectionOptionChange(option.id, 'type', e.target.value)}
                >
                  <option value="color">Colore</option>
                  <option value="species">Specie</option>
                  <option value="date">Data Fioritura</option>
                  <option value="region">Regione</option>
                </select>
              </FormGroup>
            </TwoColumnsGrid>
          </SelectionOptionItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddSelectionOption}
        >
          Aggiungi Opzione di Selezione
        </Button>
      </FormGroup>
      
      <FormGroup label="Opzioni di Ordinamento:">
        {formData.sortingOptions.map(option => (
          <SortingOptionItem key={option.id}>
            <OptionHeader>
              <Button
                variant="text"
                size="small"
                icon={<FiTrash2 />}
                onClick={() => handleRemoveSortingOption(option.id)}
                disabled={formData.sortingOptions.length <= 1}
              />
            </OptionHeader>
            
            <TwoColumnsGrid>
              <FormGroup label="Campo:">
                <select
                  value={option.field}
                  onChange={(e) => handleSortingOptionChange(option.id, 'field', e.target.value)}
                >
                  <option value="name">Nome</option>
                  <option value="color">Colore</option>
                  <option value="blooming_date">Data Fioritura</option>
                  <option value="region">Regione</option>
                </select>
              </FormGroup>
              
              <FormGroup label="Direzione:">
                <select
                  value={option.direction}
                  onChange={(e) => handleSortingOptionChange(option.id, 'direction', e.target.value)}
                >
                  <option value="ASCENDING">Crescente</option>
                  <option value="DESCENDING">Decrescente</option>
                </select>
              </FormGroup>
            </TwoColumnsGrid>
          </SortingOptionItem>
        ))}
        
        <Button
          variant="outline"
          size="small"
          icon={<FiPlus />}
          onClick={handleAddSortingOption}
        >
          Aggiungi Opzione di Ordinamento
        </Button>
      </FormGroup>
      
      <FormGroup label="Funzionalità Aggiuntive:">
        <OptionsFlex>
          <FormGroup inline>
            <input
              type="checkbox"
              name="additionalFeatures.includeStatistics"
              checked={formData.additionalFeatures.includeStatistics}
              onChange={(e) => setFormData(prevData => ({
                ...prevData,
                additionalFeatures: {
                  ...prevData.additionalFeatures,
                  includeStatistics: e.target.checked
                }
              }))}
              id="includeStatistics"
            />
            <label htmlFor="includeStatistics">Includi Statistiche</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="additionalFeatures.includeVariantSave"
              checked={formData.additionalFeatures.includeVariantSave}
              onChange={(e) => setFormData(prevData => ({
                ...prevData,
                additionalFeatures: {
                  ...prevData.additionalFeatures,
                  includeVariantSave: e.target.checked
                }
              }))}
              id="includeVariantSave"
            />
            <label htmlFor="includeVariantSave">Salvataggio Varianti</label>
          </FormGroup>
          
          <FormGroup inline>
            <input
              type="checkbox"
              name="additionalFeatures.highlightRarePlants"
              checked={formData.additionalFeatures.highlightRarePlants}
              onChange={(e) => setFormData(prevData => ({
                ...prevData,
                additionalFeatures: {
                  ...prevData.additionalFeatures,
                  highlightRarePlants: e.target.checked
                }
              }))}
              id="highlightRarePlants"
            />
            <label htmlFor="highlightRarePlants">Evidenzia Piante Rare</label>
          </FormGroup>
        </OptionsFlex>
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
`;

const SelectionOptionItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const SortingOptionItem = styled(SelectionOptionItem)``;

const OptionHeader = styled.div`
  display: flex;
  justify-content: flex-end;
  margin-bottom: 10px;
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
  gap: 15px;
  margin-top: 10px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default FlowerForm;