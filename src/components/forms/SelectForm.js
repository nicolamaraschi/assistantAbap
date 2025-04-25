// SelectForm.js - Migliorato con selezione tabelle SAP e join correlati
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Definizione delle tabelle SAP raggruppate per modulo
// Definizione delle tabelle SAP raggruppate per modulo
const sapTables = {
  PP: [
    { name: 'MARA', description: 'Anagrafica Materiale', key: 'MANDT, MATNR' },
    { name: 'MAKT', description: 'Testo Materiale', key: 'MANDT, MATNR, SPRAS' },
    { name: 'PLKO', description: 'Distinta Base - Testata', key: 'MANDT, PLNNR, PLNAL' },
    { name: 'PLPO', description: 'Distinta Base - Posizioni', key: 'MANDT, PLNNR, PLNAL, PLNFL, ZAEHL' },
    { name: 'MAST', description: 'Legame BOM / Materiali', key: 'MANDT, MATNR, WERKS, STLNR, STLAN' },
    { name: 'CRHD', description: 'Anagrafica Posto di Lavoro - Testata', key: 'MANDT, OBJTY, OBJID' },
    { name: 'AUFK', description: 'Ordini di Produzione - Testata', key: 'MANDT, AUFNR' },
    { name: 'AFKO', description: 'Ordini di Produzione PP - Testata', key: 'MANDT, AUFNR' },
    { name: 'RESB', description: 'Prenotazioni / Fabbisogni Dipendenti', key: 'MANDT, RSNUM, RSPOS' },
    { name: 'AFVC', description: 'Operazioni Ordine', key: 'MANDT, AUFPL' },
    { name: 'CRCA', description: 'Capacità standard per centro di lavoro', key: 'MANDT, OBJID' }
  ],
  SD: [
    { name: 'KNA1', description: 'Anagrafica Cliente - Dati Generali', key: 'MANDT, KUNNR' },
    { name: 'KNVV', description: 'Anagrafica Cliente - Dati Area Vendite', key: 'MANDT, KUNNR, VKORG, VTWEG, SPARTE' },
    { name: 'MVKE', description: 'Anagrafica Materiale - Dati Vendita', key: 'MANDT, MATNR, VKORG, VTWEG' },
    { name: 'VBAK', description: 'Documento di Vendita - Testata', key: 'MANDT, VBELN' },
    { name: 'VBAP', description: 'Documento di Vendita - Posizioni', key: 'MANDT, VBELN, POSNR' },
    { name: 'LIKP', description: 'Documento di Consegna - Testata', key: 'MANDT, VBELN' },
    { name: 'LIPS', description: 'Documento di Consegna - Posizioni', key: 'MANDT, VBELN, POSNR' },
    { name: 'VBRK', description: 'Documento di Fatturazione - Testata', key: 'MANDT, VBELN' },
    { name: 'VBRP', description: 'Documento di Fatturazione - Posizioni', key: 'MANDT, VBELN, POSNR' },
    { name: 'VBPA', description: 'Partner Documento di Vendita', key: 'MANDT, VBELN, POSNR, PARNR, PARVW' },
    { name: 'VBUK', description: 'Stato Documento di Vendita', key: 'MANDT, VBELN' }
  ],
  CO: [
    { name: 'CSKS', description: 'Centri di Costo - Dati Anagrafici', key: 'MANDT, KOKRS, KOSTL, DATBI' },
    { name: 'CSKT', description: 'Centri di Costo - Testi', key: 'MANDT, KOKRS, KOSTL, SPRAS, DATBI' },
    { name: 'CSKA', description: 'Elementi di Costo - Dati Dipendenti dal Piano dei Conti', key: 'MANDT, KTOPL, KSTAR' },
    { name: 'CEPC', description: 'Centri di Profitto - Dati Anagrafici', key: 'MANDT, KOKRS, PRCTR, DATBI' },
    { name: 'COBK', description: 'Oggetto CO - Testata Documento', key: 'MANDT, KOKRS, BELNR' },
    { name: 'COEP', description: 'Oggetto CO - Posizioni (per Periodo)', key: 'MANDT, KOKRS, BELNR, BUZEI' },
    { name: 'CSKB', description: 'Elementi di costo per centro di costo', key: 'MANDT, KOKRS, KSTAR, DATBI' },
    { name: 'COAS', description: 'Ordini interni', key: 'MANDT, AUFNR' }
  ],
  FI: [
    { name: 'SKA1', description: 'Conti Co.Ge. - Dati Anagrafici (Piano dei Conti)', key: 'MANDT, KTOPL, SAKNR' },
    { name: 'SKAT', description: 'Conti Co.Ge. - Testi (Piano dei Conti)', key: 'MANDT, KTOPL, SAKNR, SPRAS' },
    { name: 'SKB1', description: 'Conti Co.Ge. - Dati Anagrafici (Società)', key: 'MANDT, BUKRS, SAKNR' },
    { name: 'LFA1', description: 'Anagrafica Fornitore - Dati Generali', key: 'MANDT, LIFNR' },
    { name: 'BKPF', description: 'Documento Contabile - Testata', key: 'MANDT, BUKRS, BELNR, GJAHR' },
    { name: 'BSEG', description: 'Documento Contabile - Posizioni', key: 'MANDT, BUKRS, BELNR, GJAHR, BUZEI' },
    { name: 'LFB1', description: 'Dati fornitore per società', key: 'MANDT, LIFNR, BUKRS' }
  ],
  MM: [
    { name: 'MARC', description: 'Anagrafica Materiale - Dati Impianto', key: 'MANDT, MATNR, WERKS' },
    { name: 'MARD', description: 'Anagrafica Materiale - Dati Magazzino', key: 'MANDT, MATNR, WERKS, LGORT' },
    { name: 'LFM1', description: 'Anagrafica Fornitore - Dati Acquisti', key: 'MANDT, LIFNR, EKORG' },
    { name: 'EINA', description: 'Record Info Acquisti - Dati Generali', key: 'MANDT, LIFNR, MATNR' },
    { name: 'EKKO', description: 'Documento di Acquisto - Testata', key: 'MANDT, EBELN' },
    { name: 'EKPO', description: 'Documento di Acquisto - Posizioni', key: 'MANDT, EBELN, EBELP' },
    { name: 'EBAN', description: 'Richieste d\'acquisto', key: 'MANDT, BANFN' },
    { name: 'EBKN', description: 'Allocazioni di costo', key: 'MANDT, BANFN, BNFPO' }
  ]
};

// Definizione delle relazioni tra tabelle per suggerire JOIN
const tableRelations = {
  'MARA': [
    { table: 'MAKT', condition: 'MARA~MATNR = MAKT~MATNR AND MAKT~SPRAS = SY-LANGU', module: 'PP/SD/MM' },
    { table: 'MARC', condition: 'MARA~MATNR = MARC~MATNR', module: 'MM' },
    { table: 'MAST', condition: 'MARA~MATNR = MAST~MATNR', module: 'PP' },
    { table: 'MVKE', condition: 'MARA~MATNR = MVKE~MATNR', module: 'SD' }
  ],
  'KNA1': [
    { table: 'KNVV', condition: 'KNA1~KUNNR = KNVV~KUNNR', module: 'SD' },
    { table: 'VBAK', condition: 'KNA1~KUNNR = VBAK~KUNNR', module: 'SD' },
    { table: 'VBPA', condition: 'KNA1~KUNNR = VBPA~KUNNR', module: 'SD' },
    { table: 'BSEG', condition: 'KNA1~KUNNR = BSEG~KUNNR', module: 'FI/SD' }
  ],
  'VBAK': [
    { table: 'VBAP', condition: 'VBAK~VBELN = VBAP~VBELN', module: 'SD' },
    { table: 'VBPA', condition: 'VBAK~VBELN = VBPA~VBELN', module: 'SD' },
    { table: 'VBFA', condition: 'VBAK~VBELN = VBFA~VBELN', module: 'SD' },
    { table: 'VBUK', condition: 'VBAK~VBELN = VBUK~VBELN', module: 'SD' }
  ],
  'EKKO': [
    { table: 'EKPO', condition: 'EKKO~EBELN = EKPO~EBELN', module: 'MM' },
    { table: 'LFA1', condition: 'EKKO~LIFNR = LFA1~LIFNR', module: 'MM' },
    { table: 'EBAN', condition: 'EKKO~BANFN = EBAN~BANFN', module: 'MM' }
  ],
  'BKPF': [
    { table: 'BSEG', condition: 'BKPF~BUKRS = BSEG~BUKRS AND BKPF~BELNR = BSEG~BELNR AND BKPF~GJAHR = BSEG~GJAHR', module: 'FI' }
  ],
  'CSKS': [
    { table: 'CSKT', condition: 'CSKS~KOKRS = CSKT~KOKRS AND CSKS~KOSTL = CSKT~KOSTL AND CSKT~SPRAS = SY-LANGU', module: 'CO' }
  ],
  'AUFK': [
    { table: 'AFKO', condition: 'AUFK~AUFNR = AFKO~AUFNR', module: 'PP' },
    { table: 'RESB', condition: 'AUFK~AUFNR = RESB~AUFNR', module: 'PP' },
    { table: 'COAS', condition: 'AUFK~AUFNR = COAS~AUFNR', module: 'CO' }
  ],
  'AFKO': [
    { table: 'AFVC', condition: 'AFKO~AUFPL = AFVC~AUFPL', module: 'PP' }
  ],
  'CRHD': [
    { table: 'CRCA', condition: 'CRHD~OBJID = CRCA~OBJID', module: 'PP' }
  ],
  'LFA1': [
    { table: 'LFB1', condition: 'LFA1~LIFNR = LFB1~LIFNR', module: 'FI' },
    { table: 'BSEG', condition: 'LFA1~LIFNR = BSEG~LIFNR', module: 'FI/MM' }
  ],
  'BSEG': [
    { table: 'KNA1', condition: 'BSEG~KUNNR = KNA1~KUNNR', module: 'FI/SD' },
    { table: 'LFA1', condition: 'BSEG~LIFNR = LFA1~LIFNR', module: 'FI/MM' }
  ],
  'EBAN': [
    { table: 'EBKN', condition: 'EBAN~BANFN = EBKN~BANFN AND EBAN~BNFPO = EBKN~BNFPO', module: 'MM' }
  ],
  'EKPO': [
    { table: 'EBAN', condition: 'EKPO~BANFN = EBAN~BANFN AND EKPO~BNFPO = EBAN~BNFPO', module: 'MM' }
  ],
  'CSKA': [
    { table: 'CSKB', condition: 'CSKA~KSTAR = CSKB~KSTAR AND CSKA~KTOPL = CSKB~KTOPL', module: 'CO' }
  ]
};

const SelectForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    fields: '*',
    table: 'MARA',
    into: 'TABLE lt_result',
    where: '',
    orderBy: '',
    groupBy: '',
    having: '',
    addJoin: false,
    joinType: 'INNER JOIN',
    joinTable: '',
    joinCondition: '',
    useUnion: false,
    unionType: 'UNION ALL',
    unionSelect: 'SELECT field1, field2\n  FROM ztable2\n  WHERE field2 = \'value\'',
    useForAllEntries: false,
    forAllEntriesTable: 'lt_keys',
    forAllEntriesWhere: '',
    // Nuove proprietà per la selezione delle tabelle SAP
    sapModule: 'PP',
    suggestedJoins: [],
    selectedTableKeys: ''
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['select']) {
      setFormData(formState['select']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('select', formData);
  }, [formData, updateFormState]);
  
  // Aggiorna i suggerimenti di JOIN quando cambia la tabella principale
  useEffect(() => {
    updateSuggestedJoins(formData.table);
    updateTableKeys(formData.table);
  }, [formData.table]);
  
  // Funzione per aggiornare i suggerimenti di JOIN
  const updateSuggestedJoins = (tableName) => {
    const relations = tableRelations[tableName] || [];
    setFormData(prev => ({
      ...prev,
      suggestedJoins: relations,
      joinTable: relations.length > 0 ? relations[0].table : ''
    }));
  };
  
  // Funzione per aggiornare le chiavi della tabella selezionata
  const updateTableKeys = (tableName) => {
    let keys = '';
    for (const moduleKey in sapTables) {
      const table = sapTables[moduleKey].find(t => t.name === tableName);
      if (table) {
        keys = table.key;
        break;
      }
    }
    setFormData(prev => ({
      ...prev,
      selectedTableKeys: keys
    }));
  };
  
  // Funzione per aggiornare la condizione JOIN in base alla tabella JOIN selezionata
  const updateJoinCondition = (joinTableName) => {
    const relations = formData.suggestedJoins;
    const relation = relations.find(r => r.table === joinTableName);
    if (relation) {
      setFormData(prev => ({
        ...prev,
        joinCondition: relation.condition
      }));
    }
  };
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    let updatedFormData = {
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    };
    
    // Logica speciale quando cambia il modulo SAP
    if (name === 'sapModule') {
      updatedFormData.table = sapTables[value][0].name;
    }
    
    // Logica speciale quando cambia la tabella JOIN
    if (name === 'joinTable') {
      updateJoinCondition(value);
    }
    
    setFormData(updatedFormData);
  };
  
  // Gestisce la selezione della tabella
  const handleTableSelect = (e) => {
    const tableName = e.target.value;
    setFormData({
      ...formData,
      table: tableName
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('select', formData);
    }
  };
  
  return (
    <FormContainer>
      <ModuleSelector>
        <h4>Seleziona Modulo SAP</h4>
        <ModuleTabs>
          {Object.keys(sapTables).map(module => (
            <ModuleTab 
              key={module}
              active={formData.sapModule === module} 
              onClick={() => setFormData({
                ...formData,
                sapModule: module,
                table: sapTables[module][0].name
              })}
            >
              {module}
            </ModuleTab>
          ))}
        </ModuleTabs>
      </ModuleSelector>

      <FormGroup label="Tabella SAP:">
        <select
          name="table"
          value={formData.table}
          onChange={handleTableSelect}
          style={{width: '100%'}}
        >
          {sapTables[formData.sapModule].map(table => (
            <option key={table.name} value={table.name}>
              {table.name} - {table.description}
            </option>
          ))}
        </select>
      </FormGroup>
      
      {formData.selectedTableKeys && (
        <KeysInfo>
          <strong>Chiave primaria:</strong> {formData.selectedTableKeys}
        </KeysInfo>
      )}
      
      <FormGroup label="Campi da selezionare:" tooltip="Specificare i campi separati da virgola, o * per tutti">
        <ControlledInput
          type="text"
          name="fields"
          value={formData.fields}
          onChange={handleChange}
        />
      </FormGroup>
      
      <SelectOptions>
        <OptionTabs>
          <OptionTab active={!formData.addJoin && !formData.useForAllEntries} onClick={() => setFormData({
            ...formData,
            addJoin: false,
            useForAllEntries: false
          })}>
            Select Standard
          </OptionTab>
          <OptionTab active={formData.addJoin} onClick={() => setFormData({
            ...formData,
            addJoin: true,
            useForAllEntries: false
          })}>
            Join
          </OptionTab>
          <OptionTab active={formData.useForAllEntries} onClick={() => setFormData({
            ...formData,
            addJoin: false,
            useForAllEntries: true
          })}>
            For All Entries
          </OptionTab>
        </OptionTabs>
        
        {formData.addJoin && (
          <>
            <FormGroup label="Tipo di JOIN:">
              <select
                name="joinType"
                value={formData.joinType}
                onChange={handleChange}
              >
                <option value="INNER JOIN">INNER JOIN</option>
                <option value="LEFT OUTER JOIN">LEFT OUTER JOIN</option>
                <option value="RIGHT OUTER JOIN">RIGHT OUTER JOIN</option>
              </select>
            </FormGroup>
            
            {formData.suggestedJoins.length > 0 ? (
              <>
                <FormGroup label="Tabella JOIN correlata:">
                  <select
                    name="joinTable"
                    value={formData.joinTable}
                    onChange={handleChange}
                    style={{width: '100%'}}
                  >
                    {formData.suggestedJoins.map(join => (
                      <option key={join.table} value={join.table}>
                        {join.table} ({join.module})
                      </option>
                    ))}
                  </select>
                </FormGroup>
              </>
            ) : (
              <FormGroup label="Tabella JOIN:">
                <ControlledInput
                  type="text"
                  name="joinTable"
                  value={formData.joinTable}
                  onChange={handleChange}
                />
              </FormGroup>
            )}
            
            <FormGroup label="Condizione JOIN:">
              <ControlledInput
                type="text"
                name="joinCondition"
                value={formData.joinCondition}
                onChange={handleChange}
              />
            </FormGroup>
          </>
        )}
        
        {formData.useForAllEntries && (
          <>
            <FormGroup label="Tabella FOR ALL ENTRIES:">
              <ControlledInput
                type="text"
                name="forAllEntriesTable"
                value={formData.forAllEntriesTable}
                onChange={handleChange}
              />
            </FormGroup>
            
            <FormGroup label="Condizione FOR ALL ENTRIES:">
              <ControlledInput
                type="text"
                name="forAllEntriesWhere"
                value={formData.forAllEntriesWhere}
                onChange={handleChange}
                placeholder={`${formData.table}~MATNR = ${formData.forAllEntriesTable}-MATNR`}
              />
            </FormGroup>
          </>
        )}
      </SelectOptions>
      
      <FormGroup label="INTO:">
        <ControlledInput
          type="text"
          name="into"
          value={formData.into}
          onChange={handleChange}
        />
      </FormGroup>
      
      {!formData.useForAllEntries && (
        <FormGroup label="WHERE (opzionale):">
          <ControlledInput
            type="text"
            name="where"
            value={formData.where}
            onChange={handleChange}
          />
        </FormGroup>
      )}
      
      <AdvancedOptions>
        <h4>Opzioni Avanzate</h4>
        
        <AdvancedSection>
          <FormGroup label="ORDER BY (opzionale):">
            <ControlledInput
              type="text"
              name="orderBy"
              value={formData.orderBy}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="GROUP BY (opzionale):">
            <ControlledInput
              type="text"
              name="groupBy"
              value={formData.groupBy}
              onChange={handleChange}
            />
          </FormGroup>
          
          <FormGroup label="HAVING (opzionale):">
            <ControlledInput
              type="text"
              name="having"
              value={formData.having}
              onChange={handleChange}
            />
          </FormGroup>
        </AdvancedSection>
        
        <AdvancedSection>
          <FormGroup inline>
            <input
              type="checkbox"
              name="useUnion"
              checked={formData.useUnion}
              onChange={handleChange}
              id="useUnion"
            />
            <label htmlFor="useUnion">Aggiungi UNION</label>
          </FormGroup>
          
          {formData.useUnion && (
            <>
              <FormGroup label="Tipo di UNION:">
                <select
                  name="unionType"
                  value={formData.unionType}
                  onChange={handleChange}
                >
                  <option value="UNION">UNION</option>
                  <option value="UNION ALL">UNION ALL</option>
                </select>
              </FormGroup>
              
              <FormGroup label="SELECT UNION:">
                <ControlledTextarea
                  name="unionSelect"
                  value={formData.unionSelect}
                  onChange={handleChange}
                  rows={5}
                />
              </FormGroup>
            </>
          )}
        </AdvancedSection>
      </AdvancedOptions>
      
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

const SelectOptions = styled.div`
  margin-bottom: 15px;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
`;

const OptionTabs = styled.div`
  display: flex;
  margin-bottom: 15px;
  border-bottom: 1px solid #ddd;
`;

const OptionTab = styled.div`
  padding: 8px 16px;
  cursor: pointer;
  border-bottom: 2px solid ${props => props.active ? '#0066cc' : 'transparent'};
  color: ${props => props.active ? '#0066cc' : '#333'};
  font-weight: ${props => props.active ? 'bold' : 'normal'};
  
  &:hover {
    background-color: #f9f9f9;
  }
`;

const AdvancedOptions = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 6px;
  padding: 15px;
  margin-bottom: 20px;
  
  h4 {
    margin-top: 0;
    margin-bottom: 15px;
    font-size: 16px;
    color: #333;
  }
`;

const AdvancedSection = styled.div`
  background: #fff;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 12px;
  margin-bottom: 12px;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

const ModuleSelector = styled.div`
  margin-bottom: 20px;
`;

const ModuleTabs = styled.div`
  display: flex;
  gap: 8px;
  margin-top: 8px;
`;

const ModuleTab = styled.button`
  padding: 6px 12px;
  background-color: ${props => props.active ? '#007bff' : '#f0f0f0'};
  color: ${props => props.active ? '#fff' : '#333'};
  border: 1px solid ${props => props.active ? '#0069d9' : '#ddd'};
  border-radius: 4px;
  cursor: pointer;
  font-size: 14px;
  
  &:hover {
    background-color: ${props => props.active ? '#0069d9' : '#e0e0e0'};
  }
`;

const KeysInfo = styled.div`
  font-size: 0.9rem;
  margin-bottom: 15px;
  padding: 8px;
  background-color: #f8f9fa;
  border-left: 3px solid #007bff;
`;


export default SelectForm;