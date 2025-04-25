// TryCatchForm.js - Migliorato con supporto RESUME
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

// Lista delle eccezioni ABAP comuni per il dropdown
const ABAP_EXCEPTIONS = {
  // Eccezioni classiche
  "Classiche": [
    "NO_AUTHORITY",
    "NOT_FOUND", 
    "INVALID_INPUT",
    "NO_DATA_FOUND",
    "INTERNAL_ERROR",
    "ERROR_OCCURRED",
    "DATA_NOT_FOUND",
    "INVALID_DATA",
    "NO_NUM_1",
    "MANDATORY_PARAM_MISSING",
    "MAX_RECORD",
    "OTHERS"
  ],
  
  // Eccezioni basate su classi (OO)
  "Eccezioni CX_SY (sistema)": [
    "cx_sy_zerodivide",
    "cx_sy_conversion_no_number",
    "cx_sy_open_sql_error",
    "cx_sy_itab_line_not_found",
    "cx_sy_import_mismatch_error",
    "cx_sy_file_open",
    "cx_sy_file_access_error",
    "cx_sy_arithmetic_error",
    "cx_sy_no_handler",
    "cx_dynamic_check",
    "cx_sy_ref_is_initial",
    "cx_sy_dyn_call_illegal_method",
    "cx_sy_range_out_of_bounds",
    "cx_sy_conversion_overflow",
    "cx_sy_invalid_cast",
    "cx_sy_file_delete",
    "cx_sy_db_conversion",
    "cx_sy_oref_is_no_object"
  ],
  
  // Eccezioni DB
  "Eccezioni Database": [
    "cx_sy_open_sql_db",
    "cx_sy_dynamic_osql_error",
    "cx_sy_dynamic_osql_syntax",
    "cx_sy_sql_unsupported_feature",
    "cx_dbif_internal_error",
    "cx_dbif_illegal_statement",
    "cx_dbif_rsql_error"
  ],
  
  // Eccezioni di gestione memoria
  "Eccezioni Memoria": [
    "cx_sy_export_buffer_no_memory",
    "cx_sy_export_no_shared_memory",
    "cx_sy_no_free_memory",
    "cx_sy_table_too_large",
    "cx_sy_import_buffer_too_small"
  ],
  
  // Altre eccezioni di sistema
  "Altre Eccezioni": [
    "cx_os_file_not_found",
    "cx_os_path_too_long",
    "cx_bsp_parse_error",
    "cx_bsp_runtime_error",
    "cx_wdy_controller_error",
    "cx_sy_com_error",
    "cx_root"
  ]
};

const ABAP_EXCEPTION_DESCRIPTIONS = {
  // Classiche
  "NO_AUTHORITY": "Utente senza autorizzazioni per l'operazione.",
  "NOT_FOUND": "Oggetto richiesto non trovato.",
  "INVALID_INPUT": "Valore inserito non valido.",
  "NO_DATA_FOUND": "Nessun dato trovato per la selezione.",
  "INTERNAL_ERROR": "Errore interno durante l'esecuzione.",
  "ERROR_OCCURRED": "Si è verificato un errore generico.",
  "DATA_NOT_FOUND": "I dati richiesti non sono disponibili.",
  "INVALID_DATA": "Dati ricevuti non corretti o incompleti.",
  "NO_NUM_1": "Valore numerico mancante.",
  "MANDATORY_PARAM_MISSING": "Parametro obbligatorio mancante.",
  "MAX_RECORD": "Raggiunto il numero massimo di record.",
  "OTHERS": "Eccezione generica non specificata.",
  
  // CX_SY (OO)
  "cx_sy_zerodivide": "Divisione per zero non permessa.",
  "cx_sy_conversion_no_number": "Errore nella conversione di un valore non numerico.",
  "cx_sy_open_sql_error": "Errore generico durante l'esecuzione SQL.",
  "cx_sy_itab_line_not_found": "Riga non trovata nell'internal table.",
  "cx_sy_import_mismatch_error": "Differenza tra struttura attesa e dati importati.",
  "cx_sy_file_open": "Impossibile aprire il file specificato.",
  "cx_sy_file_access_error": "Errore di accesso al file (lettura/scrittura).",
  "cx_sy_arithmetic_error": "Errore aritmetico generico.",
  "cx_sy_no_handler": "Nessun blocco CATCH definito per l'eccezione.",
  "cx_dynamic_check": "Errore di controllo dinamico dell'eccezione.",
  "cx_sy_ref_is_initial": "Riferimento oggetto non inizializzato (null).",
  "cx_sy_dyn_call_illegal_method": "Chiamata dinamica a metodo non valido.",
  "cx_sy_range_out_of_bounds": "Indice fuori dai limiti consentiti.",
  "cx_sy_conversion_overflow": "Overflow durante la conversione di tipo.",
  "cx_sy_invalid_cast": "Cast non valido tra tipi di classe.",
  "cx_sy_file_delete": "Errore durante la cancellazione del file.",
  "cx_sy_db_conversion": "Errore nella conversione tra struttura DB e ABAP.",
  "cx_sy_oref_is_no_object": "Oggetto non esistente (riferimento nullo).",

  // Database
  "cx_sy_open_sql_db": "Errore di accesso al database (Open SQL).",
  "cx_sy_dynamic_osql_error": "Errore dinamico in una query SQL.",
  "cx_sy_dynamic_osql_syntax": "Sintassi errata nella query SQL dinamica.",
  "cx_sy_sql_unsupported_feature": "Feature SQL non supportata.",
  "cx_dbif_internal_error": "Errore interno durante una chiamata DBIF.",
  "cx_dbif_illegal_statement": "Istruzione SQL non ammessa o errata.",
  "cx_dbif_rsql_error": "Errore generico SQL nel runtime ABAP.",
  
  // Memoria
  "cx_sy_export_buffer_no_memory": "Buffer di esportazione esaurito.",
  "cx_sy_export_no_shared_memory": "Memoria condivisa non disponibile.",
  "cx_sy_no_free_memory": "Memoria non sufficiente per l'operazione.",
  "cx_sy_table_too_large": "Internal table troppo grande per essere gestita.",
  "cx_sy_import_buffer_too_small": "Buffer troppo piccolo per importare i dati.",
  
  // Altre
  "cx_os_file_not_found": "File non trovato nel sistema operativo.",
  "cx_os_path_too_long": "Percorso del file troppo lungo.",
  "cx_bsp_parse_error": "Errore durante il parsing BSP.",
  "cx_bsp_runtime_error": "Errore a runtime in applicazione BSP.",
  "cx_wdy_controller_error": "Errore nel controller Web Dynpro.",
  "cx_sy_com_error": "Errore di comunicazione tra sistemi.",
  "cx_root": "Classe base per tutte le eccezioni (generico)."
};


const TryCatchForm = ({ onGenerate }) => {
  // Stato locale del form
  const [formData, setFormData] = useState({
    tryBlock: 'DATA(lv_result) = cl_some_class=>method( ).',
    catchBlocks: [
      { id: 1, className: 'cx_sy_zerodivide', content: 'WRITE: / \'Divisione per zero\'.', useResume: false }
    ],
    cleanup: '',
    addCleanup: false,
    multipleExceptions: false,
    useResume: false // Nuovo campo
  });
  
  // Accesso al context
  const { updateFormState, formState } = useAbap();
  
  // Carica lo stato salvato nel contesto
  useEffect(() => {
    if (formState['try-catch']) {
      setFormData(formState['try-catch']);
    }
  }, [formState]);
  
  // Salva lo stato nel contesto quando cambia
  useEffect(() => {
    updateFormState('try-catch', formData);
  }, [formData, updateFormState]);
  
  // Gestisce il cambiamento dei campi
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;
    setFormData({
      ...formData,
      [name]: type === 'checkbox' ? checked : value
    });
  };
  
  // Gestisce il cambiamento dei blocchi catch
  const handleCatchChange = (id, field, value) => {
    setFormData({
      ...formData,
      catchBlocks: formData.catchBlocks.map(c => 
        c.id === id ? { ...c, [field]: field === 'useResume' ? !c.useResume : value } : c
      )
    });
  };
  
  // Aggiunge un nuovo blocco catch
  const handleAddCatch = () => {
    const newId = Math.max(0, ...formData.catchBlocks.map(c => c.id)) + 1;
    setFormData({
      ...formData,
      catchBlocks: [
        ...formData.catchBlocks,
        { id: newId, className: 'cx_root', content: 'WRITE: / \'Errore generico\'.', useResume: false }
      ]
    });
  };
  
  // Rimuove un blocco catch
  const handleRemoveCatch = (id) => {
    setFormData({
      ...formData,
      catchBlocks: formData.catchBlocks.filter(c => c.id !== id)
    });
  };
  
  // Gestisce la generazione del codice
  const handleGenerate = () => {
    if (onGenerate) {
      onGenerate('try-catch', formData);
    }
  };
  
  // Renderizza un selettore di eccezioni
  const renderExceptionSelect = (currentValue, onChange) => (
    <ExceptionSelect 
      value={currentValue}
      onChange={onChange}
    >
      <option value="">-- Seleziona eccezione --</option>
      {Object.entries(ABAP_EXCEPTIONS).map(([category, exceptions]) => (
        <optgroup key={category} label={category}>
          {exceptions.map(exception => (
            <option key={exception} value={exception}>
              {exception} - {ABAP_EXCEPTION_DESCRIPTIONS[exception]}
            </option>
          ))}
        </optgroup>
      ))}
    </ExceptionSelect>
  );
  
  return (
    <FormContainer>
      <FormGroup label="Blocco TRY:">
        <ControlledTextarea
          name="tryBlock"
          value={formData.tryBlock}
          onChange={handleChange}
          rows={5}
        />
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="multipleExceptions"
          checked={formData.multipleExceptions}
          onChange={handleChange}
          id="multipleExceptions"
        />
        <label htmlFor="multipleExceptions">Gestisci eccezioni multiple</label>
      </FormGroup>
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="useResume"
          checked={formData.useResume}
          onChange={handleChange}
          id="useResume"
        />
        <label htmlFor="useResume">Abilita opzione RESUME</label>
      </FormGroup>
      
      {formData.multipleExceptions ? (
        <FormGroup label="Blocchi CATCH:">
          {formData.catchBlocks.map(catchBlock => (
            <CatchItem key={catchBlock.id}>
              <CatchHeader>
                <CatchTitle>Eccezione {catchBlock.id}</CatchTitle>
                <Button
                  variant="text"
                  size="small"
                  icon={<FiTrash2 />}
                  onClick={() => handleRemoveCatch(catchBlock.id)}
                  disabled={formData.catchBlocks.length === 1}
                />
              </CatchHeader>
              <FormGroup label="Classe eccezione:">
                {renderExceptionSelect(
                  catchBlock.className,
                  (e) => handleCatchChange(catchBlock.id, 'className', e.target.value)
                )}
              </FormGroup>
              <FormGroup label="Contenuto blocco CATCH:">
                <ControlledTextarea
                  value={catchBlock.content}
                  onChange={(e) => handleCatchChange(catchBlock.id, 'content', e.target.value)}
                  rows={3}
                />
              </FormGroup>
              {formData.useResume && (
                <FormGroup inline>
                  <input
                    type="checkbox"
                    checked={catchBlock.useResume}
                    onChange={() => handleCatchChange(catchBlock.id, 'useResume')}
                    id={`useResume-${catchBlock.id}`}
                  />
                  <label htmlFor={`useResume-${catchBlock.id}`}>Usa RESUME dopo questo catch</label>
                </FormGroup>
              )}
            </CatchItem>
          ))}
          
          <Button
            variant="outline"
            size="small"
            icon={<FiPlus />}
            onClick={handleAddCatch}
          >
            Aggiungi eccezione
          </Button>
        </FormGroup>
      ) : (
        <>
          <FormGroup label="Classe eccezione:">
            {renderExceptionSelect(
              formData.catchBlocks[0].className,
              (e) => handleCatchChange(formData.catchBlocks[0].id, 'className', e.target.value)
            )}
          </FormGroup>
          <FormGroup label="Contenuto blocco CATCH:">
            <ControlledTextarea
              value={formData.catchBlocks[0].content}
              onChange={(e) => handleCatchChange(formData.catchBlocks[0].id, 'content', e.target.value)}
              rows={4}
            />
          </FormGroup>
          {formData.useResume && (
            <FormGroup inline>
              <input
                type="checkbox"
                checked={formData.catchBlocks[0].useResume}
                onChange={() => handleCatchChange(formData.catchBlocks[0].id, 'useResume')}
                id="useResume-single"
              />
              <label htmlFor="useResume-single">Usa RESUME dopo questo catch</label>
            </FormGroup>
          )}
        </>
      )}
      
      <FormGroup inline>
        <input
          type="checkbox"
          name="addCleanup"
          checked={formData.addCleanup}
          onChange={handleChange}
          id="addCleanup"
        />
        <label htmlFor="addCleanup">Aggiungi blocco CLEANUP</label>
      </FormGroup>
      
      {formData.addCleanup && (
        <FormGroup label="Contenuto blocco CLEANUP:">
          <ControlledTextarea
            name="cleanup"
            value={formData.cleanup}
            onChange={handleChange}
            rows={4}
          />
        </FormGroup>
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
  
  input[type="text"],
  textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 15px;
    transition: border-color 0.3s, box-shadow 0.3s;
    font-family: 'Courier New', monospace;
  }
  
  input[type="text"]:focus,
  textarea:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  textarea {
    resize: vertical;
    min-height: 80px;
  }
`;

const ExceptionSelect = styled.select`
  width: 100%;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 15px;
  transition: border-color 0.3s, box-shadow 0.3s;
  background-color: #fff;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
  
  option {
    padding: 5px;
  }
  
  optgroup {
    font-weight: bold;
    color: #333;
  }
`;

const CatchItem = styled.div`
  background: #f9f9f9;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 15px;
`;

const CatchHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 10px;
  padding-bottom: 5px;
  border-bottom: 1px solid #eee;
`;

const CatchTitle = styled.div`
  font-weight: bold;
  color: #444;
`;

const ButtonContainer = styled.div`
  margin-top: 20px;
`;

export default TryCatchForm;