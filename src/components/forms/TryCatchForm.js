// TryCatchForm.js - Migliorato con supporto RESUME
import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import FormGroup from '../common/FormGroup';
import Button from '../common/Button';
import { FiPlus, FiTrash2 } from 'react-icons/fi';
import { useAbap } from '../../context/AbapContext';
import ControlledInput from '../common/ControlledInput';
import ControlledTextarea from '../common/ControlledTextarea';

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
                <ControlledInput
                  type="text"
                  value={catchBlock.className}
                  onChange={(e) => handleCatchChange(catchBlock.id, 'className', e.target.value)}
                />
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
            <ControlledInput
              type="text"
              value={formData.catchBlocks[0].className}
              onChange={(e) => handleCatchChange(formData.catchBlocks[0].id, 'className', e.target.value)}
            />
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