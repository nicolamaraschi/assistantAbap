import React from 'react';
import styled from 'styled-components';

const Documentation = () => {
  return (
    <DocContainer>
      <h3>Documentazione</h3>
      <p>
        Benvenuto nella documentazione del Generatore Avanzato di Costrutti ABAP.
        Questa applicazione ti permette di generare rapidamente codice ABAP
        correttamente formattato per vari costrutti.
      </p>
      
      <DocSection>
        <h4>Come utilizzare l'applicazione</h4>
        <ol>
          <li>Seleziona un tipo di costrutto dal menu a tendina</li>
          <li>Compila i campi del form specifico per quel costrutto</li>
          <li>Clicca su "Genera Codice" per vedere il risultato</li>
          <li>Puoi copiare il codice, scaricarlo o salvarlo come template</li>
        </ol>
      </DocSection>
      
      <DocSection>
        <h4>Tipi di Costrutti Disponibili</h4>
        <p>L'applicazione supporta i seguenti costrutti ABAP:</p>
        
        <CategorySection>
          <h5>Controllo Flusso</h5>
          <ul>
            <li><strong>IF-ELSE</strong> - Condizioni di base</li>
            <li><strong>CASE</strong> - Switch tra valori diversi</li>
            <li><strong>LOOP AT</strong> - Iterazione su tabelle interne</li>
            <li><strong>DO-ENDDO</strong> - Loop con contatore</li>
            <li><strong>WHILE</strong> - Loop con condizione</li>
          </ul>
        </CategorySection>
        
        <CategorySection>
          <h5>Manipolazione Dati</h5>
          <ul>
            <li><strong>SELECT</strong> - Query su database</li>
            <li><strong>UPDATE</strong> - Aggiornamento record</li>
            <li><strong>INSERT</strong> - Inserimento dati</li>
            <li><strong>MODIFY</strong> - Modifica dati</li>
            <li><strong>DELETE</strong> - Eliminazione dati</li>
          </ul>
        </CategorySection>
        
        <CategorySection>
          <h5>Strutture e Definizioni</h5>
          <ul>
            <li><strong>FORM</strong> - Subroutine</li>
            <li><strong>STRUCTURE</strong> - Definizione strutture</li>
            <li><strong>FIELD-SYMBOL</strong> - Field-symbols</li>
            <li><strong>INTERNAL TABLE</strong> - Tabelle interne</li>
            <li><strong>DATA Declaration</strong> - Dichiarazione variabili</li>
          </ul>
        </CategorySection>
        
        <CategorySection>
          <h5>OOP</h5>
          <ul>
            <li><strong>CLASS</strong> - Definizione classi</li>
            <li><strong>INTERFACE</strong> - Definizione interfacce</li>
            <li><strong>Method Chain</strong> - Concatenazione metodi</li>
            <li><strong>Method Definition</strong> - Definizione metodi</li>
          </ul>
        </CategorySection>
      </DocSection>
      
      <DocSection>
        <h4>Funzionalità Aggiuntive</h4>
        <ul>
          <li><strong>Preferiti</strong> - Salva i costrutti che usi più spesso</li>
          <li><strong>Template</strong> - Salva configurazioni complete per riutilizzarle</li>
          <li><strong>Cronologia</strong> - Accedi alle tue generazioni precedenti</li>
          <li><strong>Impostazioni</strong> - Personalizza l'applicazione (tema, formattazione)</li>
        </ul>
      </DocSection>
      
      <DocSection>
        <h4>Scorciatoie da tastiera</h4>
        <ul>
          <li><kbd>Ctrl</kbd> + <kbd>C</kbd> - Copia il codice generato</li>
          <li><kbd>Ctrl</kbd> + <kbd>S</kbd> - Salva come template</li>
          <li><kbd>Alt</kbd> + <kbd>G</kbd> - Genera codice</li>
          <li><kbd>Alt</kbd> + <kbd>1-5</kbd> - Cambia tab</li>
        </ul>
      </DocSection>
    </DocContainer>
  );
};

const DocContainer = styled.div`
  line-height: 1.6;
  
  h3 {
    margin-top: 0;
    margin-bottom: 15px;
  }
  
  p {
    margin-bottom: 15px;
  }
`;

const DocSection = styled.section`
  margin-bottom: 25px;
  
  h4 {
    margin-top: 0;
    margin-bottom: 10px;
    color: #0066cc;
  }
  
  ol, ul {
    margin-top: 5px;
    padding-left: 20px;
  }
  
  li {
    margin-bottom: 5px;
  }
  
  kbd {
    background-color: #f7f7f7;
    border: 1px solid #ccc;
    border-radius: 3px;
    box-shadow: 0 1px 0 rgba(0,0,0,0.2);
    color: #333;
    display: inline-block;
    font-size: 0.85em;
    font-weight: 700;
    line-height: 1;
    padding: 2px 4px;
    white-space: nowrap;
  }
`;

const CategorySection = styled.div`
  margin-bottom: 15px;
  
  h5 {
    margin-top: 10px;
    margin-bottom: 5px;
    color: #444;
  }
  
  ul {
    margin-top: 5px;
  }
`;

export default Documentation;