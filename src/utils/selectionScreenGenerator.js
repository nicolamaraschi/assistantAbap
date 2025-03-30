// src/utils/selectionScreenGenerator.js

/**
 * Genera codice ABAP per una Selection Screen basato sulla configurazione
 * @param {Object} formData - Configurazione per la selection screen
 * @returns {String} Codice ABAP generato
 */
export function generateSelectionScreen(formData) {
    const {
      reportTitle,
      reportDescription,
      parameters = [],
      selectOptions = [],
      blocks = [],
      includeVariants = true,
      useSelectionScreen = true,
      addAtSelection = false,
      atSelectionText = ''
    } = formData;
  
    // Inizia con l'intestazione del report
    let code = `*&---------------------------------------------------------------------*
  *& Report ${reportTitle}
  *&---------------------------------------------------------------------*
  *& ${reportDescription}
  *&---------------------------------------------------------------------*
  REPORT ${reportTitle}.
  
  `;
  
    // Aggiungi dichiarazioni di tipo/dati per i parametri
    code += `*----------------------------------------------------------------------*
  * Type Pools and Data Declarations
  *----------------------------------------------------------------------*
  DATA: gv_ok_code TYPE sy-ucomm.
  
  `;
  
    // Includi la gestione delle varianti se richiesto
    if (includeVariants) {
      code += `* Variant handling
  DATA: gs_variant TYPE disvariant,
        g_save     TYPE c LENGTH 1.
  
  * Initialize variant
  gs_variant-report = sy-repid.
  g_save = 'A'.
  
  `;
    }
  
    // Genera dichiarazioni PARAMETERS
    if (parameters.length > 0) {
      code += `*----------------------------------------------------------------------*
  * Parameters
  *----------------------------------------------------------------------*
  `;
  
      parameters.forEach(param => {
        let paramLine = `PARAMETERS: ${param.name} TYPE ${param.type}`;
        
        // Aggiungi opzioni
        if (param.defaultValue) {
          paramLine += ` DEFAULT ${param.defaultValue}`;
        }
        
        if (param.obligatory) {
          paramLine += " OBLIGATORY";
        }
        
        if (param.lowerCase) {
          paramLine += " LOWER CASE";
        }
        
        if (param.checkBox) {
          paramLine += " AS CHECKBOX";
        }
        
        if (param.radioButton) {
          paramLine += ` RADIOBUTTON GROUP ${param.radioGroup || 'g1'}`;
        }
        
        if (param.visibleLength) {
          paramLine += ` LENGTH ${param.visibleLength}`;
        }
        
        // Aggiungi descrizione parametro
        paramLine += ` MODIF ID bas.`;
        
        if (param.matchcode) {
          paramLine += `
  PARAMETER-ID ${param.matchcode}.`;
        }
        
        code += `${paramLine}\n`;
      });
      
      code += `\n`;
    }
  
    // Genera dichiarazioni SELECT-OPTIONS
    if (selectOptions.length > 0) {
      code += `*----------------------------------------------------------------------*
  * Select Options
  *----------------------------------------------------------------------*
  `;
  
      selectOptions.forEach(selOpt => {
        let selOptLine = `SELECT-OPTIONS: ${selOpt.name} FOR DATABASE-FIELD ${selOpt.type}`;
        
        // Aggiungi opzioni
        if (selOpt.noExtension) {
          selOptLine += " NO EXTENSION";
        }
        
        if (selOpt.noIntervals) {
          selOptLine += " NO INTERVALS";
        }
        
        if (!selOpt.multiple) {
          selOptLine += " NO-EXTENSION";
        }
        
        if (selOpt.obligatory) {
          selOptLine += " OBLIGATORY";
        }
        
        // Aggiungi descrizione parametro
        selOptLine += ` MODIF ID bas.`;
        
        if (selOpt.matchcode) {
          selOptLine += `
  PARAMETER-ID ${selOpt.matchcode}.`;
        }
        
        code += `${selOptLine}\n`;
      });
      
      code += `\n`;
    }
  
    // Genera blocchi SELECTION-SCREEN
    if (useSelectionScreen && blocks.length > 0) {
      code += `*----------------------------------------------------------------------*
  * Selection Screen Definition
  *----------------------------------------------------------------------*
  `;
  
      blocks.forEach(block => {
        // Inizia blocco
        code += `SELECTION-SCREEN BEGIN OF BLOCK ${block.title.replace(/\s+/g, '_')}`;
        
        if (block.useFrame) {
          code += ` WITH FRAME TITLE TEXT-b01. "${block.frameTitle}"`;
        } else {
          code += `.`;
        }
        
        code += `\n`;
        
        // Includi parametri in questo blocco
        if (block.parameters && block.parameters.length > 0) {
          code += `SELECTION-SCREEN BEGIN OF LINE.\n`;
          
          block.parameters.forEach(paramId => {
            const param = parameters.find(p => p.id === paramId);
            if (param) {
              code += `  SELECTION-SCREEN COMMENT 1(30) TEXT-p${String(paramId).padStart(2, '0')} FOR FIELD ${param.name}. "${param.label}"\n`;
            }
          });
          
          code += `SELECTION-SCREEN END OF LINE.\n`;
        }
        
        // Includi select-options in questo blocco
        if (block.selectOptions && block.selectOptions.length > 0) {
          code += `SELECTION-SCREEN BEGIN OF LINE.\n`;
          
          block.selectOptions.forEach(selOptId => {
            const selOpt = selectOptions.find(s => s.id === selOptId);
            if (selOpt) {
              code += `  SELECTION-SCREEN COMMENT 1(30) TEXT-s${String(selOptId).padStart(2, '0')} FOR FIELD ${selOpt.name}-low. "${selOpt.label}"\n`;
            }
          });
          
          code += `SELECTION-SCREEN END OF LINE.\n`;
        }
        
        // Fine blocco
        code += `SELECTION-SCREEN END OF BLOCK ${block.title.replace(/\s+/g, '_')}.\n\n`;
      });
    }
  
    // Aggiungi gestore AT SELECTION-SCREEN se richiesto
    if (addAtSelection) {
      code += `*----------------------------------------------------------------------*
  * AT SELECTION-SCREEN
  *----------------------------------------------------------------------*
  AT SELECTION-SCREEN.
    ${atSelectionText || '* Handle selection screen input here'}\n\n`;
    }
  
    // Aggiungi flusso principale del programma
    code += `*----------------------------------------------------------------------*
  * Initialization
  *----------------------------------------------------------------------*
  INITIALIZATION.
    ${includeVariants ? 'PERFORM get_variant.' : '* Initialization logic goes here'}\n\n`;
  
    // Aggiungi START-OF-SELECTION
    code += `*----------------------------------------------------------------------*
  * Start of selection
  *----------------------------------------------------------------------*
  START-OF-SELECTION.
    PERFORM get_data.
    PERFORM display_data.\n\n`;
  
    // Aggiungi subroutine di base
    code += `*&---------------------------------------------------------------------*
  *& Form get_data
  *&---------------------------------------------------------------------*
  FORM get_data.
    * Get data based on selection criteria
  ENDFORM.\n\n`;
  
    code += `*&---------------------------------------------------------------------*
  *& Form display_data
  *&---------------------------------------------------------------------*
  FORM display_data.
    * Display data in ALV or other format
  ENDFORM.\n\n`;
  
    // Aggiungi subroutine per gestione varianti se richiesto
    if (includeVariants) {
      code += `*&---------------------------------------------------------------------*
  *& Form get_variant
  *&---------------------------------------------------------------------*
  FORM get_variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gs_variant
      EXCEPTIONS
        not_found  = 2.
  ENDFORM.\n`;
    }
  
    return code;
  }
  
  // Esporta anche come default per compatibilità
  const selectionScreenUtils = { generateSelectionScreen };
  export default selectionScreenUtils.generateSelectionScreen;