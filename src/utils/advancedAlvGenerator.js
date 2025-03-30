// src/utils/advancedAlvGenerator.js
// Implementazione completa del generatore di codice per ALV avanzato

/**
 * Genera codice ABAP per ALV avanzato in base alle opzioni configurate
 * @param {Object} formData - Dati dal form AdvancedAlvForm
 * @returns {String} - Codice ABAP generato
 */
export const generateAdvancedAlv = (formData) => {
    // Estrai valori principali dal formData
    const {
      alvType,
      tableName,
      structName,
      fieldCatalog,
      layout,
      events = {},
      sortingEnabled,
      sorting = [],
      filteringEnabled,
      filtering = [],
      subtotalsEnabled,
      subtotals = [],
      toolbarEnabled,
      toolbarButtons = [],
      editModeEnabled,
      registerEnterPressed,
      checkChanges,
      displayChanges,
      variantEnabled,
      variantHandle,
      variantReport,
      variantUsername,
      isOO,
      className,
      eventMethod,
      containerName,
      popupEnabled,
      popupStartCol,
      popupEndCol,
      popupStartLine,
      popupEndLine,
      useBuilder,
      outputType
    } = formData;
  
    // Determina quale approccio usare (OO vs procedurale)
    if (isOO) {
      return useBuilder 
        ? generateOOBuilderPattern(formData)
        : generateOOStandardPattern(formData);
    } else {
      return generateProceduralPattern(formData);
    }
  };
  
  /**
   * Genera codice usando Pattern Builder (fluent interface)
   */
  const generateOOBuilderPattern = (formData) => {
    const {
      alvType,
      tableName,
      structName,  // Aggiungi questa riga
      fieldCatalog,
      className,
      containerName,
      layout
    } = formData;
  
    let code = `CLASS ${className} DEFINITION.
    PUBLIC SECTION.
      METHODS:
        constructor,
        display_alv.
    
    PRIVATE SECTION.
      DATA:
        mo_alv    TYPE REF TO cl_gui_alv_grid,
        mo_custom TYPE REF TO cl_gui_custom_container,
        mt_fcat   TYPE lvc_t_fcat,
        ms_layout TYPE lvc_s_layo,
        mt_${tableName} TYPE STANDARD TABLE OF ${structName}.
  
      METHODS:
        build_fieldcatalog,
        build_layout,
        register_events.
  ENDCLASS.
  
  CLASS ${className} IMPLEMENTATION.
    METHOD constructor.
      " Inizializzazione
      build_fieldcatalog( ).
      build_layout( ).
    ENDMETHOD.
  
    METHOD display_alv.
      " Prepara i dati
      SELECT * FROM ${structName} INTO TABLE mt_${tableName}.
  
      " Crea container
      IF mo_custom IS NOT BOUND.
        CREATE OBJECT mo_custom
          EXPORTING
            container_name = '${containerName || 'CONTAINER'}'.
      ENDIF.
  
      " Crea ALV Grid
      IF mo_alv IS NOT BOUND.
        CREATE OBJECT mo_alv
          EXPORTING
            i_parent = mo_custom.
  
        " Registra gli eventi
        register_events( ).
  
        " Imposta i parametri dell'ALV e visualizzalo
        mo_alv->set_table_for_first_display(
          EXPORTING
            is_layout       = ms_layout
          CHANGING
            it_outtab       = mt_${tableName}
            it_fieldcatalog = mt_fcat ).
      ELSE.
        " Aggiorna ALV esistente
        mo_alv->refresh_table_display( ).
      ENDIF.
    ENDMETHOD.
  
    METHOD build_fieldcatalog.
      " Genera field catalog
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = '${formData.structName}'
        CHANGING
          ct_fieldcat      = mt_fcat.
  
      " Personalizza il field catalog
      LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
        CASE <fs_fcat>-fieldname.`;
  
    // Aggiungi personalizzazioni per ogni campo del field catalog
    fieldCatalog.forEach(field => {
      code += `
          WHEN '${field.fieldname}'.
            <fs_fcat>-seltext = '${field.seltext}'.`;
            
      if (field.outputlen) {
        code += `
            <fs_fcat>-outputlen = '${field.outputlen}'.`;
      }
      if (field.hotspot) {
        code += `
            <fs_fcat>-hotspot = 'X'.`;
      }
      if (field.checkbox) {
        code += `
            <fs_fcat>-checkbox = 'X'.`;
      }
      if (field.editable) {
        code += `
            <fs_fcat>-edit = 'X'.`;
      }
      if (field.key) {
        code += `
            <fs_fcat>-key = 'X'.`;
      }
      if (field.emphasize) {
        code += `
            <fs_fcat>-emphasize = 'X'.`;
      }
      if (field.noOut) {
        code += `
            <fs_fcat>-no_out = 'X'.`;
      }
      if (field.tech) {
        code += `
            <fs_fcat>-tech = 'X'.`;
      }
    });
  
    code += `
        ENDCASE.
      ENDLOOP.
    ENDMETHOD.
  
    METHOD build_layout.
      " Imposta layout
      ms_layout-zebra = '${layout.zebra ? 'X' : ''}'.
      ms_layout-cwidth_opt = '${layout.colwdOpt ? 'X' : ''}'.
      ms_layout-sel_mode = '${layout.selMode}'.
      ms_layout-grid_title = '${layout.gridTitle}'.`;
      
    if (layout.cellEdit) {
      code += `
      ms_layout-edit = 'X'.`;
    }
    if (layout.noHeaders) {
      code += `
      ms_layout-no_headers = 'X'.`;
    }
    if (layout.noMerging) {
      code += `
      ms_layout-no_merging = 'X'.`;
    }
    if (layout.sglClick) {
      code += `
      ms_layout-sgl_clk_hd = 'X'.`;
    }
    if (layout.noRowMark) {
      code += `
      ms_layout-no_rowmark = 'X'.`;
    }
    if (layout.smallTitle) {
      code += `
      ms_layout-smalltitle = 'X'.`;
    }
    
    code += `
    ENDMETHOD.
  
    METHOD register_events.
      " Registra eventi dell'ALV`;
      
    // Aggiungi registrazioni degli eventi selezionati
    if (formData.events.topOfPage) {
      code += `
      SET HANDLER handle_top_of_page FOR mo_alv.`;
    }
    if (formData.events.userCommand) {
      code += `
      SET HANDLER handle_user_command FOR mo_alv.`;
    }
    if (formData.events.doubleClick) {
      code += `
      SET HANDLER handle_double_click FOR mo_alv.`;
    }
    if (formData.events.hotspotClick) {
      code += `
      SET HANDLER handle_hotspot_click FOR mo_alv.`;
    }
    if (formData.events.dataChanged) {
      code += `
      SET HANDLER handle_data_changed FOR mo_alv.`;
    }
      
    code += `
    ENDMETHOD.
    
    " Altre implementazioni di metodi per gli handler di eventi...
  ENDCLASS.`;
  
    return code;
  };
  
  /**
   * Genera codice usando Pattern OO Standard
   */
  const generateOOStandardPattern = (formData) => {
    const {
      alvType,
      tableName,
      structName,
      fieldCatalog,
      layout,
      containerName
    } = formData;
    
    let code = `DATA: go_alv    TYPE REF TO cl_gui_alv_grid,
        go_custom TYPE REF TO cl_gui_custom_container,
        gt_fcat   TYPE lvc_t_fcat,
        gs_layout TYPE lvc_s_layo,
        gt_${tableName} TYPE STANDARD TABLE OF ${structName}.
  
  FORM build_fieldcatalog.
    " Genera field catalog
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '${structName}'
      CHANGING
        ct_fieldcat      = gt_fcat.
  
    " Personalizza il field catalog
    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      CASE <fs_fcat>-fieldname.`;
  
    // Aggiungi personalizzazioni per ogni campo del field catalog
    fieldCatalog.forEach(field => {
      code += `
        WHEN '${field.fieldname}'.
          <fs_fcat>-seltext = '${field.seltext}'.`;
          
      if (field.outputlen) {
        code += `
          <fs_fcat>-outputlen = '${field.outputlen}'.`;
      }
      if (field.hotspot) {
        code += `
          <fs_fcat>-hotspot = 'X'.`;
      }
      if (field.checkbox) {
        code += `
          <fs_fcat>-checkbox = 'X'.`;
      }
      if (field.editable) {
        code += `
          <fs_fcat>-edit = 'X'.`;
      }
      if (field.key) {
        code += `
          <fs_fcat>-key = 'X'.`;
      }
      if (field.emphasize) {
        code += `
          <fs_fcat>-emphasize = 'X'.`;
      }
      if (field.noOut) {
        code += `
          <fs_fcat>-no_out = 'X'.`;
      }
      if (field.tech) {
        code += `
          <fs_fcat>-tech = 'X'.`;
      }
    });
  
    code += `
      ENDCASE.
    ENDLOOP.
  ENDFORM.
  
  FORM build_layout.
    " Imposta layout
    gs_layout-zebra = '${layout.zebra ? 'X' : ''}'.
    gs_layout-cwidth_opt = '${layout.colwdOpt ? 'X' : ''}'.
    gs_layout-sel_mode = '${layout.selMode}'.
    gs_layout-grid_title = '${layout.gridTitle}'.`;
    
    if (layout.cellEdit) {
      code += `
    gs_layout-edit = 'X'.`;
    }
    if (layout.noHeaders) {
      code += `
    gs_layout-no_headers = 'X'.`;
    }
    if (layout.noMerging) {
      code += `
    gs_layout-no_merging = 'X'.`;
    }
    if (layout.sglClick) {
      code += `
    gs_layout-sgl_clk_hd = 'X'.`;
    }
    if (layout.noRowMark) {
      code += `
    gs_layout-no_rowmark = 'X'.`;
    }
    if (layout.smallTitle) {
      code += `
    gs_layout-smalltitle = 'X'.`;
    }
    
    code += `
  ENDFORM.
  
  FORM display_alv.
    " Prepara i dati
    SELECT * FROM ${structName} INTO TABLE gt_${tableName}.
  
    " Inizializza layout e field catalog
    PERFORM build_fieldcatalog.
    PERFORM build_layout.
  
    " Crea container
    IF go_custom IS NOT BOUND.
      CREATE OBJECT go_custom
        EXPORTING
          container_name = '${containerName || 'CONTAINER'}'.
    ENDIF.
  
    " Crea ALV Grid
    IF go_alv IS NOT BOUND.
      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_custom.
  
      " Impostazioni ALV
      SET HANDLER handle_user_command FOR go_alv.`;
      
    if (formData.events.topOfPage) {
      code += `
      SET HANDLER handle_top_of_page FOR go_alv.`;
    }
    if (formData.events.doubleClick) {
      code += `
      SET HANDLER handle_double_click FOR go_alv.`;
    }
    if (formData.events.hotspotClick) {
      code += `
      SET HANDLER handle_hotspot_click FOR go_alv.`;
    }
    if (formData.events.dataChanged) {
      code += `
      SET HANDLER handle_data_changed FOR go_alv.`;
    }
    
    code += `
      go_alv->set_table_for_first_display(
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_outtab       = gt_${tableName}
          it_fieldcatalog = gt_fcat ).
    ELSE.
      " Aggiorna ALV esistente
      go_alv->refresh_table_display( ).
    ENDIF.
  ENDFORM.`;
  
    return code;
  };
  
  /**
   * Genera codice usando Pattern Procedurale
   */
  const generateProceduralPattern = (formData) => {
    const {
      alvType,
      tableName,
      structName,
      fieldCatalog,
      layout,
      events = {},
      sortingEnabled,
      sorting = [],
      filteringEnabled,
      filtering = [],
      subtotalsEnabled,
      subtotals = [],
      variantEnabled,
      variantHandle,
      variantReport,
      variantUsername,
      outputType
    } = formData;
  
    let code = `*&---------------------------------------------------------------------*
  *& ALV Grid Report
  *&---------------------------------------------------------------------*
  REPORT z_alv_grid.
  
  * Dichiarazione dei tipi e delle tabelle
  TYPES: BEGIN OF ty_${structName},`;
  
    // Genera struttura in base ai campi del field catalog
    fieldCatalog.forEach((field, index) => {
      code += `
         ${field.fieldname} TYPE ${field.datatype}`;
      if (index < fieldCatalog.length - 1) {
        code += ',';
      }
    });
    
    code += `
       END OF ty_${structName}.
  
  * Dichiarazione dei dati
  DATA: gt_${tableName} TYPE TABLE OF ty_${structName},
        gs_${tableName} TYPE ty_${structName},
        gt_fieldcat TYPE lvc_t_fcat,
        gs_layout   TYPE lvc_s_layo,`;
        
    if (sortingEnabled) {
      code += `
        gt_sort     TYPE lvc_t_sort,`;
    }
    
    if (filteringEnabled) {
      code += `
        gt_filter   TYPE lvc_t_filt,`;
    }
    
    if (variantEnabled) {
      code += `
        gs_variant  TYPE disvariant,`;
    }
    
    code += `
        gv_repid    TYPE sy-repid.
  
  * Ottieni l'ID del programma
  gv_repid = sy-repid.
  
  *&---------------------------------------------------------------------*
  *& Inizializzazione dello schermo
  *&---------------------------------------------------------------------*
  INITIALIZATION.
    " Inizializza impostazioni
  
  START-OF-SELECTION.
    " Ottieni dati da visualizzare
    PERFORM get_data.
    
    " Visualizza ALV
    PERFORM display_alv.
  
  *&---------------------------------------------------------------------*
  *& Form GET_DATA
  *&---------------------------------------------------------------------*
  FORM get_data.
    " Recupera dati (esempio)
    SELECT * FROM ${structName.toUpperCase()} INTO TABLE gt_${tableName}.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form DISPLAY_ALV
  *&---------------------------------------------------------------------*
  FORM display_alv.
    " Imposta field catalog
    PERFORM build_fieldcatalog.
    
    " Imposta layout
    PERFORM build_layout.`;
    
    if (sortingEnabled) {
      code += `
    
    " Imposta ordinamento
    PERFORM build_sort.`;
    }
    
    if (filteringEnabled) {
      code += `
    
    " Imposta filtri
    PERFORM build_filter.`;
    }
    
    if (variantEnabled) {
      code += `
    
    " Imposta variante
    gs_variant-report = ${variantReport}.
    gs_variant-handle = '${variantHandle}'.
    gs_variant-username = ${variantUsername}.`;
    }
    
    // Output dell'ALV in base al tipo selezionato
    switch (outputType) {
      case 'GRID':
        code += `
    
    " Visualizza ALV Grid
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = gv_repid`;
        
        if (events.userCommand) {
          code += `
        i_callback_user_command = 'USER_COMMAND'`;
        }
        
        if (events.topOfPage) {
          code += `
        i_callback_top_of_page = 'TOP_OF_PAGE'`;
        }
        
        code += `
        is_layout              = gs_layout
        it_fieldcat            = gt_fieldcat`;
        
        if (sortingEnabled) {
          code += `
        it_sort                = gt_sort`;
        }
        
        if (filteringEnabled) {
          code += `
        it_filter              = gt_filter`;
        }
        
        if (variantEnabled) {
          code += `
        is_variant             = gs_variant
        i_save                 = 'A'`;
        }
        
        code += `
      TABLES
        t_outtab               = gt_${tableName}
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.`;
        break;
        
      case 'GRID_DISPLAY':
        code += `
    
    " Visualizza ALV Grid LVC
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program     = gv_repid`;
        
        if (events.userCommand) {
          code += `
        i_callback_user_command = 'USER_COMMAND'`;
        }
        
        if (events.topOfPage) {
          code += `
        i_callback_top_of_page = 'TOP_OF_PAGE'`;
        }
        
        code += `
        is_layout_lvc          = gs_layout
        it_fieldcat_lvc        = gt_fieldcat`;
        
        if (sortingEnabled) {
          code += `
        it_sort_lvc            = gt_sort`;
        }
        
        if (filteringEnabled) {
          code += `
        it_filter_lvc          = gt_filter`;
        }
        
        if (variantEnabled) {
          code += `
        is_variant             = gs_variant
        i_save                 = 'A'`;
        }
        
        code += `
      TABLES
        t_outtab               = gt_${tableName}
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.`;
        break;
        
      case 'LIST':
        code += `
    
    " Visualizza ALV List
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program     = gv_repid`;
        
        if (events.userCommand) {
          code += `
        i_callback_user_command = 'USER_COMMAND'`;
        }
        
        if (events.topOfPage) {
          code += `
        i_callback_top_of_page = 'TOP_OF_PAGE'`;
        }
        
        code += `
        is_layout              = gs_layout
        it_fieldcat            = gt_fieldcat`;
        
        if (sortingEnabled) {
          code += `
        it_sort                = gt_sort`;
        }
        
        if (filteringEnabled) {
          code += `
        it_filter              = gt_filter`;
        }
        
        if (variantEnabled) {
          code += `
        is_variant             = gs_variant
        i_save                 = 'A'`;
        }
        
        code += `
      TABLES
        t_outtab               = gt_${tableName}
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.`;
        break;
        
      default:
        code += `
    
    " Visualizza ALV Grid (default)
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = gv_repid
        is_layout              = gs_layout
        it_fieldcat            = gt_fieldcat
      TABLES
        t_outtab               = gt_${tableName}
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.`;
    }
    
    code += `
  
    IF sy-subrc <> 0.
      MESSAGE 'Errore nella visualizzazione ALV' TYPE 'E'.
    ENDIF.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form BUILD_FIELDCATALOG
  *&---------------------------------------------------------------------*
  FORM build_fieldcatalog.
    " Genera field catalog da struttura
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '${structName.toUpperCase()}'
      CHANGING
        ct_fieldcat      = gt_fieldcat.
  
    " Personalizza il field catalog
    LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      CASE <fs_fcat>-fieldname.`;
  
    // Aggiungi personalizzazioni per ogni campo del field catalog
    fieldCatalog.forEach(field => {
      code += `
        WHEN '${field.fieldname}'.
          <fs_fcat>-seltext = '${field.seltext}'.`;
          
      if (field.outputlen) {
        code += `
          <fs_fcat>-outputlen = '${field.outputlen}'.`;
      }
      if (field.hotspot) {
        code += `
          <fs_fcat>-hotspot = 'X'.`;
      }
      if (field.checkbox) {
        code += `
          <fs_fcat>-checkbox = 'X'.`;
      }
      if (field.editable) {
        code += `
          <fs_fcat>-edit = 'X'.`;
      }
      if (field.key) {
        code += `
          <fs_fcat>-key = 'X'.`;
      }
      if (field.emphasize) {
        code += `
          <fs_fcat>-emphasize = 'X'.`;
      }
      if (field.noOut) {
        code += `
          <fs_fcat>-no_out = 'X'.`;
      }
      if (field.tech) {
        code += `
          <fs_fcat>-tech = 'X'.`;
      }
      if (field.coltext) {
        code += `
          <fs_fcat>-coltext = '${field.coltext}'.`;
      }
      if (field.tipText) {
        code += `
          <fs_fcat>-tooltip = '${field.tipText}'.`;
      }
    });
  
    code += `
      ENDCASE.
    ENDLOOP.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form BUILD_LAYOUT
  *&---------------------------------------------------------------------*
  FORM build_layout.
    " Imposta layout
    gs_layout-zebra = '${layout.zebra ? 'X' : ''}'.
    gs_layout-cwidth_opt = '${layout.colwdOpt ? 'X' : ''}'.
    gs_layout-sel_mode = '${layout.selMode}'.
    gs_layout-grid_title = '${layout.gridTitle}'.`;
    
    if (layout.cellEdit) {
      code += `
    gs_layout-edit = 'X'.`;
    }
    if (layout.noHeaders) {
      code += `
    gs_layout-no_headers = 'X'.`;
    }
    if (layout.noMerging) {
      code += `
    gs_layout-no_merging = 'X'.`;
    }
    if (layout.sglClick) {
      code += `
    gs_layout-sgl_clk_hd = 'X'.`;
    }
    if (layout.noRowMark) {
      code += `
    gs_layout-no_rowmark = 'X'.`;
    }
    if (layout.smallTitle) {
      code += `
    gs_layout-smalltitle = 'X'.`;
    }
    if (layout.styleName) {
      code += `
    gs_layout-stylefname = '${layout.styleName}'.`;
    }
    if (layout.excelDisp) {
      code += `
    gs_layout-xls_format = 'X'.`;
    }
    if (layout.webDisp) {
      code += `
    gs_layout-web_format = 'X'.`;
    }
    
    code += `
  ENDFORM.`;
  
    // Aggiungi FORM per l'ordinamento se necessario
    if (sortingEnabled && sorting.length > 0) {
      code += `
  
  *&---------------------------------------------------------------------*
  *& Form BUILD_SORT
  *&---------------------------------------------------------------------*
  FORM build_sort.
    DATA: ls_sort TYPE lvc_s_sort.
  
    " Pulisci tabella
    CLEAR gt_sort.`;
      
      sorting.forEach((sort, index) => {
        code += `
    
    " Ordinamento campo ${sort.fieldname}
    ls_sort-spos = ${index + 1}.
    ls_sort-fieldname = '${sort.fieldname}'.
    ls_sort-up = '${sort.sequence === 'ASCENDING' ? 'X' : ''}'. 
    ls_sort-down = '${sort.sequence === 'DESCENDING' ? 'X' : ''}'. 
    APPEND ls_sort TO gt_sort.`;
      });
      
      code += `
  ENDFORM.`;
    }
  
    // Aggiungi FORM per il filtro se necessario
    if (filteringEnabled && filtering.length > 0) {
      code += `
  
  *&---------------------------------------------------------------------*
  *& Form BUILD_FILTER
  *&---------------------------------------------------------------------*
  FORM build_filter.
    DATA: ls_filter TYPE lvc_s_filt.
  
    " Pulisci tabella
    CLEAR gt_filter.`;
      
      filtering.forEach((filter, index) => {
        code += `
    
    " Filtro campo ${filter.fieldname}
    ls_filter-fieldname = '${filter.fieldname}'.
    ls_filter-sign = '${filter.sign}'.
    ls_filter-option = '${filter.option}'.
    ls_filter-low = '${filter.low}'.${filter.high ? `
    ls_filter-high = '${filter.high}'.` : ''}
    APPEND ls_filter TO gt_filter.`;
      });
      
      code += `
  ENDFORM.`;
    }
  
    // Aggiungi handlers per gli eventi se necessario
    if (events.userCommand) {
      code += `
  
  *&---------------------------------------------------------------------*
  *& Form USER_COMMAND
  *&---------------------------------------------------------------------*
  FORM user_command USING r_ucomm LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.
  
    CASE r_ucomm.
      WHEN '&IC1'. " Doppio click
        " Gestione doppio click
        
      WHEN 'DETAIL'.
        " Gestione pulsante dettaglio
        
      WHEN OTHERS.
        " Altre azioni
        
    ENDCASE.
  ENDFORM.`;
    }
  
    if (events.topOfPage) {
      code += `
  
  *&---------------------------------------------------------------------*
  *& Form TOP_OF_PAGE
  *&---------------------------------------------------------------------*
  FORM top_of_page.
    DATA: lt_list_commentary TYPE slis_t_listheader,
          ls_list_commentary TYPE slis_listheader.
  
    " Intestazione
    ls_list_commentary-typ = 'H'.
    ls_list_commentary-info = '${layout.gridTitle}'.
    APPEND ls_list_commentary TO lt_list_commentary.
    CLEAR ls_list_commentary.
  
    " Informazioni aggiuntive
    ls_list_commentary-typ = 'S'.
    ls_list_commentary-info = 'Generato il ' && sy-datum.
    APPEND ls_list_commentary TO lt_list_commentary.
    CLEAR ls_list_commentary.
  
    " Output intestazione
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_list_commentary.
  ENDFORM.`;
    }
  
    return code;
  };
  
