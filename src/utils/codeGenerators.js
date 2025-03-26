// Collezione di funzioni per generare codice ABAP
// Ogni funzione accetta un oggetto con i dati del form e restituisce una stringa di codice

// Gestione generica di un qualsiasi costrutto
export const generateGeneric = (formData) => {
    // Se l'utente ha inserito del contenuto personalizzato, lo utilizziamo
    if (formData.content) {
      return formData.content;
    }
    
    // Altrimenti, restituiamo un messaggio generico
    return '* Contenuto generico per il costrutto selezionato';
  };
  
  // IF-ELSE
  export const generateIfElse = (formData) => {
    const { condition, trueAction, falseAction, addElseIf, elseIfCondition, elseIfAction } = formData;
    
    let code = `IF ${condition}.
    ${trueAction}`;
    
    if (addElseIf) {
      code += `
  ELSEIF ${elseIfCondition}.
    ${elseIfAction}`;
    }
    
    code += `
  ELSE.
    ${falseAction}
  ENDIF.`;
    
    return code;
  };
  
  // CASE
  export const generateCase = (formData) => {
    const { variable, cases, defaultAction } = formData;
    
    let code = `CASE ${variable}.`;
    
    for (const caseItem of cases) {
      code += `
    WHEN ${caseItem.value}.
      ${caseItem.action}`;
    }
    
    if (defaultAction) {
      code += `
    WHEN OTHERS.
      ${defaultAction}`;
    }
    
    code += `
  ENDCASE.`;
    
    return code;
  };
  
  // LOOP AT
  export const generateLoopAt = (formData) => {
    const { table, variable, whereCondition, content, useAssigning, addIndex } = formData;
    
    let code = `LOOP AT ${table}`;
    
    if (useAssigning) {
      code += ` ASSIGNING FIELD-SYMBOL(<${variable}>)`;
    } else {
      code += ` INTO ${variable}`;
    }
    
    if (whereCondition) {
      code += ` WHERE ${whereCondition}`;
    }
    
    if (addIndex) {
      code += ` ASSIGNING FIELD-SYMBOL(<fs>) FROM 1 TO 10`;
    }
    
    code += `.
    ${content}
  ENDLOOP.`;
    
    return code;
  };
  
  // DO-ENDDO
  export const generateDoEnddo = (formData) => {
    const { times, condition, content, addExitAt } = formData;
    
    let code = `DO`;
    
    if (times) {
      code += ` ${times} TIMES`;
    }
    
    code += `.
    ${content}`;
    
    if (condition && !times) {
      code += `
    
    CHECK ${condition}.`;
    }
    
    if (addExitAt) {
      code += `
    
    IF sy-index = 5.
      EXIT.
    ENDIF.`;
    }
    
    code += `
  ENDDO.`;
    
    return code;
  };
  
  // WHILE
  export const generateWhile = (formData) => {
    const { condition, content } = formData;
    
    return `WHILE ${condition}.
    ${content}
  ENDWHILE.`;
  };
  
  // SELECT
  export const generateSelect = (formData) => {
    const { fields, table, into, where, orderBy, groupBy, having, addJoin, joinType, joinTable, joinCondition } = formData;
    
    let code = `SELECT ${fields}`;
    
    if (addJoin) {
      code += `
    FROM ${table}
    ${joinType} ${joinTable} ON ${joinCondition}`;
    } else {
      code += `
    FROM ${table}`;
    }
    
    code += `
    INTO ${into}`;
    
    if (where) {
      code += `
    WHERE ${where}`;
    }
    
    if (groupBy) {
      code += `
    GROUP BY ${groupBy}`;
    }
    
    if (having) {
      code += `
    HAVING ${having}`;
    }
    
    if (orderBy) {
      code += `
    ORDER BY ${orderBy}`;
    }
    
    code += `.`;
    
    return code;
  };
  
  // UPDATE
  export const generateUpdate = (formData) => {
    const { table, fields, where } = formData;
    
    return `UPDATE ${table} SET
    ${fields}
  WHERE ${where}.`;
  };
  
  // INSERT
  export const generateInsert = (formData) => {
    const { table, insertType, fields, source } = formData;
    
    let code = `INSERT INTO ${table}`;
    
    if (insertType === 'values') {
      code += ` VALUES (
    ${fields}
  ).`;
    } else {
      code += ` FROM TABLE ${source}.`;
    }
    
    return code;
  };
  
  // MODIFY
  export const generateModify = (formData) => {
    const { table, source, index, addTransporting, fields } = formData;
    
    let code = `MODIFY ${table} FROM ${source}`;
    
    if (index) {
      code += ` INDEX ${index}`;
    }
    
    if (addTransporting) {
      code += ` TRANSPORTING ${fields}`;
    }
    
    code += `.`;
    
    return code;
  };
  
  // DELETE
  export const generateDelete = (formData) => {
    const { table, deleteType, where, source, index } = formData;
    
    let code = `DELETE FROM ${table}`;
    
    if (deleteType === 'where') {
      code += ` WHERE ${where}.`;
    } else {
      code += index ? ` INDEX ${index}.` : ` FROM TABLE ${source}.`;
    }
    
    return code;
  };
  
  // FORM
  export const generateForm = (formData) => {
    const { name, params, content } = formData;
    
    let code = `FORM ${name}`;
    
    if (params) {
      code += ` USING ${params}`;
    }
    
    code += `.
    ${content}
  ENDFORM.`;
    
    return code;
  };
  
  // STRUCTURE
  export const generateStructure = (formData) => {
    const { name, components } = formData;
    
    let code = `TYPES: BEGIN OF ${name},`;
    
    for (let i = 0; i < components.length; i++) {
      const component = components[i];
      code += `
           ${component.name} TYPE ${component.type}`;
      if (i < components.length - 1) {
        code += ',';
      }
    }
    
    code += `
         END OF ${name}.`;
    
    return code;
  };
  
  // FIELD-SYMBOL
  export const generateFieldSymbol = (formData) => {
    const { name, type } = formData;
    
    return `FIELD-SYMBOLS: <${name}> TYPE ${type}.`;
  };
  
  // INTERNAL TABLE
  export const generateInternalTable = (formData) => {
    const { name, type, isStandard, keyType, initialSize } = formData;
    
    let code = `DATA: ${name} TYPE`;
    
    if (isStandard) {
      code += ` STANDARD`;
    } else {
      code += ` SORTED`;
    }
    
    code += ` TABLE OF ${type}`;
    
    if (keyType) {
      code += ` WITH ${keyType} KEY`;
    }
    
    if (initialSize) {
      code += ` INITIAL SIZE ${initialSize}`;
    }
    
    code += `.`;
    
    return code;
  };
  
  // DATA Declaration
  export const generateDataDeclaration = (formData) => {
    const { variables } = formData;
    
    let code = `DATA: `;
    
    const declarations = variables.map(variable => {
      let declaration = `${variable.name} TYPE ${variable.type}`;
      
      if (variable.initialValue) {
        declaration += ` VALUE ${variable.initialValue}`;
      }
      
      return declaration;
    });
    
    code += declarations.join(',\n      ');
    code += '.';
    
    return code;
  };
  
  // TRY-CATCH
  export const generateTryCatch = (formData) => {
    const { tryBlock, catchBlock, catchClass, cleanup } = formData;
    
    let code = `TRY.
    ${tryBlock}
  CATCH ${catchClass}.
    ${catchBlock}`;
    
    if (cleanup) {
      code += `
  CLEANUP.
    ${cleanup}`;
    }
    
    code += `
  ENDTRY.`;
    
    return code;
  };
  
  // RAISE EXCEPTION
  export const generateRaiseException = (formData) => {
    const { exceptionClass, text, exporting } = formData;
    
    let code = `RAISE EXCEPTION TYPE ${exceptionClass}`;
    
    if (text) {
      code += `\n  EXPORTING\n    textid = ${text}`;
    }
    
    if (exporting) {
      if (!text) {
        code += `\n  EXPORTING`;
      }
      code += `\n    ${exporting}`;
    }
    
    code += `.`;
    
    return code;
  };
  
  // MESSAGE
  export const generateMessage = (formData) => {
    const { type, id, number, with1, with2, with3, with4 } = formData;
    
    let code = `MESSAGE ${id}${number}`;
    
    if (with1 || with2 || with3 || with4) {
      const withValues = [];
      if (with1) withValues.push(with1);
      if (with2) withValues.push(with2);
      if (with3) withValues.push(with3);
      if (with4) withValues.push(with4);
      
      code += ` WITH ${withValues.join(' ')}`;
    }
    
    code += ` ${type}.`;
    
    return code;
  };
  
  // CLASS
  export const generateClass = (formData) => {
    const { name, definition, visibility, superclass, inheritance, interfaces, methods, attributes } = formData;
    
    let code = `CLASS ${name} ${definition}`;
    
    if (superclass) {
      code += ` INHERITING FROM ${superclass}`;
    }
    
    if (interfaces) {
      code += ` INTERFACES ${interfaces}`;
    }
    
    code += `.\n`;
    
    // Attributi della classe
    if (attributes && attributes.length > 0) {
      code += `  PUBLIC SECTION.\n`;
      for (const attr of attributes.filter(a => a.visibility === 'PUBLIC')) {
        code += `    DATA: ${attr.name} TYPE ${attr.type}.\n`;
      }
      
      code += `\n  PROTECTED SECTION.\n`;
      for (const attr of attributes.filter(a => a.visibility === 'PROTECTED')) {
        code += `    DATA: ${attr.name} TYPE ${attr.type}.\n`;
      }
      
      code += `\n  PRIVATE SECTION.\n`;
      for (const attr of attributes.filter(a => a.visibility === 'PRIVATE')) {
        code += `    DATA: ${attr.name} TYPE ${attr.type}.\n`;
      }
    }
    
    // Metodi della classe
    if (methods && methods.length > 0) {
      code += `\n  METHODS:\n`;
      
      const methodDeclarations = methods.map(method => {
        let methodDecl = `    ${method.name}`;
        
        if (method.importing) {
          methodDecl += `\n      IMPORTING ${method.importing}`;
        }
        
        if (method.exporting) {
          methodDecl += `\n      EXPORTING ${method.exporting}`;
        }
        
        if (method.returning) {
          methodDecl += `\n      RETURNING VALUE(${method.returning.name}) TYPE ${method.returning.type}`;
        }
        
        return methodDecl;
      });
      
      code += methodDeclarations.join(',\n\n');
      code += `.`;
    }
    
    code += `\nENDCLASS.`;
    
    return code;
  };
  
  // INTERFACE
  export const generateInterface = (formData) => {
    const { name, methods } = formData;
    
    let code = `INTERFACE ${name}.\n`;
    
    if (methods && methods.length > 0) {
      code += `  METHODS:\n`;
      
      const methodDeclarations = methods.map(method => {
        let methodDecl = `    ${method.name}`;
        
        if (method.importing) {
          methodDecl += `\n      IMPORTING ${method.importing}`;
        }
        
        if (method.exporting) {
          methodDecl += `\n      EXPORTING ${method.exporting}`;
        }
        
        if (method.returning) {
          methodDecl += `\n      RETURNING VALUE(${method.returning.name}) TYPE ${method.returning.type}`;
        }
        
        return methodDecl;
      });
      
      code += methodDeclarations.join(',\n\n');
      code += `.`;
    }
    
    code += `\nENDINTERFACE.`;
    
    return code;
  };
  
  // Method Definition
  export const generateMethodDefinition = (formData) => {
    const { className, methodName, importing, exporting, returning, raising, content } = formData;
    
    let code = `METHOD ${className}=>${methodName}`;
    
    if (importing) {
      code += `.\n  ${importing}`;
    }
    
    if (exporting) {
      code += `.\n  ${exporting}`;
    }
    
    if (returning) {
      code += `.\n  ${returning}`;
    }
    
    if (raising) {
      code += `.\n  ${raising}`;
    }
    
    code += `.\n\n  ${content}\n\nENDMETHOD.`;
    
    return code;
  };
  
  // Method Chain
  export const generateMethodChain = (formData) => {
    const { object, methods } = formData;
    
    let code = `DATA(result) = ${object}`;
    
    for (const method of methods) {
      code += `\n  ->${method.name}(`;
      
      if (method.params && method.params.length > 0) {
        const params = method.params.map(param => `${param.name} = ${param.value}`);
        code += `\n    ${params.join(',\n    ')}`;
      }
      
      code += `)`;
    }
    
    code += `.`;
    
    return code;
  };
  
  // ALV Grid
  export const generateAlvGrid = (formData) => {
    const { tableName, fieldCatalog, layout, variant } = formData;
    
    return `DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_layout   TYPE lvc_s_layo,
        lt_sort     TYPE lvc_t_sort.
  
  * Create field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = '${fieldCatalog}'
    CHANGING
      ct_fieldcat      = lt_fieldcat.
  
  * Set layout
  ls_layout-zebra      = 'X'.
  ls_layout-cwidth_opt = 'X'.
  ls_layout-sel_mode   = 'A'.
  ${layout ? layout : ''}
  
  * Display ALV Grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = ls_layout
      it_fieldcat_lvc    = lt_fieldcat
      i_save             = 'A'
      is_variant         = ${variant ? variant : 'ls_variant'}
    TABLES
      t_outtab           = ${tableName}
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.`;
  };
  
  // BAPI Call
  export const generateBapiCall = (formData) => {
    const { bapiName, imports, tables, exports } = formData;
    
    let code = `* Call BAPI ${bapiName}
  DATA: ls_return TYPE bapiret2.
  
  CALL FUNCTION '${bapiName}'
    EXPORTING
      ${imports ? imports : '* Add import parameters here'}
    IMPORTING
      return        = ls_return
      ${exports ? exports : '* Add export parameters here'}
    TABLES
      ${tables ? tables : '* Add table parameters here'}.
  
  * Process return message
  IF ls_return-type = 'E'.
    MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
       WITH ls_return-message_v1 ls_return-message_v2
            ls_return-message_v3 ls_return-message_v4.
  ENDIF.`;
  
    return code;
  };
  
  // Funzione generale che seleziona il generatore corretto in base al tipo di costrutto
  export const generateCodeByType = (type, formData) => {
    const generators = {
      'if-else': generateIfElse,
      'case': generateCase,
      'loop-at': generateLoopAt,
      'do-enddo': generateDoEnddo,
      'while': generateWhile,
      'select': generateSelect,
      'update': generateUpdate,
      'insert': generateInsert,
      'modify': generateModify,
      'delete': generateDelete,
      'form': generateForm,
      'structure': generateStructure,
      'field-symbol': generateFieldSymbol,
      'internal-table': generateInternalTable,
      'data-declaration': generateDataDeclaration,
      'try-catch': generateTryCatch,
      'raise': generateRaiseException,
      'message': generateMessage,
      'class': generateClass,
      'interface': generateInterface,
      'method-chain': generateMethodChain,
      'method-definition': generateMethodDefinition,
      'alv-grid': generateAlvGrid,
      'bapi-call': generateBapiCall
    };
    
    if (generators[type]) {
      return generators[type](formData);
    }
    
    // Usa il generatore generico per i tipi non ancora implementati
    return generateGeneric(formData);
  };