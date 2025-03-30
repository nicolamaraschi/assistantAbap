// Importa tutti i moduli necessari all'inizio del file
import { generateAdvancedAlv } from './advancedAlvGenerator';
import selectionScreenUtils from './selectionScreenGenerator';

// Altri import esistenti...


// Collezione di funzioni per generare codice ABAP
// Ogni funzione accetta un oggetto con i dati del form e restituisce una stringa di codice
// Funzione per generare il report fiori
export const generateFioriApp = (formData) => {
  const {
    appType = "transactional",
    appTitle = "My Fiori App",
    appDescription = "SAP Fiori Application",
    appId = "z.myfioriapp",
    oDataService = "ZMY_ODATA_SERVICE",
    entitySet = "MyEntitySet",
    navigationProperty = "",
    includeAnnotations = true,
    includeAnalytics = false,
    includeCustomActions = false,
    customActions = [],
    includeDraftHandling = false,
    includeFlexibleColumnLayout = false,
    useSmartControls = true,
    addAuthentication = true,
    i18nSupport = true,
    supportedLanguages = ["EN", "DE"],
  } = formData;

  // Creazione base del manifest.json come oggetto JS
  const manifest = {
    _version: "1.32.0",
    "sap.app": {
      id: appId,
      type: "application",
      i18n: "i18n/i18n.properties",
      title: "{{appTitle}}",
      description: "{{appDescription}}",
      applicationVersion: {
        version: "1.0.0",
      },
      dataSources: {
        mainService: {
          uri: `/sap/opu/odata/sap/${oDataService}/`,
          type: "OData",
          settings: {
            odataVersion: "2.0",
            ...(includeAnnotations && { annotations: [`${appId}.annotations`] }),
          },
        },
        ...(includeAnnotations && {
          [`${appId}.annotations`]: {
            uri: "annotations/annotations.xml",
            type: "ODataAnnotation",
            settings: {
              localUri: "annotations/annotations.xml",
            },
          },
        }),
      },
    },
    "sap.ui": {
      technology: "UI5",
      icons: {
        icon: "sap-icon://task",
        favIcon: "",
        phone: "",
        "phone@2": "",
        tablet: "",
        "tablet@2": "",
      },
      deviceTypes: {
        desktop: true,
        tablet: true,
        phone: true,
      },
      supportedThemes: ["sap_horizon", "sap_fiori_3", "sap_fiori_3_dark"],
    },
    "sap.ui5": {
      rootView: {
        viewName: `${appId}.view.App`,
        type: "XML",
        id: "app",
      },
      dependencies: {
        minUI5Version: "1.96.0",
        libs: {
          "sap.ui.core": {},
          "sap.m": {},
          ...(useSmartControls && {
            "sap.ui.comp": {},
            "sap.ui.generic.app": {},
          }),
          ...(includeFlexibleColumnLayout && { "sap.f": {} }),
          "sap.suite.ui.generic.template": {},
          "sap.ui.layout": {},
        },
      },
      contentDensities: {
        compact: true,
        cozy: true,
      },
      models: {
        i18n: {
          type: "sap.ui.model.resource.ResourceModel",
          settings: {
            bundleName: `${appId}.i18n.i18n`,
          },
        },
        "": {
          dataSource: "mainService",
          preload: true,
          settings: {},
        },
      },
      routing: {
        config: {
          routerClass: includeFlexibleColumnLayout
            ? "sap.f.routing.Router"
            : "sap.m.routing.Router",
          viewType: "XML",
          viewPath: `${appId}.view`,
          controlId: includeFlexibleColumnLayout ? "flexibleColumnLayout" : "app",
          controlAggregation: includeFlexibleColumnLayout
            ? "beginColumnPages"
            : "pages",
          bypassed: {
            target: "notFound",
          },
          async: true,
        },
        routes: [
          {
            pattern: "",
            name: "main",
            target: "main",
          },
          ...(navigationProperty
            ? [
                {
                  pattern: "Detail/{objectId}",
                  name: "detail",
                  target: "detail",
                },
              ]
            : []),
        ],
        targets: {
          main: {
            viewName: "Main",
            viewId: "main",
            viewLevel: 1,
            title: "{i18n>mainViewTitle}",
          },
          ...(navigationProperty && {
            detail: {
              viewName: "Detail",
              viewId: "detail",
              viewLevel: 2,
              title: "{i18n>detailViewTitle}",
              ...(includeFlexibleColumnLayout && {
                controlAggregation: "midColumnPages",
              }),
            },
          }),
          notFound: {
            viewName: "NotFound",
            viewId: "notFound",
          },
        },
      },
    },
  };

  // Restituisce il JSON formattato come stringa
  return JSON.stringify(manifest, null, 2);
};


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
  // IF-ELSE migliorato con supporto multi-ELSEIF
export const generateIfElse = (formData) => {
  const { 
    condition, 
    trueAction, 
    falseAction, 
    addElseIf, 
    elseIfConditions = [], // Supporto per multipli blocchi ELSEIF
    elseIfCondition,       // Per retrocompatibilità
    elseIfAction           // Per retrocompatibilità
  } = formData;
  
  let code = `IF ${condition}.
  ${trueAction}`;
  
  // Gestisce sia il vecchio formato che il nuovo formato multi-ELSEIF
  if (addElseIf) {
    if (elseIfConditions.length > 0) {
      // Nuovo formato: array di condizioni multiple
      elseIfConditions.forEach(elseIf => {
        code += `
ELSEIF ${elseIf.condition}.
  ${elseIf.action}`;
      });
    } else if (elseIfCondition) {
      // Vecchio formato: singolo ELSEIF
      code += `
ELSEIF ${elseIfCondition}.
  ${elseIfAction}`;
    }
  }
  
  code += `
ELSE.
  ${falseAction}
ENDIF.`;
  
  return code;
};
  
  // CASE migliorato
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
  
// LOOP AT migliorato con tutte le nuove funzionalità
export const generateLoopAt = (formData) => {
  const { 
    table, 
    variable, 
    whereCondition, 
    content, 
    useAssigning, 
    addIndex,
    useGroupBy,
    groupByField,
    groupByOrder,
    useReferenceInto,
    indexFrom,
    indexTo,
    useTableMode,        // Nuovo: LOOP AT IN TABLE MODE
    useRange,            // Nuovo: LOOP AT con RANGE
    rangeVariable,
    useFilter,           // Nuovo: LOOP AT con FILTER CONDITIONS
    filterCondition,
    useTransporting,     // Nuovo: LOOP AT con TRANSPORTING
    transportingFields,
    useNested,           // Nuovo: supporto per tabelle annidate
    nestedTable,
    nestedVariable,
    nestedContent,
    useUpTo,             // Nuovo: UP TO n ROWS
    upToRows
  } = formData;
  
  let code = `LOOP AT ${table}`;
  
  // Aggiungi IN TABLE MODE se selezionato
  if (useTableMode) {
    code += ` IN TABLE MODE`;
  }
  
  // Aggiungi FILTER se selezionato
  if (useFilter && filterCondition) {
    code += ` FILTER ( ${filterCondition} )`;
  }
  
  // Aggiungi RANGE se selezionato
  if (useRange && rangeVariable) {
    code += ` IN ${rangeVariable}`;
  }
  
  // Aggiungi WHERE se presente e non si usano FILTER o RANGE
  if (whereCondition && !useFilter && !useRange) {
    code += ` WHERE ${whereCondition}`;
  }
  
  // Aggiungi GROUP BY se richiesto
  if (useGroupBy && groupByField) {
    code += `\n  GROUP BY ${groupByField}`;
    if (groupByOrder) {
      code += `\n  ${groupByOrder}`;
    }
  }
  
  // Scegli tra INTO, REFERENCE INTO o ASSIGNING
  if (useAssigning) {
    code += ` ASSIGNING FIELD-SYMBOL(<${variable}>)`;
  } else if (useReferenceInto) {
    code += ` REFERENCE INTO DATA(${variable})`;
  } else {
    code += ` INTO ${variable}`;
  }
  
  // Aggiungi TRANSPORTING se richiesto
  if (useTransporting && transportingFields) {
    code += `\n  TRANSPORTING ${transportingFields}`;
  }
  
  // Aggiungi FROM/TO se richiesto
  if (addIndex) {
    code += ` FROM ${indexFrom} TO ${indexTo}`;
  }
  
  // Aggiungi UP TO n ROWS se richiesto
  if (useUpTo && upToRows) {
    code += `\n  UP TO ${upToRows} ROWS`;
  }
  
  code += `.`;
  
  // Aggiungi il contenuto del loop
  code += `\n  ${content}`;
  
  // Aggiungi un loop annidato se richiesto
  if (useNested && nestedTable && nestedVariable) {
    code += `\n\n  LOOP AT ${nestedTable} INTO ${nestedVariable}.`;
    code += `\n    ${nestedContent}`;
    code += `\n  ENDLOOP.`;
  }
  
  code += `\nENDLOOP.`;
  
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
  
  // SELECT migliorato con UNION ALL e FOR ALL ENTRIES
export const generateSelect = (formData) => {
  const { 
    fields, 
    table, 
    into, 
    where, 
    orderBy, 
    groupBy, 
    having, 
    addJoin, 
    joinType, 
    joinTable, 
    joinCondition,
    useUnion = false,           // Nuovo parametro
    unionType = 'UNION ALL',    // Nuovo parametro
    unionSelect = '',           // Nuovo parametro
    useForAllEntries = false,   // Nuovo parametro
    forAllEntriesTable = '',    // Nuovo parametro
    forAllEntriesWhere = ''     // Nuovo parametro
  } = formData;
  
  let code = `SELECT ${fields}`;
  
  if (addJoin) {
    code += `
  FROM ${table}
  ${joinType} ${joinTable} ON ${joinCondition}`;
  } else if (useForAllEntries && forAllEntriesTable) {
    code += `
  FROM ${table}
  FOR ALL ENTRIES IN ${forAllEntriesTable}
  WHERE ${forAllEntriesWhere || `${table}~id = ${forAllEntriesTable}-id`}`;
  } else {
    code += `
  FROM ${table}`;
  }
  
  if (where && !useForAllEntries) {
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
  
  // Aggiungi UNION o UNION ALL se richiesto
  if (useUnion && unionSelect) {
    code += `
${unionType}
${unionSelect}`;
  }
  
  code += `
  INTO ${into}.`;
  
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
  
  // INTERNAL TABLE migliorato con dati iniziali
export const generateInternalTable = (formData) => {
  const { 
    name, 
    type, 
    tableType, 
    keyType, 
    initialSize,
    withHeader = false,
    includeInitialData = false,  // Nuovo parametro
    initialData = ''             // Nuovo parametro
  } = formData;
  
  let code = `DATA: ${name} TYPE`;
  
  if (tableType) {
    code += ` ${tableType} TABLE OF ${type}`;
  } else {
    code += ` STANDARD TABLE OF ${type}`;
  }
  
  if (keyType) {
    code += ` WITH ${keyType} KEY`;
  }
  
  if (initialSize) {
    code += ` INITIAL SIZE ${initialSize}`;
  }
  
  if (withHeader) {
    code += ` WITH HEADER LINE`;
  }
  
  code += `.`;
  
  // Aggiungi dati iniziali se richiesto
  if (includeInitialData && initialData) {
    code += `

${name} = VALUE #( 
  ${initialData}
).`;
  }
  
  return code;
};

  
  // Funzione migliorata per generare dichiarazioni DATA in ABAP
export const generateDataDeclaration = (formData) => {
  const { variables } = formData;
  
  if (!variables || variables.length === 0) {
    return '* Nessuna variabile definita';
  }
  
  // Raggruppa le variabili per tipo per una dichiarazione più compatta
  const groupedVars = {};
  
  variables.forEach(variable => {
    // Costruisci il tipo completo
    let fullType = variable.type;
    
    // Aggiungi lunghezza/decimali per i tipi che li supportano
    if ((variable.type === 'C' || variable.type === 'N' || variable.type === 'X') && variable.length) {
      fullType = `${variable.type}(${variable.length})`;
    } else if (variable.type === 'P' && variable.length) {
      const decimals = variable.decimals || '0';
      fullType = `${variable.type}(${variable.length}.${decimals})`;
    }
    
    // Aggiungi dettagli tabella se è una tabella
    if (variable.isTable) {
      fullType = `${variable.tableType} OF ${variable.tableOf} WITH ${variable.keyType}`;
    }
    
    // Aggiungi REF TO se è un riferimento
    if (variable.isReference && !fullType.startsWith('REF TO')) {
      fullType = `REF TO ${fullType}`;
    }
    
    // Chiave del gruppo (tipo + valore iniziale, se presente)
    const initialValuePart = variable.initialValue ? ` VALUE ${variable.initialValue}` : '';
    const groupKey = `${fullType}${initialValuePart}`;
    
    if (!groupedVars[groupKey]) {
      groupedVars[groupKey] = {
        type: fullType,
        initialValue: variable.initialValue,
        names: []
      };
    }
    
    groupedVars[groupKey].names.push(variable.name);
  });
  
  // Costruisci il codice ABAP
  let code = 'DATA:';
  
  // Formatta ogni gruppo
  const typeGroups = Object.values(groupedVars);
  typeGroups.forEach((group, index) => {
    const isLast = index === typeGroups.length - 1;
    
    code += `\n  ${group.names.join(', ')} TYPE ${group.type}`;
    
    if (group.initialValue) {
      code += ` VALUE ${group.initialValue}`;
    }
    
    code += isLast ? '.' : ',';
  });
  
  return code;
};

// Esempio di funzione per generare dichiarazioni TYPE
export const generateTypeDeclaration = (formData) => {
  const { types } = formData;
  
  if (!types || types.length === 0) {
    return '* Nessun tipo definito';
  }
  
  let code = 'TYPES:';
  
  types.forEach((type, index) => {
    const isLast = index === types.length - 1;
    
    code += `\n  ${type.name} TYPE ${type.type}`;
    
    code += isLast ? '.' : ',';
  });
  
  return code;
};

// Versione alternativa che può essere combinata con le dichiarazioni di tipo
export const generateDataDeclarationAlternative = (formData) => {
  const { variables, useTypes, types } = formData;
  
  if ((!variables || variables.length === 0) && (!useTypes || !types || types.length === 0)) {
    return '* Nessuna dichiarazione definita';
  }
  
  let code = '';
  
  // Aggiungi dichiarazioni di tipo se necessario
  if (useTypes && types && types.length > 0) {
    code += 'TYPES:';
    
    types.forEach((type, index) => {
      const isLast = index === types.length - 1 && (!variables || variables.length === 0);
      
      code += `\n  ${type.name} TYPE ${type.type}`;
      
      code += isLast ? '.' : ',';
    });
    
    // Aggiungi un separatore se ci sono anche variabili
    if (variables && variables.length > 0) {
      code += '\n\n';
    }
  }
  
  // Aggiungi dichiarazioni di variabili
  if (variables && variables.length > 0) {
    code += 'DATA:';
    
    // Raggruppa le variabili per tipo
    const groupedVars = {};
    
    variables.forEach(variable => {
      // Costruisci il tipo completo
      let fullType = variable.type;
      
      // Aggiungi lunghezza/decimali per i tipi che li supportano
      if ((variable.type === 'C' || variable.type === 'N' || variable.type === 'X') && variable.length) {
        fullType = `${variable.type}(${variable.length})`;
      } else if (variable.type === 'P' && variable.length) {
        const decimals = variable.decimals || '0';
        fullType = `${variable.type}(${variable.length}.${decimals})`;
      }
      
      // Aggiungi dettagli tabella se è una tabella
      if (variable.isTable) {
        if (fullType.includes('TABLE OF')) {
          // Il tipo è già una dichiarazione di tabella
          fullType = `${fullType} WITH ${variable.keyType}`;
        } else {
          // Costruisci la dichiarazione di tabella
          fullType = `${variable.tableType} OF ${fullType} WITH ${variable.keyType}`;
        }
      }
      
      // Aggiungi REF TO se è un riferimento
      if (variable.isReference && !fullType.startsWith('REF TO')) {
        fullType = `REF TO ${fullType}`;
      }
      
      // Chiave del gruppo (tipo + valore iniziale, se presente)
      const initialValuePart = variable.initialValue ? ` VALUE ${variable.initialValue}` : '';
      const groupKey = `${fullType}${initialValuePart}`;
      
      if (!groupedVars[groupKey]) {
        groupedVars[groupKey] = {
          type: fullType,
          initialValue: variable.initialValue,
          names: []
        };
      }
      
      groupedVars[groupKey].names.push(variable.name);
    });
    
    // Formatta ogni gruppo
    const typeGroups = Object.values(groupedVars);
    typeGroups.forEach((group, index) => {
      const isLast = index === typeGroups.length - 1;
      
      code += `\n  ${group.names.join(', ')} TYPE ${group.type}`;
      
      if (group.initialValue) {
        code += ` VALUE ${group.initialValue}`;
      }
      
      code += isLast ? '.' : ',';
    });
  }
  
  return code;
};
  
  // TRY-CATCH migliorato con supporto RESUME
export const generateTryCatch = (formData) => {
  const { 
    tryBlock, 
    catchBlocks, 
    cleanup,
    addCleanup = false,
    multipleExceptions = false,
    useResume = false  // Nuovo parametro
  } = formData;
  
  let code = `TRY.
  ${tryBlock}`;
  
  if (multipleExceptions && catchBlocks && catchBlocks.length > 0) {
    catchBlocks.forEach(catchBlock => {
      code += `
CATCH ${catchBlock.className} INTO DATA(lx_exc).
  ${catchBlock.content}`;
      if (useResume && catchBlock.useResume) {
        code += `
  RESUME.`;
      }
    });
  } else if (catchBlocks && catchBlocks.length > 0) {
    code += `
CATCH ${catchBlocks[0].className} INTO DATA(lx_exc).
  ${catchBlocks[0].content}`;
    if (useResume && catchBlocks[0].useResume) {
      code += `
  RESUME.`;
    }
  }
  
  if (addCleanup && cleanup) {
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
  
  // CLASS migliorata con supporto per EVENTS e metodi GET/SET automatici
export const generateClass = (formData) => {
  const { 
    name, 
    definition, 
    superclass, 
    interfaces,
    attributes = [],
    methods = [],
    events = [],                // Nuovo parametro
    generateGetSet = false,     // Nuovo parametro
    isSingleton = false         // Nuovo parametro
  } = formData;
  
  let code = `CLASS ${name} ${definition}`;
  
  if (isSingleton) {
    code += ` CREATE PRIVATE`;
  }
  
  if (superclass) {
    code += ` INHERITING FROM ${superclass}`;
  }
  
  if (interfaces) {
    code += ` INTERFACES ${interfaces}`;
  }
  
  code += `.\n`;
  
  // Sezione pubblica
  code += `  PUBLIC SECTION.\n`;
  
  // Eventi
  if (events && events.length > 0) {
    code += `    EVENTS:\n`;
    const eventDeclarations = events.map(event => `      ${event.name} EXPORTING VALUE(${event.paramName}) TYPE ${event.paramType}`);
    code += eventDeclarations.join(',\n');
    code += `.\n\n`;
  }
  
  // Singleton pattern
  if (isSingleton) {
    code += `    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO ${name}.\n\n`;
  }
  
  // Metodi pubblici
  if (methods && methods.filter(m => m.visibility === 'PUBLIC').length > 0) {
    code += `    METHODS:\n`;
    const publicMethods = methods.filter(m => m.visibility === 'PUBLIC');
    
    const methodDeclarations = publicMethods.map(method => {
      let methodDecl = `      ${method.name}`;
      
      if (method.importing) {
        methodDecl += `\n        IMPORTING ${method.importing}`;
      }
      
      if (method.exporting) {
        methodDecl += `\n        EXPORTING ${method.exporting}`;
      }
      
      if (method.returning) {
        methodDecl += `\n        RETURNING VALUE(${method.returning.name}) TYPE ${method.returning.type}`;
      }
      
      return methodDecl;
    });
    
    code += methodDeclarations.join(',\n\n');
    code += `.\n\n`;
  }
  
  // Attributi pubblici
  if (attributes && attributes.filter(a => a.visibility === 'PUBLIC').length > 0) {
    code += `    DATA:\n`;
    const publicAttrs = attributes.filter(a => a.visibility === 'PUBLIC');
    const attrDeclarations = publicAttrs.map(attr => `      ${attr.name} TYPE ${attr.type}`);
    code += attrDeclarations.join(',\n');
    code += `.\n\n`;
    
    // Genera metodi getter/setter automatici se richiesto
    if (generateGetSet && publicAttrs.length > 0) {
      code += `    METHODS:\n`;
      const getSetMethods = [];
      
      publicAttrs.forEach(attr => {
        const attrNameWithoutPrefix = attr.name.replace(/^m[tvs]_/, '');
        const setterName = `set_${attrNameWithoutPrefix}`;
        const getterName = `get_${attrNameWithoutPrefix}`;
        
        getSetMethods.push(`      ${setterName} IMPORTING iv_${attrNameWithoutPrefix} TYPE ${attr.type}`);
        getSetMethods.push(`      ${getterName} RETURNING VALUE(rv_${attrNameWithoutPrefix}) TYPE ${attr.type}`);
      });
      
      code += getSetMethods.join(',\n');
      code += `.\n\n`;
    }
  }
  
  // Sezione protetta e attributi protected
  if (attributes && attributes.filter(a => a.visibility === 'PROTECTED').length > 0) {
    code += `  PROTECTED SECTION.\n    DATA:\n`;
    const protectedAttrs = attributes.filter(a => a.visibility === 'PROTECTED');
    const attrDeclarations = protectedAttrs.map(attr => `      ${attr.name} TYPE ${attr.type}`);
    code += attrDeclarations.join(',\n');
    code += `.\n\n`;
  }
  
  // Sezione privata e attributi private
  code += `  PRIVATE SECTION.\n`;
  
  // Singleton instance
  if (isSingleton) {
    code += `    CLASS-DATA:
      go_instance TYPE REF TO ${name}.\n\n`;
  }
  
  if (attributes && attributes.filter(a => a.visibility === 'PRIVATE').length > 0) {
    code += `    DATA:\n`;
    const privateAttrs = attributes.filter(a => a.visibility === 'PRIVATE');
    const attrDeclarations = privateAttrs.map(attr => `      ${attr.name} TYPE ${attr.type}`);
    code += attrDeclarations.join(',\n');
    code += `.\n\n`;
  }
  
  code += `ENDCLASS.`;
  
  // Implementazione GET_INSTANCE per singleton
  if (isSingleton) {
    code += `

CLASS ${name} IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.
ENDCLASS.`;
  }
  
  return code;
};
  
  // Funzione per generare codice INTERFACE migliorato
export const generateInterface = (formData) => {
  const { name, constants = [], methods = [] } = formData;
  
  let code = `INTERFACE ${name}.
`;
  
  // Aggiunta delle costanti
  if (constants && constants.length > 0) {
    code += `
  CONSTANTS:
`;
    
    const constantDeclarations = constants.map((constant, index) => {
      return `    ${constant.name} TYPE ${constant.type} VALUE ${constant.value}` + 
             (index < constants.length - 1 ? ',' : '.');
    });
    
    code += constantDeclarations.join('\n');
    code += '\n';
  }
  
  // Aggiunta dei metodi
  if (methods && methods.length > 0) {
    code += `
  METHODS:
`;
    
    const methodDeclarations = methods.map((method, index) => {
      let methodDecl = `    ${method.name}`;
      
      if (method.importing) {
        methodDecl += `\n      IMPORTING ${method.importing}`;
      }
      
      if (method.exporting) {
        methodDecl += `\n      EXPORTING ${method.exporting}`;
      }
      
      if (method.raising) {
        methodDecl += `\n      RAISING ${method.raising}`;
      }
      
      return methodDecl + (index < methods.length - 1 ? ',' : '.');
    });
    
    code += methodDeclarations.join('\n\n');
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
      'bapi-call': generateBapiCall,
      'advanced-alv': generateAdvancedAlv,
      'selection-screen': selectionScreenUtils.generateSelectionScreen,
      'flower': generateFioriApp
    };
    
    if (generators[type]) {
      return generators[type](formData);
    }
    
    // Usa il generatore generico per i tipi non ancora implementati
    return generateGeneric(formData);
  };