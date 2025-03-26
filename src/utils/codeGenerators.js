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

// Esempio di implementazione per altri costrutti ABAP
// Puoi implementare tutte le funzioni di generazione del codice seguendo lo stesso modello

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
    // Aggiungi qui gli altri generatori
  };
  
  if (generators[type]) {
    return generators[type](formData);
  }
  
  // Usa il generatore generico per i tipi non ancora implementati
  return generateGeneric(formData);
};