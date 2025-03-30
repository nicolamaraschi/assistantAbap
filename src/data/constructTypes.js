// Definizione dei tipi di costrutti disponibili
const constructTypes = [
  {
    group: "Controllo Flusso",
    items: [
      { id: "if-else", name: "IF-ELSE" },
      { id: "case", name: "CASE" },
      { id: "loop-at", name: "LOOP AT" },
      { id: "do-enddo", name: "DO-ENDDO" },
      { id: "while", name: "WHILE" }
    ]
  },
  {
    group: "Manipolazione Dati",
    items: [
      { id: "select", name: "SELECT" },
      { id: "update", name: "UPDATE" },
      { id: "insert", name: "INSERT" },
      { id: "modify", name: "MODIFY" },
      { id: "delete", name: "DELETE" }
    ]
  },
  {
    group: "Strutture e Definizioni",
    items: [
      { id: "form", name: "FORM" },
      { id: "structure", name: "STRUCTURE" },
      { id: "field-symbol", name: "FIELD-SYMBOL" },
      { id: "internal-table", name: "INTERNAL TABLE" },
      { id: "data-declaration", name: "DATA Declaration" },
      { id: "selection-screen", name: "Selection Screen" }
    ]
  },
  {
    group: "UI5 & Fiori",
    items: [
      { id: "flower", name: "SAP Fiori App" },
      { id: "alv-grid", name: "ALV Grid" },
      { id: "advanced-alv", name: "ALV Grid Avanzato" },
      { id: "bapi-call", name: "BAPI Call" }
    ]
  },
  {
    group: "Gestione Errori",
    items: [
      { id: "try-catch", name: "TRY-CATCH" },
      { id: "raise", name: "RAISE EXCEPTION" },
      { id: "message", name: "MESSAGE" }
    ]
  },
  {
    group: "OOP",
    items: [
      { id: "class", name: "CLASS" },
      { id: "interface", name: "INTERFACE" },
      { id: "method-chain", name: "Method Chain" },
      { id: "method-definition", name: "Method Definition" }
    ]
  },
  {
    group: "Altri Costrutti",
    items: [
      { id: "bdc-session", name: "BDC Session" },
      { id: "smartform", name: "Smartform" }
    ]
  }
];

// Funzione per trovare il nome visualizzato di un costruttore dato il suo ID
export const getConstructNameById = (id) => {
  for (const group of constructTypes) {
    const item = group.items.find(item => item.id === id);
    if (item) return item.name;
  }
  return id;
};

export default constructTypes;