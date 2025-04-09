/**
 * Generatore di codice per applicazioni SAP Fiori
 * Questo modulo contiene funzioni per generare i diversi file necessari
 * per un'applicazione SAP Fiori (manifest.json, Component.js, controller, view, i18n)
 */

/**
 * Genera il manifest.json completo per un'applicazione Fiori
 * @param {Object} formData - Dati del form con la configurazione
 * @returns {String} Codice JSON formattato del manifest
 */
export const generateManifest = (formData) => {
    const {
      appType = "transactional",
      appTitle = "My Fiori App",
      appDescription = "SAP Fiori Application",
      appId = "z.myfioriapp",
      namespace = "com.mycompany",
      oDataService = "ZMY_ODATA_SERVICE",
      oDataVersion = "v2",
      entitySet = "MyEntitySet",
      navigationProperty = "",
      includeAnnotations = true,
      annotationsSource = "LOCAL_ANNOTATIONS",
      includeAnalytics = false,
      includeCustomActions = false,
      customActions = [],
      includeDraftHandling = false,
      includeFlexibleColumnLayout = false,
      useSmartControls = true,
      smartFields = [],
      addAuthentication = true,
      authenticationMethod = "SAML2",
      i18nSupport = true,
      supportedLanguages = ["EN", "DE"],
      includePersonalization = true,
      includeOfflineCapabilities = false,
    } = formData;
  
    // Costruisci il namespace completo
    const fullAppId = `${namespace}.${appId}`;
  
    // Creazione base del manifest.json come oggetto JS
    const manifest = {
      _version: "1.40.0",
      "sap.app": {
        id: fullAppId,
        type: "application",
        i18n: i18nSupport ? "i18n/i18n.properties" : "",
        applicationVersion: {
          version: "1.0.0",
        },
        title: "{{appTitle}}",
        description: "{{appDescription}}",
        tags: {
          keywords: ["SAP Fiori"]
        },
        resources: "resources.json",
        ach: "PA-FIO",
        dataSources: {
          mainService: {
            uri: `/sap/opu/odata/sap/${oDataService}/`,
            type: "OData",
            settings: {
              odataVersion: oDataVersion === "v4" ? "4.0" : "2.0",
              localUri: "localService/metadata.xml",
              ...(includeAnnotations && { annotations: [`${annotationsSource}`] }),
            },
          },
          ...(includeAnnotations && {
            [`${annotationsSource}`]: {
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
        supportedThemes: ["sap_horizon", "sap_horizon_dark", "sap_fiori_3", "sap_fiori_3_dark"],
        fullWidth: appType === "analytical" || includeAnalytics,
      },
      "sap.ui5": {
        flexEnabled: includePersonalization,
        dependencies: {
          minUI5Version: "1.108.0",
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
            ...(includeAnalytics && { "sap.viz": {} }),
          },
        },
        contentDensities: {
          compact: true,
          cozy: true,
        },
        models: {
          ...(i18nSupport && {
            i18n: {
              type: "sap.ui.model.resource.ResourceModel",
              settings: {
                bundleName: `${fullAppId}.i18n.i18n`,
                supportedLocales: supportedLanguages,
                fallbackLocale: "en"
              },
            },
          }),
          "": {
            dataSource: "mainService",
            preload: true,
            settings: {
              defaultBindingMode: "TwoWay",
              defaultCountMode: "Inline",
              refreshAfterChange: true,
              useBatch: true,
              ...(includeDraftHandling && { 
                defaultOperationMode: "Server",
                autoExpandSelect: true 
              }),
            },
          },
        },
        routing: {
          config: {
            routerClass: includeFlexibleColumnLayout
              ? "sap.f.routing.Router"
              : "sap.m.routing.Router",
            viewType: "XML",
            viewPath: `${fullAppId}.view`,
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
        resources: {
          css: [
            {
              uri: "css/style.css"
            }
          ]
        },
        rootView: {
          viewName: `${fullAppId}.view.App`,
          type: "XML",
          id: "app",
        },
      },
      "sap.fiori": {
        registrationIds: [],
        archeType: appType === "transactional" ? "transactional" : 
                   appType === "analytical" ? "analytical" : 
                   appType === "factsheet" ? "factsheet" : "standard"
      }
    };
  
  /**
   * Genera il file DetailController.js per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice del controller di dettaglio
   */
  export const generateDetailController = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      entitySet = "MyEntitySet",
      navigationProperty = "ID",
      includeFlexibleColumnLayout = false
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
  
    return `sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap/m/MessageToast"${includeFlexibleColumnLayout ? ',\n    "sap/f/LayoutType"' : ''}
  ], function(Controller, JSONModel, MessageBox, MessageToast${includeFlexibleColumnLayout ? ', LayoutType' : ''}) {
    "use strict";
  
    return Controller.extend("${fullAppId}.controller.Detail", {
      /**
       * Called when the controller is instantiated
       */
      onInit: function() {
        // Set up local view model
        var oViewModel = new JSONModel({
          busy: false,
          delay: 0,
          editable: false,
          objectId: ""
        });
        this.getView().setModel(oViewModel, "viewModel");
        
        // Initialize the router
        this.getRouter().getRoute("detail").attachPatternMatched(this._onRouteMatched, this);
      },
      
      /**
       * Convenience method for accessing the router
       * @returns {sap.ui.core.routing.Router} The router for this component
       */
      getRouter: function() {
        return this.getOwnerComponent().getRouter();
      },
      
      /**
       * Convenience method for getting the view model by name
       * @param {string} sName the model name
       * @returns {sap.ui.model.Model} the model instance
       */
      getModel: function(sName) {
        return this.getView().getModel(sName);
      },
      
      /**
       * Convenience method for setting the view model
       * @param {sap.ui.model.Model} oModel the model instance
       * @param {string} sName the model name
       */
      setModel: function(oModel, sName) {
        this.getView().setModel(oModel, sName);
      },
      
      /**
       * Event handler for navigating back
       */
      onNavBack: function() {
        ${includeFlexibleColumnLayout ? `
        var oModel = this.getOwnerComponent().getModel();
        oModel.setProperty("/layout", LayoutType.OneColumn);
        this.getRouter().navTo("main");` : `
        this.getRouter().navTo("main");`}
      },
      
      /**
       * Event handler for route matched event
       * @param {sap.ui.base.Event} oEvent The event object
       * @private
       */
      _onRouteMatched: function(oEvent) {
        var oViewModel = this.getModel("viewModel");
        var sObjectId = oEvent.getParameter("arguments").objectId;
        
        oViewModel.setProperty("/busy", true);
        oViewModel.setProperty("/objectId", sObjectId);
        
        // Load the object data
        this._loadObject(sObjectId);
      },
      
      /**
       * Loads the object data
       * @param {string} sObjectId The object ID
       * @private
       */
      _loadObject: function(sObjectId) {
        var oViewModel = this.getModel("viewModel");
        var oModel = this.getModel();
        
        var sPath = "/" + "${entitySet}('" + sObjectId + "')";
        
        oModel.read(sPath, {
          success: function(oData) {
            oViewModel.setProperty("/busy", false);
          },
          error: function() {
            oViewModel.setProperty("/busy", false);
            MessageBox.error(this.getModel("i18n").getResourceBundle().getText("errorLoadingObject"));
          }.bind(this)
        });
      },
      
      /**
       * Event handler for edit button
       */
      onEdit: function() {
        this.getModel("viewModel").setProperty("/editable", true);
      },
      
      /**
       * Event handler for save button
       */
      onSave: function() {
        var oViewModel = this.getModel("viewModel");
        oViewModel.setProperty("/busy", true);
        
        // Simulate save operation
        setTimeout(function() {
          oViewModel.setProperty("/busy", false);
          oViewModel.setProperty("/editable", false);
          MessageToast.show(this.getModel("i18n").getResourceBundle().getText("saveSuccessMessage"));
        }.bind(this), 1000);
      },
      
      /**
       * Event handler for cancel button
       */
      onCancel: function() {
        // Reset any changes
        this._loadObject(this.getModel("viewModel").getProperty("/objectId"));
        this.getModel("viewModel").setProperty("/editable", false);
      },
      
      /**
       * Event handler for delete button
       */
      onDelete: function() {
        var sObjectId = this.getModel("viewModel").getProperty("/objectId");
        var oResourceBundle = this.getModel("i18n").getResourceBundle();
        
        MessageBox.confirm(
          oResourceBundle.getText("confirmDeleteMessage"),
          {
            title: oResourceBundle.getText("confirmDeleteTitle"),
            onClose: function(sAction) {
              if (sAction === MessageBox.Action.OK) {
                // Perform deletion
                this.getModel("viewModel").setProperty("/busy", true);
                
                // Simulate delete operation
                setTimeout(function() {
                  this.getModel("viewModel").setProperty("/busy", false);
                  this.onNavBack();
                  MessageToast.show(oResourceBundle.getText("deleteSuccessMessage"));
                }.bind(this), 1000);
              }
            }.bind(this)
          }
        );
      }
    });
  });`;
  };
  
    // Restituisce il JSON formattato come stringa
    return JSON.stringify(manifest, null, 2);
  };
  
  /**
   * Genera il file Component.js per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice Component.js
   */
  export const generateComponent = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      useSmartControls = true,
      includeFlexibleColumnLayout = false,
      includeOfflineCapabilities = false,
      includePersonalization = true
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
  
    return `sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "${fullAppId}/model/models"${includeFlexibleColumnLayout ? ',\n  "sap/f/FlexibleColumnLayoutSemanticHelper"' : ''}
  ], function(UIComponent, Device, models${includeFlexibleColumnLayout ? ', FlexibleColumnLayoutSemanticHelper' : ''}) {
    "use strict";
  
    return UIComponent.extend("${fullAppId}.Component", {
      metadata: {
        manifest: "json"
      },
  
      /**
       * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
       * @public
       * @override
       */
      init: function() {
        // Call the base component's init function
        UIComponent.prototype.init.apply(this, arguments);
  
        // Enable routing
        this.getRouter().initialize();
  
        // Set the device model
        this.setModel(models.createDeviceModel(), "device");
        
        ${useSmartControls ? '// Set the FLP model\n      this.setModel(models.createFLPModel(), "FLP");' : ''}
        ${includePersonalization ? '// Set personalization model\n      this.setModel(models.createPersonalizationModel(), "personalization");' : ''}
      }${includeFlexibleColumnLayout ? ',\n    \n    /**\n     * Returns an instance of the semantic helper\n     * @returns {sap.f.FlexibleColumnLayoutSemanticHelper} An instance of the semantic helper\n     */\n    getHelper: function() {\n      var oFCL = this.getRootControl().byId("flexibleColumnLayout");\n      var oParams = jQuery.sap.getUriParameters();\n      var oSettings = {\n        defaultTwoColumnLayoutType: sap.f.LayoutType.TwoColumnsMidExpanded,\n        defaultThreeColumnLayoutType: sap.f.LayoutType.ThreeColumnsMidExpanded,\n        mode: oParams.get("mode"),\n        initialColumnsCount: oParams.get("initial"),\n        maxColumnsCount: oParams.get("max")\n      };\n      \n      return FlexibleColumnLayoutSemanticHelper.getInstanceFor(oFCL, oSettings);\n    }' : ''}
    });
  });`;
  };
  
  /**
   * Genera il file models.js per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice models.js
   */
  export const generateModels = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      includePersonalization = true,
      useSmartControls = true
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
  
    return `sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap/ui/Device"
  ], function(JSONModel, Device) {
    "use strict";
  
    return {
      /**
       * Creates a device model that provides information about the current device
       * @returns {sap.ui.model.json.JSONModel} The device model
       */
      createDeviceModel: function() {
        var oModel = new JSONModel(Device);
        oModel.setDefaultBindingMode("OneWay");
        return oModel;
      },
  
      ${useSmartControls ? `/**
       * Creates a FLP model for Smart controls
       * @returns {sap.ui.model.json.JSONModel} The FLP model
       */
      createFLPModel: function() {
        var fnGetUser = jQuery.sap.getObject("sap.ushell.Container.getUser");
        var bIsShareInJamActive = fnGetUser ? fnGetUser().isJamActive() : false;
        var oModel = new JSONModel({
          isShareInJamActive: bIsShareInJamActive
        });
        oModel.setDefaultBindingMode("OneWay");
        return oModel;
      },` : ''}
  
      ${includePersonalization ? `/**
       * Creates a model for user personalization settings
       * @returns {sap.ui.model.json.JSONModel} The personalization model
       */
      createPersonalizationModel: function() {
        var oModel = new JSONModel({
          // Default personalization settings
          settings: {
            // User display preferences
            display: {
              density: "cozy",
              theme: "sap_horizon"
            },
            // Table personalization
            table: {
              visibleColumns: [],
              sortSettings: [],
              filterSettings: []
            }
          }
        });
        return oModel;
      }` : ''}
    };
  });`;
  };
  
  /**
   * Genera il file MainController.js per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice del controller
   */
  export const generateMainController = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      entitySet = "MyEntitySet",
      navigationProperty = "",
      useSmartControls = true,
      includeCustomActions = false,
      customActions = [],
      includeFlexibleColumnLayout = false
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
    let customActionsCode = '';
    
    if (includeCustomActions && customActions.length > 0) {
      customActionsCode = customActions.map(action => {
        return `
      /**
       * Handler for the "${action.label}" action
       * @param {sap.ui.base.Event} oEvent The event object
       */
      on${action.name.charAt(0).toUpperCase() + action.name.slice(1)}: function(oEvent) {
        // Implementation for ${action.label} action
        sap.m.MessageToast.show("${action.label} action triggered");
      },`;
      }).join('\n');
    }
  
    return `sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"${useSmartControls ? ',\n  "sap/ui/comp/smartfilterbar/SmartFilterBar",\n  "sap/ui/comp/smarttable/SmartTable"' : ''}${includeFlexibleColumnLayout ? ',\n  "sap/f/LayoutType"' : ''}
  ], function(Controller, JSONModel, MessageToast${useSmartControls ? ', SmartFilterBar, SmartTable' : ''}${includeFlexibleColumnLayout ? ', LayoutType' : ''}) {
    "use strict";
  
    return Controller.extend("${fullAppId}.controller.Main", {
      /**
       * Called when the controller is instantiated
       */
      onInit: function() {
        // Set up local view model
        var oViewModel = new JSONModel({
          busy: false,
          delay: 0,
          editable: false,
          entitySet: "${entitySet}"
        });
        this.getView().setModel(oViewModel, "viewModel");
        
        // Initialize the router
        this.getRouter().getRoute("main").attachPatternMatched(this._onRouteMatched, this);
      },
      
      /**
       * Convenience method for accessing the router
       * @returns {sap.ui.core.routing.Router} The router for this component
       */
      getRouter: function() {
        return this.getOwnerComponent().getRouter();
      },
      
      /**
       * Convenience method for getting the view model by name
       * @param {string} sName the model name
       * @returns {sap.ui.model.Model} the model instance
       */
      getModel: function(sName) {
        return this.getView().getModel(sName);
      },
      
      /**
       * Convenience method for setting the view model
       * @param {sap.ui.model.Model} oModel the model instance
       * @param {string} sName the model name
       */
      setModel: function(oModel, sName) {
        this.getView().setModel(oModel, sName);
      },
      
      /**
       * Event handler for route matched event
       * @param {sap.ui.base.Event} oEvent The event object
       * @private
       */
      _onRouteMatched: function(oEvent) {
        this._refreshData();
      },
      
      /**
       * Refresh the data in the view
       * @private
       */
      _refreshData: function() {
        var oViewModel = this.getModel("viewModel");
        oViewModel.setProperty("/busy", true);
        
        // Here you can perform any data loading logic
        
        // Simulate data loading
        setTimeout(function() {
          oViewModel.setProperty("/busy", false);
        }, 1000);
      },${navigationProperty ? `
      
      /**
       * Event handler for navigating to the detail view
       * @param {sap.ui.base.Event} oEvent The event object
       */
      onItemPress: function(oEvent) {
        var oItem = oEvent.getSource();
        var oBindingContext = oItem.getBindingContext();
        var sObjectId = oBindingContext.getProperty("${navigationProperty}");
        
        this.getRouter().navTo("detail", {
          objectId: sObjectId
        }${includeFlexibleColumnLayout ? ',\n        {\n          "layout": LayoutType.TwoColumnsMidExpanded\n        }' : ''});
      },` : ''}
      ${customActionsCode}
      
      /**
       * Event handler for searching in the table
       * @param {sap.ui.base.Event} oEvent The event object
       */
      onSearch: function(oEvent) {
        var sQuery = oEvent.getParameter("query");
        var oTable = this.byId("table");
        var oBinding = oTable.getBinding("items");
        
        if (oBinding) {
          var aFilters = [];
          if (sQuery && sQuery.length > 0) {
            // Create filters for the search
            // Add your search logic here based on your entity properties
          }
          
          oBinding.filter(aFilters);
        }
      }
    });
  });`;
  };
  
  /**
   * Genera il file MainView.xml per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice della vista XML
   */
  export const generateMainView = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      appTitle = "My Fiori App",
      entitySet = "MyEntitySet",
      navigationProperty = "",
      useSmartControls = true,
      includeCustomActions = false,
      customActions = [],
      includeFlexibleColumnLayout = false,
      smartFields = [],
      includeAnalytics = false
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
    
    let customActionButtons = '';
    if (includeCustomActions && customActions.length > 0) {
      customActionButtons = customActions.map(action => {
        return `					<Button
                          id="${action.name}Button"
                          text="{i18n>${action.name}ButtonText}"
                          icon="${action.icon}"
                          press=".on${action.name.charAt(0).toUpperCase() + action.name.slice(1)}"
                          tooltip="{i18n>${action.name}ButtonTooltip}" />`;
      }).join('\n');
    }
  
    // Genera colonne per la tabella standard (se non si usano smart controls)
    let standardTableColumns = '';
    if (!useSmartControls && smartFields.length > 0) {
      standardTableColumns = smartFields.map(field => {
        return `					<Column id="${field.toLowerCase()}Column">
                          <Text text="{i18n>${field.toLowerCase()}ColumnTitle}" />
                      </Column>`;
      }).join('\n');
    }
  
    // Celle della tabella standard
    let standardTableCells = '';
    if (!useSmartControls && smartFields.length > 0) {
      standardTableCells = smartFields.map(field => {
        return `						<Text text="{${field}}" />`;
      }).join('\n');
    }
  
    return `<mvc:View
      controllerName="${fullAppId}.controller.Main"
      displayBlock="true"
      xmlns="sap.m"
      xmlns:mvc="sap.ui.core.mvc"${useSmartControls ? '\n\txmlns:smartFilterBar="sap.ui.comp.smartfilterbar"\n\txmlns:smartTable="sap.ui.comp.smarttable"' : ''}${includeFlexibleColumnLayout ? '\n\txmlns:f="sap.f"' : ''}${includeAnalytics ? '\n\txmlns:viz="sap.viz.ui5.controls"\n\txmlns:vizData="sap.viz.ui5.data"' : ''}>
      <Page
          id="mainPage"
          title="{i18n>mainViewTitle}"
          showNavButton="false"
          busy="{viewModel>/busy}"
          busyIndicatorDelay="{viewModel>/delay}">
          <subHeader>
              <OverflowToolbar>
                  <SearchField
                      id="searchField"
                      width="20%"
                      search=".onSearch" />
                  <ToolbarSpacer />
  ${customActionButtons}
              </OverflowToolbar>
          </subHeader>
          <content>
  ${useSmartControls ? `			<!-- Smart Filter Bar -->
              <smartFilterBar:SmartFilterBar
                  id="smartFilterBar"
                  entitySet="${entitySet}"
                  persistencyKey="SmartFilter_Explored"
                  basicSearchFieldName="SearchField"
                  enableBasicSearch="true">
              </smartFilterBar:SmartFilterBar>
              
              <!-- Smart Table -->
              <smartTable:SmartTable
                  id="smartTable"
                  entitySet="${entitySet}"
                  smartFilterId="smartFilterBar"
                  tableType="ResponsiveTable"
                  useExportToExcel="true"
                  useVariantManagement="true"
                  useTablePersonalisation="true"
                  header="{i18n>tableHeader}"
                  showRowCount="true"
                  enableAutoBinding="true"
                  class="sapUiResponsiveContentPadding"${navigationProperty ? '\n\titemPress=".onItemPress"' : ''}>
                  <smartTable:layoutData>
                      <FlexItemData growFactor="1" baseSize="0%" />
                  </smartTable:layoutData>
              </smartTable:SmartTable>` : 
  `			<!-- Standard Table -->
              <Table
                  id="table"
                  items="{/${entitySet}}"
                  growing="true"
                  growingThreshold="10"
                  growingScrollToLoad="true"${navigationProperty ? '\n\titemPress=".onItemPress"\n\tmode="SingleSelectMaster"' : ''}>
                  <headerToolbar>
                      <OverflowToolbar>
                          <Title text="{i18n>tableHeader}" level="H2" />
                          <ToolbarSpacer />
                          <Button 
                              icon="sap-icon://refresh"
                              tooltip="{i18n>refreshButtonTooltip}"
                              press=".onRefresh" />
                      </OverflowToolbar>
                  </headerToolbar>
                  <columns>
  ${standardTableColumns}
                  </columns>
                  <items>
                      <ColumnListItem${navigationProperty ? ' type="Navigation"' : ''}>
                          <cells>
  ${standardTableCells}
                          </cells>
                      </ColumnListItem>
                  </items>
              </Table>`}
  ${includeAnalytics ? `
  
              <!-- Analytics Section -->
              <Panel
                  headerText="{i18n>analyticsSectionTitle}"
                  class="sapUiMediumMarginTop"
                  expandable="true"
                  expanded="false">
                  <viz:Vizframe
                      id="idVizFrame"
                      uiConfig="{applicationSet:'fiori'}"
                      height="400px"
                      width="100%"
                      vizType="column">
                      <viz:dataset>
                          <vizData:FlattenedDataset
                              data="{/${entitySet}}">
                              <vizData:dimensions>
                                  <vizData:DimensionDefinition
                                      name="Category"
                                      value="{Name}" />
                              </vizData:dimensions>
                              <vizData:measures>
                                  <vizData:MeasureDefinition
                                      name="Value"
                                      value="{Value}" />
                              </vizData:measures>
                          </vizData:FlattenedDataset>
                      </viz:dataset>
                      <viz:feeds>
                          <viz:FeedItem
                              uid="valueAxis"
                              type="Measure"
                              values="Value" />
                          <viz:FeedItem
                              uid="categoryAxis"
                              type="Dimension"
                              values="Category" />
                      </viz:feeds>
                  </viz:Vizframe>
              </Panel>` : ''}
          </content>
          <footer>
              <OverflowToolbar>
                  <ToolbarSpacer />
                  <Button
                      text="{i18n>footerButtonText}"
                      type="Emphasized"
                      press=".onFooterPress" />
              </OverflowToolbar>
          </footer>
      </Page>
  </mvc:View>`;
  };
  
  /**
   * Genera il file i18n.properties per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Contenuto del file i18n
   */
  export const generateI18n = (formData) => {
    const {
      appTitle = "My Fiori App",
      appDescription = "SAP Fiori Application",
      includeCustomActions = false,
      customActions = [],
      smartFields = [],
      includeAnalytics = false
    } = formData;
  
    // Intestazione e titoli applicazione
    let i18n = `# Translations for SAP Fiori application
  # __ldi.translation.uuid=
  
  # App Descriptor
  appTitle=${appTitle}
  appDescription=${appDescription}
  
  # Main View
  mainViewTitle=${appTitle}
  tableHeader=Dati ${appTitle}
  searchFieldPlaceholder=Cerca...
  footerButtonText=Azione
  
  # Messaggi generali
  dataLoadedMessage=Dati caricati con successo
  saveSuccessMessage=Dati salvati con successo
  errorMessage=Si è verificato un errore
  refreshButtonTooltip=Aggiorna dati
  
  # Messaggi di conferma
  confirmDeleteTitle=Conferma eliminazione
  confirmDeleteMessage=Sei sicuro di voler eliminare questo elemento?
  yesButtonText=Sì
  noButtonText=No
  cancelButtonText=Annulla
  `;
  
    // Aggiungi traduzioni per le azioni personalizzate
    if (includeCustomActions && customActions.length > 0) {
      i18n += '\n# Azioni personalizzate\n';
      
      customActions.forEach(action => {
        const actionName = action.name.charAt(0).toLowerCase() + action.name.slice(1);
        const actionLabel = action.label || action.name;
        
        i18n += `${actionName}ButtonText=${actionLabel}\n`;
        i18n += `${actionName}ButtonTooltip=${actionLabel}\n`;
        i18n += `${actionName}SuccessMessage=${actionLabel} completata con successo\n`;
      });
    }
  
    // Aggiungi traduzioni per i campi Smart
    if (smartFields && smartFields.length > 0) {
      i18n += '\n# Colonne e campi\n';
      
      smartFields.forEach(field => {
        const fieldName = field.toLowerCase();
        i18n += `${fieldName}ColumnTitle=${field}\n`;
        i18n += `${fieldName}Label=${field}\n`;
        
        // Se il campo è ID, aggiungi anche un messaggio per "non trovato"
        if (field === 'ID' || field === 'Id' || field.includes('ID') || field.includes('Id')) {
          i18n += `${fieldName}NotFound=${field} non trovato\n`;
        }
      });
    }
  
    // Aggiungi traduzioni per la parte analitica
    if (includeAnalytics) {
      i18n += `
  # Analytics
  analyticsSectionTitle=Analisi Dati
  chartTitle=Visualizzazione Dati
  valueAxisTitle=Valore
  categoryAxisTitle=Categoria
  `;
    }
  
    // Aggiungi traduzioni per la vista detail
    i18n += `
  # Detail View
  detailViewTitle=Dettaglio
  editButtonText=Modifica
  deleteButtonText=Elimina
  backButtonText=Indietro
  notFoundTitle=Non Trovato
  notFoundText=La risorsa richiesta non è stata trovata
  `;
  
    return i18n;
  };
  
  /**
   * Genera il file AppView.xml per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice della vista XML dell'app
   */
  export const generateAppView = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      includeFlexibleColumnLayout = false
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
  
    if (includeFlexibleColumnLayout) {
      return `<mvc:View
      displayBlock="true"
      controllerName="${fullAppId}.controller.App"
      xmlns="sap.m"
      xmlns:mvc="sap.ui.core.mvc"
      xmlns:f="sap.f">
      <App id="app">
          <f:FlexibleColumnLayout
              id="flexibleColumnLayout"
              backgroundDesign="Translucent"
              layout="{/layout}"/>
      </App>
  </mvc:View>`;
    } else {
      return `<mvc:View
      displayBlock="true"
      controllerName="${fullAppId}.controller.App"
      xmlns="sap.m"
      xmlns:mvc="sap.ui.core.mvc">
      <App id="app" />
  </mvc:View>`;
    }
  };
  
  /**
   * Genera il file AppController.js per un'applicazione Fiori
   * @param {Object} formData - Dati del form con la configurazione
   * @returns {String} Codice del controller dell'app
   */
  export const generateAppController = (formData) => {
    const {
      namespace = "com.mycompany",
      appId = "z.myfioriapp",
      includeFlexibleColumnLayout = false
    } = formData;
  
    const fullAppId = `${namespace}.${appId}`;
  
    if (includeFlexibleColumnLayout) {
      return `sap.ui.define([
      "sap/ui/core/mvc/Controller",
      "sap/ui/model/json/JSONModel",
      "sap/f/LayoutType"
    ], function(Controller, JSONModel, LayoutType) {
      "use strict";
  
      return Controller.extend("${fullAppId}.controller.App", {
        onInit: function() {
          var oModel = new JSONModel({
            layout: LayoutType.OneColumn
          });
          this.getView().setModel(oModel);
        }
      });
    });`;
    } else {
      return `sap.ui.define([
      "sap/ui/core/mvc/Controller"
    ], function(Controller) {
      "use strict";
  
      return Controller.extend("${fullAppId}.controller.App", {
        onInit: function() {
          // Initialization logic
        }
      });
    });`;
    }
  };