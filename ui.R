ui <- dashboardPage(dark = T, 
                    help = T, 
                    fullscreen = T,
                    dashboardHeader(title = "MaTCH"),
                    dashboardSidebar(
                      skin = "dark",
                      sidebarMenu(
                        id = "sidebarmenu",
                        menuItem("MaTCH Tool", 
                                 tabName = "match", 
                                 icon = icon("wand-magic-sparkles")),
                        menuItem("Data Template", 
                                 tabName = "test", 
                                 icon = icon("download")),
                        menuItem("About", 
                                 tabName = "About", 
                                 icon = icon("question"))
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
                          .dataTables_wrapper .dataTables_scroll { overflow: visible !important; }
                          .dataTables_wrapper { overflow: visible !important; }
                          table.dataTable tbody { overflow: visible !important; }
                          .shiny-input-container { z-index: 1000 !important; position: relative; }
                          .form-group { z-index: 1000 !important; position: relative; }
                          .selectize-control { z-index: 1002 !important; position: relative !important; }
                          .selectize-dropdown { z-index: 10000 !important; max-height: 300px; overflow-y: auto; }
                          .selectize-input { position: relative !important; }
                          .dropdown, .dropup { position: relative; }
                          .dropdown-menu { z-index: 10000 !important; }
                          table.dataTable tbody tr { position: static !important; z-index: auto; }
                          table.dataTable tbody tr td { padding: 8px !important; overflow: visible !important; }
                          table.dataTable td div { overflow: visible !important; }
                          table.dataTable tbody tr td select { position: relative; z-index: 1050 !important; }
                          .shiny-input-container select { z-index: 1050 !important; position: relative; }
                          .popover { z-index: 10001 !important; max-width: 400px; }
                          .popover-header { background-color: #555; color: white; font-weight: bold; }
                          .popover-body { background-color: #666; color: white; }
                        ")),
                        tags$script(HTML("
                          document.addEventListener('DOMContentLoaded', function() {
                            // Helper to close all popovers
                            function closeAllPopovers() {
                              document.querySelectorAll('[data-bs-toggle=\"popover\"]').forEach(el => {
                                const popover = bootstrap.Popover.getInstance(el);
                                if (popover) {
                                  popover.hide();
                                }
                              });
                            }
                            
                            // Close all popovers when clicking outside
                            document.addEventListener('click', function(event) {
                              // Get all popover triggers and popovers
                              const triggers = document.querySelectorAll('[data-bs-toggle=\"popover\"]');
                              let isPopoverRelated = false;
                              
                              // Check if click is on a trigger or inside a popover
                              for (let trigger of triggers) {
                                const popover = bootstrap.Popover.getInstance(trigger);
                                const popoverElement = document.querySelector('[role=\"tooltip\"][data-popper-placement]') || 
                                                       $('.popover:visible')[0];
                                
                                if (trigger.contains(event.target)) {
                                  isPopoverRelated = true;
                                  break;
                                }
                                if (popoverElement && popoverElement.contains(event.target)) {
                                  isPopoverRelated = true;
                                  break;
                                }
                              }
                              
                              // If click is outside all popovers, close them all
                              if (!isPopoverRelated && !$(event.target).closest('.popover').length) {
                                closeAllPopovers();
                              }
                            });
                            
                            // Also close popovers when pressing Escape
                            document.addEventListener('keydown', function(event) {
                              if (event.key === 'Escape') {
                                closeAllPopovers();
                              }
                            });
                            
                            // Setup for each selectize instance
                            function setupSelectize($select) {
                              if (!$select[0].selectize) {
                                setTimeout(() => setupSelectize($select), 100);
                                return;
                              }
                              
                              const selectize = $select[0].selectize;
                              
                              selectize.on('dropdown_open', function() {
                                setTimeout(() => {
                                  const $dropdown = $(selectize.$dropdown);
                                  const $input = $(selectize.$control);
                                  
                                  // Move dropdown to body to escape table constraints
                                  $dropdown.appendTo('body').css({
                                    'position': 'fixed',
                                    'z-index': '10000',
                                    'margin': '0',
                                    'box-sizing': 'border-box'
                                  });
                                  
                                  // Get position from control element
                                  const inputRect = $input[0].getBoundingClientRect();
                                  $dropdown.css({
                                    'left': inputRect.left + 'px',
                                    'top': (inputRect.bottom) + 'px',
                                    'width': inputRect.width + 'px'
                                  });
                                  
                                  // Reposition on scroll while dropdown is open
                                  const scrollHandler = function() {
                                    if ($dropdown.is(':visible')) {
                                      const updatedRect = $input[0].getBoundingClientRect();
                                      $dropdown.css({
                                        'left': updatedRect.left + 'px',
                                        'top': (updatedRect.bottom) + 'px',
                                        'width': updatedRect.width + 'px'
                                      });
                                    }
                                  };
                                  
                                  $(window).on('scroll.selectize', scrollHandler);
                                  
                                  // Store handler reference for cleanup
                                  selectize._scrollHandler = scrollHandler;
                                }, 10);
                              });
                              
                              selectize.on('dropdown_close', function() {
                                // Remove scroll listener when dropdown closes
                                if (selectize._scrollHandler) {
                                  $(window).off('scroll.selectize', selectize._scrollHandler);
                                }
                              });
                            }
                            
                            // Initialize all select elements
                            $('select').each(function() {
                              setupSelectize($(this));
                            });
                            
                            // Re-initialize when table updates
                            $(document).on('draw.dt', function() {
                              setTimeout(() => {
                                $('select').each(function() {
                                  if (!this._selectizeSetup) {
                                    this._selectizeSetup = true;
                                    setupSelectize($(this));
                                  }
                                });
                              }, 200);
                            });
                          });
                        "))
                      ),
                      tabItems(
                        tabItem(tabName = "match",
                                br(),
                                # Mode Selection Tabs
                                bs4Dash::tabBox(
                                  id = "matchModeTabs",
                                  width = 12,
                                  tabPanel("Microplastic - Particle",
                                    fluidRow(
                                  column(2,
                                         fluidRow(style = "display: flex; align-items: flex-end;",
                                                  column(12, 
                                                         fileInput('particleData', "Choose CSV File", multiple = FALSE,
                                                                   placeholder = ".csv",
                                                                   accept=c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv")) %>%
                                                           bs4Dash::popover(
                                                             title = "Upload microplastics or trash data as a .csv.",
                                                             content = "File Upload", placement = "right"
                                                           )
                                                  )
                                         )
                                  ),
                                  column(8,
                                         fluidRow( 
                                           column(10, 
                                                  ## Preprocessing ----
                                                  fluidRow(
                                                    box(width = 12,
                                                        collapsed = T,
                                                        style = "height: 27vh; overflow-y: auto;",
                                                        footer = tags$small("Options for rescaling and mass conversion techniques."),
                                                        title = "Advanced Settings",
                                                        tags$div(style = "display: none;",
                                                          fluidRow(
                                                            box(width = 12,
                                                                collapsed = T,
                                                                footer = tags$small("'jenks' bins data via natural break classification from inherent groups within the data (see Jenks Natural Breaks Algorithm).
                                                                                    'equal' divides the range into 'n' parts.
                                                                                    'sd' creates classes proportionate to the standard deviation of the data provided"),
                                                                title = "Alpha Value Calculation",
                                                                br(),
                                                                selectInput('binning_type', "Binning Technique for Alpha Calculation", c("jenks","equal","sd")),
                                                                br(),
                                                                numericInput('bin_number', "Number of Bins to Fit Regression", 5, min = 5)
                                                            )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("For fibers with unknown width and height, the following values will be used. Default values are from Suaria et al. (2020) and represent Q1, Q2, and Q3 of their measured microfiber diameters."),
                                                              title = "Fiber Width Settings",
                                                              prettySwitch("fiber_auto_width",
                                                                           label = "Use Fiber Auto Width",
                                                                           inline = T,
                                                                           value = T,
                                                                           status = "success",
                                                                           fill = T),
                                                              br(),
                                                              tags$small("When toggled on, fiber width values shown here will be auto applied to all fibers in the database, reguardless if width/min feret existis in the database. Can only be turned off if alternate width values are provided."),
                                                              br(),
                                                              br(),
                                                              numericInput('fiber_min', "Fiber Minimum Diameter (microns)", 15, min = 1),
                                                              br(),
                                                              numericInput('fiber_med', "Fiber Median Diameter (microns)", 16.7, min = 1),
                                                              br(),
                                                              numericInput('fiber_max', "Fiber Max Diameter (microns)", 20.4, min = 1)
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("For films with unknown height, the following values will be used. Default values are from materials databases that report commonly used film thicknesses for consumer products and industrial/agricultural uses."),
                                                              title = "Film Thickness Settings",
                                                              br(),
                                                              numericInput('film_min', "Film Min Thickness (microns)", 20, min = 1, max = 5000),
                                                              br(),
                                                              numericInput('film_med', "Film Median Thickness (microns)", 40, min = 1, max = 5000),
                                                              br(),
                                                              numericInput('film_max', "Film Max Thickness (microns)", 100, min = 1, max = 5000)
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("If you do not have a polymer identification for every particle, this option will create a weighted average of polymer densities for a given
                                                                                  morphology, and apply this average density to particles of unknown polymer type within that morphology. You may also choose to weight by sample,
                                                                                  or to not wieght by individual morphology type."),
                                                              title = prettySwitch("polymer_avg_decision",
                                                                                   label = "Polymer Weighted Average",
                                                                                   inline = T,
                                                                                   value = T,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              prettySwitch("morph_weight",
                                                                           label = "Weight by Morphology",
                                                                           inline = T,
                                                                           value = F,
                                                                           status = "success",
                                                                           fill = T),
                                                              prettySwitch("sample_weight",
                                                                           label = "Weight by Sample",
                                                                           inline = T,
                                                                           value = F,
                                                                           status = "success",
                                                                           fill = T)
                                                              
                                                          )
                                                        )
                                                        
                                                    )
                                                  )
                                           ))),
                                  column(2,
                                         downloadButton("downloadParticleTestData",
                                                       label = "Download test data",
                                                       style = "background-color: rgb(0,0,0); color: rgb(255,255,255); width:100%;")
                                  )
                                
                                ),
                                ## Plot ----
                                fluidRow(
                                  # box(title = HTML(paste0("Data Summary")), 
                                  #     maximizable = T,
                                  #     width = 12,
                                  #     fluidRow(
                                  #       div(style = "overflow-x: scroll",
                                  #           DT::dataTableOutput("contents7")
                                  #       ))
                                  # ),
                                  box(title = HTML(paste0("Cleaned Data")), 
                                      maximizable = T,
                                      width = 12,
                                      downloadButton("downloadData", "Download Full Dataset"),
                                      fluidRow(
                                        div(style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents5")
                                        ))
                                  ),
                                  box(title = HTML(paste0("Material Terms")), 
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      fluidRow(
                                        div(#style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents8")
                                        ))
                                  ),
                                  box(title = HTML(paste0("Morphology Terms")), 
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      fluidRow(
                                        div(#style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents9")
                                        ))
                                  ),
                                  box(title = HTML(paste0("Material Plot")),
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      tags$h5("By count"),
                                      plotlyOutput("plot1"),
                                      conditionalPanel(
                                        condition = "output.plot1MassExists",
                                        tags$h5("By mass"),
                                        plotlyOutput("plot1Mass")
                                      )
                                  ),
                                  box(title = HTML(paste0("Morphology Plot")),
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      tags$h5("By count"),
                                      plotlyOutput("plot2"),
                                      conditionalPanel(
                                        condition = "output.plot2MassExists",
                                        tags$h5("By mass"),
                                        plotlyOutput("plot2Mass")
                                      )
                                  )
                                ),
                                
                                tags$hr(),
                                
                                fluidRow(
                                  column(12,
                                         align="center",
                                         hr(),
                                         tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://doi.org/10.1021/acs.est.4c02406 ES&T.")
                                  )
                                )
                                  ), # End tabPanel - Microplastic - Particle
                                  
                                  tabPanel("Microplastic - Concentration",
                                    br(),
                                    fluidRow(
                                      column(2,
                                             fluidRow(style = "display: flex; align-items: flex-end;",
                                                      column(12, 
                                                             fileInput('particleDataConc', "Choose CSV File", multiple = FALSE,
                                                                       placeholder = ".csv",
                                                                       accept=c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")) %>%
                                                               bs4Dash::popover(
                                                                 title = "Upload microplastics concentration data as a .csv.",
                                                                 content = "File Upload", placement = "right"
                                                               )
                                                      )
                                             )
                                      ),
                                      column(8,
                                             fluidRow( 
                                               column(10, 
                                                      ## Preprocessing ----
                                                      box(width = 12,
                                                          title = "Rescaling Settings",
                                                          collapsed = T,
                                                          br(),
                                                          numericInput('corrected_min_conc', "Corrected Particle Range Minimum (microns)", 20, min = 1),
                                                          br(), 
                                                          numericInput('corrected_max_conc', "Corrected Particle Range Maximum (microns)", 5000, min = 1),
                                                          footer = tags$small("Note: if inputting a study media, options include 'marine surface', 'marine sediment', 'freshwater surface', 'freshwater sediment', 'biota', 'effluent.' (See Kooi et al., 2021 (doi.org/10.1016/j.watres.2021.117429) for values), and 'drinking water' (Singh et al., in prep).")
                                                      )
                                               ))),
                                      column(2,
                                             downloadButton("downloadConcentrationTestData",
                                                           label = "Download test data",
                                                           style = "background-color: rgb(0,0,0); color: rgb(255,255,255); width:100%;")
                                      )
                                    ),
                                    ## Plot ----
                                    fluidRow(
                                      box(title = HTML(paste0("Cleaned Data")), 
                                          maximizable = T,
                                          width = 12,
                                          downloadButton("downloadDataConc", "Download Full Dataset"),
                                          fluidRow(
                                            div(style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents5Conc")
                                            ))
                                      )
                                      # HIDDEN FOR NOW - Material Terms, Morphology Terms, Material Plot, Morphology Plot
                                      # box(title = HTML(paste0("Material Terms")), 
                                      #     maximizable = T,
                                      #     collapsed = T,
                                      #     width = 6,
                                      #     fluidRow(
                                      #       div(#style = "overflow-x: scroll",
                                      #           DT::dataTableOutput("contents8Conc")
                                      #       ))
                                      # ),
                                      # box(title = HTML(paste0("Morphology Terms")), 
                                      #     maximizable = T,
                                      #     collapsed = T,
                                      #     width = 6,
                                      #     fluidRow(
                                      #       div(#style = "overflow-x: scroll",
                                      #           DT::dataTableOutput("contents9Conc")
                                      #       ))
                                      # ),
                                      # box(title = HTML(paste0("Material Plot")),
                                      #     maximizable = T,
                                      #     collapsed = T,
                                      #     width = 6,
                                      #     plotlyOutput("plot1Conc")
                                      # ),
                                      # box(title = HTML(paste0("Morphology Plot")),
                                      #     maximizable = T,
                                      #     collapsed = T,
                                      #     width = 6,
                                      #     plotlyOutput("plot2Conc")
                                      # )
                                    ),
                                    
                                    tags$hr(),
                                    
                                    fluidRow(
                                      column(12,
                                             align="center",
                                             hr(),
                                             tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://doi.org/10.1021/acs.est.4c02406 ES&T.")
                                      )
                                    )
                                  ), # End tabPanel - Microplastic - Concentration
                                  
                                  tabPanel("Trash",
                                    br(),
                                    fluidRow(
                                      column(2,
                                             fluidRow(style = "display: flex; align-items: flex-end;",
                                                      column(12, 
                                                             fileInput('particleDataTrash', "Choose CSV File", multiple = FALSE,
                                                                       placeholder = ".csv",
                                                                       accept=c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")) %>%
                                                               bs4Dash::popover(
                                                                 title = "Upload trash data as a .csv.",
                                                                 content = "File Upload", placement = "right"
                                                               )
                                                      )
                                             )
                                      ),
                                      column(8,
                                             fluidRow()
                                      ),
                                      column(2,
                                             downloadButton("downloadTrashTestData",
                                                           label = "Download test data",
                                                           style = "background-color: rgb(0,0,0); color: rgb(255,255,255); width:100%;")
                                      )
                                    ),
                                    ## Plot ----
                                    fluidRow(
                                      box(title = HTML(paste0("Cleaned Data")), 
                                          maximizable = T,
                                          width = 12,
                                          downloadButton("downloadDataTrash", "Download Full Dataset"),
                                          fluidRow(
                                            div(style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents5Trash")
                                            ))
                                      ),
                                      box(title = HTML(paste0("Material Terms")), 
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          fluidRow(
                                            div(#style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents8Trash")
                                            ))
                                      ),
                                      box(title = HTML(paste0("Morphology Terms")), 
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          fluidRow(
                                            div(#style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents9Trash")
                                            ))
                                      ),
                                      box(title = HTML(paste0("Material Plot")),
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          tags$h5("By count"),
                                          plotlyOutput("plot1Trash"),
                                          conditionalPanel(
                                            condition = "output.plot1TrashMassExists",
                                            tags$h5("By mass"),
                                            plotlyOutput("plot1TrashMass")
                                          )
                                      ),
                                      box(title = HTML(paste0("Morphology Plot")),
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          tags$h5("By count"),
                                          plotlyOutput("plot2Trash"),
                                          conditionalPanel(
                                            condition = "output.plot2TrashMassExists",
                                            tags$h5("By mass"),
                                            plotlyOutput("plot2TrashMass")
                                          )
                                      )
                                    ),
                                    
                                    tags$hr(),
                                    
                                    fluidRow(
                                      column(12,
                                             align="center",
                                             hr(),
                                             tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://doi.org/10.1021/acs.est.4c02406 ES&T.")
                                      )
                                    )
                                  ) # End tabPanel - Trash
                                ) # End tabBox
                        ),
                        #About tab
                        tabItem(tabName = "About",
                                fluidRow(
                                  column(12,
                                         
                                         shiny::HTML("<br><br><center> <h1>Overview</h1> </center><br>"),
                                         shiny::HTML("<h5>MaTCH is a tool designed to relate microplastic and trash nomenclature through semantic harmonization, as well as perform other non-semantic alignment techniques. It consists separate drag and drop boxes to perform data cleaning for three different data structures: microplastic particle, microplastic concentration, and trash particle data. For more information on what types of harmonization operations can be performed for each data structure, see the 'Data Template' tab.</h5>"),
                                         shiny::HTML("<h5>We are grateful for the funding for this project provided by the Moore Institute for Plastic Pollution Research, The Possibility Lab, the National Science Foundation, University of California, Riverside, National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>")
                                         
                                  )
                                ),
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         shiny::HTML("<br><br><center> <h1>Microplastic particle volume calculation: decision tree</h1> </center><br>"),
                                         shiny::HTML("<h5 style='max-width: 700px; margin: 0 auto 20px auto; line-height: 1.6;'>Decision tree outlining how microplastic particle volume will be calculated. This is dependent on morphology type, data availability, and model error. Methods are listed from top to bottom, most preferable to least preferable. The data required to use each method is listed in the parentheses following the citation.</h5>"),
                                         align = "center",
                                         img(src = "particle_vol_selection_tree.png", style = 'display: block; margin: 0 auto; width: 100%; height: auto;')
                                  ),
                                  column(1)
                                ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         shiny::HTML("<br><br><center> <h1>About the Relational Tables</h1> </center><br>"),
                                         align = "center",
                                         img(src = "db_diagram.JPG", style = 'display: block; margin: 0 auto; width: 100%; height: auto;'),
                                         shiny::HTML("<h5 style='max-width: 700px; margin: 20px auto 0 auto; line-height: 1.6;'>These relational tables describe alias relationships (words that mean the same thing) and hierarchical relationships (words that are nested groups within one another).</h5>")
                                  ),
                                  column(1)
                                ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Funded By</h1> </center><br>"),
                                         align = "center",
                                         img(src="https://www.ctc-n.org/sites/default/files/logo_nrel_c.jpg", width = "50%"),
                                         img(src="NOAA.png", width = "50%"),
                                         img(src="NMSF.png", width = "50%"),
                                         img(src="boi.png", width = "50%")
                                  ),
                                  column(3)
                                ),
                                
                                tags$hr(),
                                
                                fluidRow(
                                  column(12,
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://doi.org/10.1021/acs.est.4c02406 ES&T.")
                                  )
                                  )
                                
                                
                                
                                #end of about panel
                        ),
                        
                        tabItem(tabName = "test",
                                
                                fluidRow(
                                  column(12,
                                         
                                         shiny::HTML("<br><br><center> <h1>Format Your Data</h1> </center><br>"),
                                         shiny::HTML("<h5>      Describe your data to download a template, and view what data cleaning can be perfomed by MaTCH! </h5>")
                                         
                                  )
                                ),
                                br(),
                                
                                fluidRow( 
                                  column(4, 
                                         ## Preprocessing ----
                                         box(width = 12,
                                             title = "Choose Data Type",
                                             selectInput('reporting_level', "Data Reporting Level", c("Sample (particles/volume)", "Particle")) %>%
                                               bs4Dash::popover(title = "Data Reporting Level",
                                                       content = "Choose the level at which your data was reported. Sample data is in the format of plastics concentrations as a particle count per volume.
                                                           Particle data contains individual rows for each particle found detailing its characteristics.",
                                                       placement = "right"),
                                             "Known Data Characteristics",
                                             checkboxGroupInput('characteristics', 
                                                                "", 
                                                                br(),
                                                                choices = NULL),
                                             
                                             box(width = 12,
                                                 collapsed = T,
                                                 title = "Advanced Data Characteristics",
                                                 checkboxGroupInput('advanced', 
                                                                    "", 
                                                                    choices = NULL)
                                             )
                                         )
                                  ),
                                  column(8,
                                         box(title = HTML(paste0("Data Template")), 
                                             maximizable = T,
                                             width = 12,
                                             downloadButton("downloadTemplate", "Download Data Template"),
                                             fluidRow(
                                               div(style = "overflow-x: scroll",
                                                   DT::dataTableOutput("testDataDownload")
                                               )
                                             )
                                         ),
                                         box(title = "Functions Available",
                                             maximizable = T,
                                             width = 12,
                                             HTML(paste0("Cleaning functions able to be performed by Match:")),
                                             textOutput("function1"),
                                             textOutput("function2"),
                                             textOutput("function3")
                                         )
                                         
                                  )
                                ),
                                
                                fluidRow(
                                  column(12,
                                         align="center",
                                         hr(),
                                         tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://doi.org/10.1021/acs.est.4c02406 ES&T.")
                                  )
                                )
                        )
                      )
                    )
)



