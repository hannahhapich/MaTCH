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
                                      plotlyOutput("plot1")
                                  ),
                                  box(title = HTML(paste0("Morphology Plot")),
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      plotlyOutput("plot2")
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
                                                      fluidRow(
                                                        box(width = 12,
                                                            collapsed = T,
                                                            style = "height: 27vh; overflow-y: auto;",
                                                            footer = tags$small("Rescaling settings for concentration data."),
                                                            title = "Advanced Settings",
                                                            fluidRow(
                                                              box(width = 12,
                                                                  title = "Rescaling Settings",
                                                                  collapsed = T,
                                                                  br(),
                                                                  selectInput('concentration_type_conc', "Known Particle Characteristic", c("length (um)", "width (um)", "mass (ug)","volume (um3)","surface area (um2)","specific surface area (g/m2)")),
                                                                  br(),
                                                                  numericInput('corrected_min_conc', "Corrected Particle Range Minimum", 1, min = 1),
                                                                  br(), 
                                                                  numericInput('corrected_max_conc', "Corrected Particle Range Maximum", 5000, min = 1),
                                                                  footer = tags$small("Select the measured characteristic of your particles over which to normalize. 
                                                                                       Note: if inputting a study media, options include 'marine surface', 'marine sediment', 'freshwater surface', 'freshwater sediment', 'biota', and 'effluent.' 
                                                                                       (See Kooi et al., 2021 (doi.org/10.1016/j.watres.2021.117429) for values).
                                                                                       If known particle characteristic is length, 'drinking water' is an additional option.")
                                                              )
                                                            )
                                                        )
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
                                      ),
                                      box(title = HTML(paste0("Material Terms")), 
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          fluidRow(
                                            div(#style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents8Conc")
                                            ))
                                      ),
                                      box(title = HTML(paste0("Morphology Terms")), 
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          fluidRow(
                                            div(#style = "overflow-x: scroll",
                                                DT::dataTableOutput("contents9Conc")
                                            ))
                                      ),
                                      box(title = HTML(paste0("Material Plot")),
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          plotlyOutput("plot1Conc")
                                      ),
                                      box(title = HTML(paste0("Morphology Plot")),
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          plotlyOutput("plot2Conc")
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
                                          plotlyOutput("plot1Trash")
                                      ),
                                      box(title = HTML(paste0("Morphology Plot")),
                                          maximizable = T,
                                          collapsed = T,
                                          width = 6,
                                          plotlyOutput("plot2Trash")
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
                                         shiny::HTML("<h5>MaTCH is a tool designed to relate microplastic and trash nomenclature through semantic harmonization, as well as perform other non-semantic alignment techniques. It consists of a single drag and drop box to perform all data cleaning, includes all relational tables used in the development of this algorithm, and a tool that can be used to query the relational tables with microplastic and trash survey sheets.</h5>"),
                                         shiny::HTML("<h5>We are grateful for the funding for this project provided by the Moore Institute for Plastic Pollution Research, The Possibility Lab, the National Science Foundation, University of California, Riverside, National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>")
                                         
                                  )
                                ),
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(12,
                                         
                                         shiny::HTML("<br><br><center> <h1>Detailed Instructions</h1> </center><br>"),
                                         shiny::HTML("<h5>Find here a detailed demonstration of the tool, including data formatting, multiple use cases, and information about advanced settings.</h5>"),
                                         tags$hr(),
                                         align = "center",
                                         shiny::HTML("<iframe width='70%' height='150%' src='https://www.youtube-nocookie.com/embed/Nze8bnf-lqA' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                                         
                                  )
                                ),
                                fluidRow(
                                  
                                  style = "height:400px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>About the Relational Tables</h1> </center><br>"),
                                         align = "center",
                                         img(width = "600", src = "db_diagram.JPG", style = 'display: block; margin-left: auto; margin-right: auto;'),
                                         shiny::HTML("<h5>These relational tables describe alias relationships (words that mean the same thing) and hierarchical relationships (words that are nested groups within one another). You can view or download these tables using the relational table tab above!</h5>")
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                # HOW
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>About the Query Tool</h1> </center><br>"),
                                         shiny::HTML("<h5>This tool queries the relational tables with an uploaded survey list. To use the tool, upload a csv file to the upload file tab. 
                                  The file needs to be a csv with one column named -material- and another named -items-. 
                                  The material should correspond to the item names in the same row.</h5>")
                                  ),
                                  column(3)
                                ),
                                tags$hr(),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                tags$hr(),
                                
                                
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
                                             style = "height: 70vh, overflow-y: auto",
                                             title = "Choose Data Type",
                                             selectInput('reporting_level', "Data Reporting Level", c("", "Sample (particles/volume)","Particle")) %>%
                                               bs4Dash::popover(title = "Data Reporting Level",
                                                       content = "Choose the level at which your data was reported. Sample data is in the format of plastics concentrations as a particle count per volume.
                                                           Particle data contains individual rows for each particle found detailing its characteristics.",
                                                       placement = "right"),
                                             "Known Data Characteristics",
                                             checkboxGroupInput('characteristics', 
                                                                "", 
                                                                br(),
                                                                choices = NULL) %>%
                                               bs4Dash::popover(title = "Data Characteristics",
                                                       content = "Choose the commonly associated metadata you have available to describe your particles.",
                                                       placement = "right"),
                                             
                                             box(width = 12,
                                                 collapsed = T,
                                                 title = "Advanced Data Characteristics",
                                                 checkboxGroupInput('advanced', 
                                                                    "", 
                                                                    choices = NULL) %>%
                                                   bs4Dash::popover(
                                                     title = "Choose from advanced, less commonly associated metadata you may have to describe your particles.",
                                                     content = "Share Decision", placement = "right"
                                                   )
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



