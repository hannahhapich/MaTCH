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
                        menuItem("About", 
                                 tabName = "About", 
                                 icon = icon("question")),
                        menuItem("Data Template", 
                                 tabName = "test", 
                                 icon = icon("download")),
                        menuItem("Relational Tables", 
                                 tabName = "RelationalTables", 
                                 icon = icon("table")),
                        menuItem("Query Tool", 
                                 tabName = "query", 
                                 icon = icon("sliders")),
                        menuItem("Surveys for Download", 
                                 tabName = "survey", 
                                 icon = icon("pen-to-square"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "match",
                                br(),
                                fluidRow(
                                  column(2,
                                         fluidRow(style = "display: flex; align-items: flex-end;",
                                                  column(9, 
                                                         fileInput('particleData', "Choose CSV File", multiple = FALSE,
                                                                   placeholder = ".csv",
                                                                   accept=c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv")) %>%
                                                           bs4Dash::popover(
                                                             title = "Upload microplastics or trash data as a .csv.",
                                                             content = "File Upload", placement = "right"
                                                           ),
                                                         prettySwitch("share_decision",
                                                                      label = "Share Your Data?",
                                                                      inline = T,
                                                                      value = T,
                                                                      status = "success",
                                                                      fill = T) %>%
                                                           popover(
                                                             title = "If you like, we share your uploaded data table and settings with the plastic community. By default, all data will be licensed under Creative Commons Attribution 4.0 International (CC BY 4.0). Uploaded data tables will appear here: https://osf.io/rjg3c. If you have particles of known density, volume, or mass that you can share, please upload a .csv file.",
                                                             content = "Share Decision", placement = "right"
                                                           )
                                                  )
                                         )
                                  ),
                                  column(8,
                                         fluidRow( 
                                           column(7, 
                                                  ## Preprocessing ----
                                                  fluidRow(
                                                    box(width = 12,
                                                        collapsed = T,
                                                        style = "height: 27vh; overflow-y: auto;",
                                                        footer = tags$small("Options for rescaling and mass conversion techniques."),
                                                        title = "Advanced Settings",
                                                        fluidRow(
                                                          box(width = 12,
                                                              title = "Rescaling Settings",
                                                              collapsed = T,
                                                              br(),
                                                              selectInput('concentration_type', "Known Particle Characteristic", c("length (um)", "width (um)", "mass (ug)","volume (um3)","surface area (um2)","specific surface area (g/m2)")),
                                                              br(),
                                                              numericInput('corrected_min', "Corrected Particle Range Minimum", 1, min = 1),
                                                              br(), 
                                                              numericInput('corrected_max', "Corrected Particle Range Maximum", 5000, min = 1),
                                                              footer = tags$small("Select the measured characteristic of your particles over which to normalize. 
                                                                                   Note: if inputting a study media, options include 'marine surface', 'marine sediment', 'freshwater surface', 'freshwater sediment', 'biota', and 'effluent.' 
                                                                                   If known particle characteristic is length, 'drinking water' is an additional option.")
                                                          )
                                                        ),
                                                        
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("'jenks' bins data via natural break classification from inherent groups within the data (see Jenks Natural Breaks Algorithm).
                                                                                  'quantile' provides quantile breaks.
                                                                                  'equal' divides the range into 'n' parts.
                                                                                  'sd' creates classes proportionate to the standard deviation of the data provided"),
                                                              title = "Alpha Value Calculation",
                                                              br(),
                                                              selectInput('binning_type', "Binning Technique for Alpha Calculation", c("jenks","quantile","equal","sd")),
                                                              br(),
                                                              numericInput('bin_number', "Number of Bins to Fit Regression", 5, min = 5)
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
                                         selectInput(inputId = "download_selection",
                                                     label = downloadButton("download_data",
                                                                            style = "background-color: rgb(0,0,0); color: rgb(255,255,255);"),
                                                     choices = c("Particle Test Data",
                                                                 "Sample Test Data")) 
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
                                        div(style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents8")
                                        ))
                                  ),
                                  box(title = HTML(paste0("Morphology Terms")), 
                                      maximizable = T,
                                      collapsed = T,
                                      width = 6,
                                      fluidRow(
                                        div(style = "overflow-x: scroll",
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
                                )
                                
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
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>About the Relational Tables</h1> </center><br>"),
                                         align = "center",
                                         img(width = "700", src = "db_diagram.JPG", align = "center"),
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
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                # INSTRUCTIONAL SECTION
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                                         shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray. 2024. https://hannahhapich.shinyapps.io/match/ </h5>")
                                  ),
                                  column(3)
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
                                               popover(title = "Data Reporting Level",
                                                       content = "Choose the level at which your data was reported. Sample data is in the format of plastics concentrations as a particle count per volume.
                                                           Particle data contains individual rows for each particle found detailing its characteristics.",
                                                       placement = "right"),
                                             "Known Data Characteristics",
                                             checkboxGroupInput('characteristics', 
                                                                "", 
                                                                br(),
                                                                choices = NULL) %>%
                                               popover(title = "Data Characteristics",
                                                       content = "Choose the commonly associated metadata you have available to describe your particles.",
                                                       placement = "right"),
                                             
                                             box(width = 12,
                                                 collapsed = T,
                                                 title = "Advanced Data Characteristics",
                                                 checkboxGroupInput('advanced', 
                                                                    "", 
                                                                    choices = NULL) %>%
                                                   popover(
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
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://hannahhapich.shinyapps.io/match/")
                                )
                        ),
                        
                        #Relational Tables ----
                        tabItem(tabName = "RelationalTables",
                                
                                fluidRow(
                                  column(12,
                                         shiny::HTML("<br><br><center> <h1>View and Download Relational Tables</h1> </center><br>")
                                  )
                                ), 
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                    sortable(
                                      width = 12,
                                      #p(class = "text-center", paste("Column", i)),
                                      lapply(1:length(titles), FUN = function(j) {
                                        box(
                                          title = titles[j], 
                                          width = 12,
                                          collapsed = T,
                                          style = 'overflow-x: scroll',
                                          footer = tags$div(align = "center", captions[j]), 
                                                   DT::dataTableOutput(outputId = view_code[j])
                                        )
                                      }), 
                                      box(
                                        width = 12,
                                        collapsed = T,
                                        title = "Items Hierarchy Tree",
                                        shinyTree::shinyTree(outputId = "itemshierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                                      ),
                                      box(
                                        width = 12, 
                                        collapsed = T,
                                        title = "Materials Hierarchy Tree",
                                        shinyTree::shinyTree(outputId = "materialhierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)   
                                      )
                                    )
                                ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                                         shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray. 2024. https://hannahhapich.shinyapps.io/match/</h5>")
                                  ),
                                  column(3)
                                )
                        ),
                        #end of about tab
                        
                        #About tab
                        tabItem(tabName = "query",
                                fluidRow(
                                  column(12,
                                         
                                         shiny::HTML("<br><br><center> <h1>Query the Relational Tables with Survey Sheets</h1> </center><br>")
                                  )
                                ),
                                
                                fluidRow(
                                  column(2, 
                                         prettySwitch("share_decision0",
                                                      label = "Share Your Data?",
                                                      inline = T,
                                                      value = T,
                                                      status = "success",
                                                      fill = T),
                                         fileInput('df', "Choose CSV File", multiple = FALSE, accept = c(".csv"))%>%
                                           popover(placement = "right",
                                                  title = "Upload Help",
                                                  content = c("To use the tool, upload a csv file to the upload file tab. The file needs to be a csv with one column named -material- and another named -items-. The material should correspond to the item names in the same row.")),
                                         
                                         downloadButton('downloadtest', 'Download Test Data'),
                                         
                                         checkboxGroupInput('variable', "Functions:",
                                                            c("More Specific Materials"="MoreSpecificMaterial",
                                                              "Less Specific Materials"="LessSpecificMaterial",
                                                              "More Specific Items"="MoreSpecificItem",
                                                              "Less Specific Items"="LessSpecificItem"))),
                                  column(10, 
                                         fluidRow(
                                           box(
                                             width = 4,
                                             title = "Relational Results",
                                             style = 'overflow-x: scroll; height: 50vh;',
                                             maximizable = T,
                                             dataTableOutput('contents')  
                                           ),
                                           box(
                                             width = 4, 
                                             title = "Material Keys",
                                             style = 'overflow-x: scroll; height: 50vh;',
                                             maximizable = T,
                                             dataTableOutput('contents1')
                                           ),
                                           box(
                                             width = 4, 
                                             title = "Morphology Keys",
                                             style = 'overflow-x: scroll; height: 50vh;',
                                             maximizable = T,
                                             dataTableOutput('contents2')
                                           )  
                                         )
                                  )
                                  
                                ),
                                hr(),
                                fluidRow(
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://hannahhapich.shinyapps.io/match/")
                                )
                                #end of about panel
                        ),
                        
                        tabItem(tabName = "survey",
                                fluidRow(
                                  column(12,
                                         
                                         shiny::HTML("<br><br><center> <h1>Surveys for Download</h1> </center><br>"),
                                         shiny::HTML("<h5>View and download suggested trash and microplastic surveys to fit your study needs.</h5>")
                                         
                                  )
                                ),
                                
                                fluidRow(
                                  column(2, 
                                         selectInput('sizeRange', "Choose size range", c("", "Micro","Macro","All")) %>%
                                           popover(placement = "right",
                                                  title = "Selection Help",
                                                  content = c("Select if your study will include microplastics, macro-debris, or both.")),
                                         selectInput('environments', "Choose environment", c("", "Marine/Estuarine", "Riverine", "Terrestrial", "All")) %>%
                                           popover(placement = "right",
                                                  title = "Selection Help",
                                                  content = c("Select the environment your study will be conducted in, or include all.")),
                                         selectInput('media', "Choose media", c("", "Surface Water","Sediment")) %>%
                                           popover(placement = "right",
                                                  title = "Selection Help",
                                                  content = c("Select the media your study will be conducted in.")),
                                         selectInput('specificity', "Choose specificity", c("", "More Specific","Less Specific")) %>%
                                           popover(placement = "right",
                                                  title = "Selection Help",
                                                  content = c("Select how specific descriptor terms will be. More specific terms reccomended for scientific studies to increase comparability; less specific terms reccomended for volunteer groups to increase speed of surveying.")),
                                         
                                  ),
                                  
                                  column(10, 
                                         dataTableOutput('contents4')
                                  )
                                  
                                ),
                                
                                fluidRow(
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray. 2024. https://hannahhapich.shinyapps.io/match/")
                                )
                                #end of panel
                        )
                      )
                    )
)



