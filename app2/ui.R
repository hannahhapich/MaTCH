

ui <- dashboardPage(dark = T, 
                    help = T, 
                    fullscreen = T,
                    dashboardHeader(title = "MaTCH"),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "sidebarmenu",
                        menuItem("MaTCH Tool", 
                                 tabName = "match", 
                                 icon = icon("wand-magic-sparkles")),
                        menuItem("About", 
                                 tabName = "About", 
                                 icon = icon("question")),
                        menuItem("Download Test Data", 
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
                                                        #,
                                                              
                                                        # fluidRow(
                                                        #   box(width = 12,
                                                        #       footer = tags$small("All unknown terms for 'morphology' and 'material' will be matched via embedding similarity to existing terms in our database. The highest percent similarity
                                                        #                           match will display as default. However, if you wish to review the top five matches from the natural language processing model, you may do so here and override 
                                                        #                           the choice."),
                                                        #       title = prettySwitch("embedding_match_choice",
                                                        #                            label = "View Embedding Matches",
                                                        #                            inline = T,
                                                        #                            value = F,
                                                        #                            status = "success",
                                                        #                            fill = T),
                                                        #        collapsed = T
                                                        #       
                                                        #   )
                                                        # )
                                                        
                                                    )
                                                  )
                                           )))
                                ),
                                ## Plot ----
                                fluidRow(
                                  box(title = HTML(paste0("Data Summary")), 
                                      maximizable = T,
                                      width = 12,
                                      fluidRow(
                                        div(style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents7")
                                        ))
                                  ),
                                  box(title = HTML(paste0("Cleaned Data")), 
                                      maximizable = T,
                                      width = 12,
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
                                         shiny::HTML("<h5>Trash Taxonomy website is a portal for relational tables that relate trash survey nomenclature. It includes 7 relational tables and a tool that can be used to query the relational tables with trash survey sheets.
                                  This tool was developed by collating and comparing categories and terms used from over 50 commonly used in trash survey sheets.</h5>"),
                                         shiny::HTML("<h5>We are grateful for the funding for this project provided by the National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>")
                                         
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
                                         #HTML('<iframe width="560" height="315" src='https://dbdiagram.io/e/64e7b4be02bd1c4a5e5d44fc/65cbb77fac844320ae118a92'> </iframe>'>),
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
                                         shiny::HTML("<h5>This tool queries the relational tables with an uploaded trash survey list. To use the tool, upload a csv file to the upload file tab. 
                                  The file needs to be a csv with one column named -material- and another named -items-. 
                                  The material should correspond to the item names in the same row.</h5>")
                                  ),
                                  column(3)
                                ),
                                tags$hr(),
                                
                                
                                # fluidRow(
                                #   column(3),
                                #   column(6,
                                #          
                                #          shiny::HTML("<br><br><center> <h1>How To Use</h1> </center><br>"),
                                #          shiny::HTML("<h5>In order to assist your navigation through both the relational tables and query tool functions of this app, please refer to the video tutorial below.</h5>"),
                                #          shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sqeLaJKyol8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                #          
                                #   ),
                                #   column(3)
                                # ),
                                
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
                                         shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
                                  ),
                                  column(3)
                                )
                                
                                
                                #end of about panel
                        ),

                        tabItem(tabName = "test",
                                
                                fluidRow(
                                  column(12,

                                         shiny::HTML("<br><br><center> <h1>Download Test Data</h1> </center><br>"),
                                         shiny::HTML("<h5>      Describe your data to download a template, and view what data cleaning can be perfomed by MaTCH! </h5>")

                                  )
                                ),
                                br(),
                                
                                #column(8,
                                       fluidRow( 
                                         column(4, 
                                                ## Preprocessing ----
                                                #fluidRow(
                                                  box(width = 12,
                                                      #collapsed = F,
                                                      #style = "height: 50vh; overflow-y: scroll",
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
                                                  #)
                                                ),
                                         column(8,
                                                box(title = HTML(paste0("Test Data")), 
                                                    maximizable = T,
                                                    width = 12,
                                                    div(style = "overflow-x: scroll",
                                                        DT::dataTableOutput("testDataDownload")
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
                                       #),
                                
                                fluidRow(
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
                                )
                        ),
                        
                        #Relational Tables ----
                        tabItem(tabName = "RelationalTables",
                                #titlePanel(tags$h4("View and Download Relational Tables")),
                                
                                fluidRow(
                                  column(12,
                                         shiny::HTML("<br><br><center> <h1>View and Download Relational Tables</h1> </center><br>")
                                  )
                                ), 
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3
                                  ),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Materials Alias Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table describes the aliases that can be used to describe material types and links them to a key term. Each row represents a unique material and each column is an alias for that material.</h5>"),
                                         checkboxInput("show1", "Show Table", width = '100%')
                                         
                                  ),
                                  column(3
                                  )
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData1', 'Download')
                                         )
                                         
                                  ),
                                  column(3)),
                                
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show1 == true",
                                                          DT::dataTableOutput('table1')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Materials Hierarchy Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table describes how the unique material types relate to one another in a hierarchical structure (ex: foam and rubber are a subset of plastic).</h5>"),
                                         #shinyTree(outputId = "materialhierarchy"),
                                         
                                         #collapsibleTreeOutput(outputId = "material_tree", width = "100%", height = "500px")
                                         checkboxInput("show2", "Show Table", width = '50%'),
                                         shinyTree::shinyTree(outputId = "materialhierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                                         
                                  ),
                                  column(1)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData2', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show2 == true",
                                                          DT::dataTableOutput('table2')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Items Alias Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table describes the aliases that can be used to describe item types and links them to a key term. Each row represents a unique item and each column is an alias for that item.</h5>"),
                                         checkboxInput("show3", "Show Table", width = '50%')
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData3', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show3 == true",
                                                          DT::dataTableOutput('table3')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Items Hierarchy Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table describes how the unique items relate to one another in a hierarchical structure (ex: forks, knives, and spoons all fall under utensils).</h5>"),
                                         #shinyTree(outputId = "itemhierarchy"),
                                         #div(style = "background-color: white;",
                                         #    collapsibleTreeOutput(outputId = "item_tree", width = "100%", height = "500px")
                                         #),
                                         checkboxInput("show4", "Show Table", width = '50%'),
                                         shinyTree::shinyTree(outputId = "itemshierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                                         
                                         
                                  ),
                                  column(1)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData4', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show4 == true",
                                                          DT::dataTableOutput('table4')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Material-Item Relational Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table relates the items, materials, and survey sheets used to make the other relational tables.</h5>"),
                                         checkboxInput("show5", "Show Table", width = '50%')
                                         
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData5', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show5 == true",
                                                          DT::dataTableOutput('table5')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Material-Density Relational Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table relates materials to all known material densities. Sources also displayed.</h5>"),
                                         checkboxInput("show9", "Show Table", width = '50%')
                                         
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData9', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show9 == true",
                                                          DT::dataTableOutput('table9')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Manufacturer Brand Relational Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table relates brand types to their respective manufacturer.</h5>"),
                                         checkboxInput("show6", "Show Table", width = '50%')
                                         
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData6', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show6 == true",
                                                          DT::dataTableOutput('table6')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Item-Brand Relational Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table relates brands to items.</h5>"),
                                         checkboxInput("show7", "Show Table", width = '50%')
                                         
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData7', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show7 == true",
                                                          DT::dataTableOutput('table7')
                                         )
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
                                         shiny::HTML("<br><br><center> <h1>Misaligned Categories Table</h1> </center><br>"),
                                         shiny::HTML("<h5>This table displays all categories that did not fit our item-material framework.</h5>"),
                                         checkboxInput("show8", "Show Table", width = '50%')
                                         
                                  ),
                                  column(3)
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('downloadData8', 'Download')
                                         )
                                         
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show8 == true",
                                                          DT::dataTableOutput('table8')
                                         )
                                  ), 
                                  column(1)
                                ),
                                
                                tags$hr(),
                                
                                fluidRow(
                                  column(3
                                  ),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Microplastic Color Alias</h1> </center><br>"),
                                         shiny::HTML("<h5>This table describes the aliases that can be used to describe microplastic colors. Each row represents a unique color and each column is an alias for that color.</h5>"),
                                         checkboxInput("show12", "Show Table", width = '100%')
                                         
                                  ),
                                  column(3
                                  )
                                ),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         tags$div(align = "center", 
                                                  downloadButton('download12', 'Download')
                                         )
                                         
                                  ),
                                  column(3)),
                                
                                
                                fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         conditionalPanel(condition = "input.show12 == true",
                                                          DT::dataTableOutput('table12')
                                         )
                                  ), 
                                  column(1)
                                ),
                                
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                                         shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
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
                                           helper(type = "inline",
                                                  title = "Upload Help",
                                                  content = c("To use the tool, upload a csv file to the upload file tab. The file needs to be a csv with one column named -material- and another named -items-. The material should correspond to the item names in the same row."),
                                                  size = "m"),
                                         
                                         downloadButton('downloadtest', 'Download Test Data'),
                                         
                                         checkboxGroupInput('variable', "Functions:",
                                                            c("More Specific Materials"="MoreSpecificMaterial",
                                                              "Less Specific Materials"="LessSpecificMaterial",
                                                              "More Specific Items"="MoreSpecificItem",
                                                              "Less Specific Items"="LessSpecificItem"))),
                                  column(10, 
                                         dataTableOutput('contents')
                                  )
                                  
                                ),
                                hr(),
                                fluidRow(
                                  column(3), 
                                  column(6,shiny::HTML("<br><br><center> <h4>View Key Alias Matches</h4> </center><br>")
                                  ),
                                  column(3)
                                  
                                ),
                                fluidRow(
                                  column(1),
                                  column(5, 
                                         dataTableOutput('contents1')
                                  ),
                                  column(5, 
                                         dataTableOutput('contents2')
                                  ), 
                                  column(1)
                                ),
                                fluidRow(
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
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
                                           helper(type = "inline",
                                                  title = "Selection Help",
                                                  content = c("Select if your study will include microplastics, macro-debris, or both."),
                                                  size = "m"),
                                         selectInput('environments', "Choose environment", c("", "Marine/Estuarine", "Riverine", "Terrestrial", "All")) %>%
                                           helper(type = "inline",
                                                  title = "Selection Help",
                                                  content = c("Select the environment your study will be conducted in, or include all."),
                                                  size = "m"),
                                         selectInput('media', "Choose media", c("", "Surface Water","Sediment")) %>%
                                           helper(type = "inline",
                                                  title = "Selection Help",
                                                  content = c("Select the media your study will be conducted in."),
                                                  size = "m"),
                                         selectInput('specificity', "Choose specificity", c("", "More Specific","Less Specific")) %>%
                                           helper(type = "inline",
                                                  title = "Selection Help",
                                                  content = c("Select how specific descriptor terms will be. More specific terms reccomended for scientific studies to increase comparability; less specific terms reccomended for volunteer groups to increase speed of surveying."),
                                                  size = "m"),
                                         
                                  ),
                                  
                                  column(10, 
                                         dataTableOutput('contents4')
                                  )
                                  
                                ),
                                
                                fluidRow(
                                  align="center",
                                  hr(),
                                  tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
                                )
                                #end of panel
                        )
                      )
                    )
)



