

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
                                                        style = "height: 50vh; overflow-y: auto;",
                                                        footer = tags$small("Options for rescaling and mass conversion techniques."),
                                                        title = "Advanced Settings",
                                                        fluidRow(
                                                          box(width = 12,
                                                              #footer = tags$small("Signal thresholding technique, value, and histogram threshold plot."),
                                                              title = "Rescaling Settings",
                                                              collapsed = T,
                                                              br(),
                                                              selectInput('concentration_type', "Known Particle Characteristic", c("length (um)", "width (um)", "mass (ug)","volume (um3)","surface area (um2)","specific surface area (g/m2)")) %>%
                                                                helper(type = "inline",
                                                                       title = "Selection Help",
                                                                       content = c("Select the measured characteristic of your particles over which to normalize"),
                                                                       size = "m"),
                                                              br(),
                                                              numericInput('corrected_min', "Corrected Particle Range Minimum", 1, min = 1),
                                                              br(), 
                                                              numericInput('corrected_max', "Corrected Particle Range Maximum", 5000, min = 1)
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
                                                              selectInput('binning_type', "Binning Technique for Alpha Calculation", c("jenks","quantile","equal","sd")) %>%
                                                                helper(type = "inline",
                                                                       title = "Selection Help",
                                                                       content = c("Select the measured characteristic of your particles over which to normalize"),
                                                                       size = "m"),
                                                              br(),
                                                              numericInput('bin_number', "Number of Bins to Fit Regression", 5, min = 5)
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Smoothing can enhance signal to noise and uses the SG filter with the polynomial order specified, 3 default usually works well. 
                                                                            Derivative transformation uses the order specified. 
                                                                            If doing identification with a derivative library, 1 is required, 0 should be used if no derivative transformation is desired. 
                                                                            Smoothing uses the SG filter on an window of points, specifying the wavenumber window larger will make the spectra more smooth.
                                                                            The absolute value does something similar to intensity correction to make the spectra more absorbance-like."),
                                                              title =  prettySwitch(inputId = "smooth_decision",
                                                                                    label = "Smoothing/Derivative",
                                                                                    inline = T,
                                                                                    value = T,
                                                                                    status = "success",
                                                                                    fill = T),
                                                              sliderInput("smoother", "Polynomial", min = 0, max = 5, value = 3),
                                                              sliderInput("derivative_order", "Derivative Order", min = 0, max = 3, value = 1),
                                                              sliderInput("smoother_window", "Wavenumber Window", min = 50, max = 200, value = 60, step = 5),
                                                              prettySwitch("derivative_abs", 
                                                                           label = "Absolute Value",  
                                                                           inline = T,
                                                                           value = T,
                                                                           status = "success",
                                                                           fill = T))),
                                                        fluidRow(
                                                          box(width = 12,
                                                              footer = tags$small("Options for conforming spectra to a new wavenumber resolution.
                                                                                                Conformation technique specifies the strategy for performing the conformation. 
                                                                                                Nearest will use the nearest value to the wavenumber resolution specified, this is 
                                                                                                faster but less accurate. Linear Interpolation will perform a linear regression between 
                                                                                                the nearest points to identify the intensity values at the new wavenumbers. Wavenumber Resolution 
                                                                                                will set the step size in wavenumbers for the new wavenumber values."),
                                                              title = prettySwitch("conform_decision",
                                                                                   label = "Conform Wavenumbers",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              collapsed = T,
                                                              selectInput(inputId = "conform_selection", 
                                                                          label = "Conformation Technique", 
                                                                          choices = c("Nearest" = "roll",
                                                                                      "Linear Interpolation" = "interp")), 
                                                              br(),
                                                              sliderInput("conform_res", "Wavenumber Resolution", min = 4, max = 16, value = 5)
                                                              
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(
                                                            width = 12,
                                                            collapsed = T,
                                                            footer = tags$small("Open Specy assumes spectra are in Absorbance units. If the uploaded spectrum is not in absorbance units, 
                                                                    use this input to specify the units to convert from.The transmittance adjustment uses the log10(1/T) calculation 
                                                                    which does not correct for system and particle characteristics. The reflectance adjustment uses the Kubelka-Munk 
                                                                    equation (1-R)2/(2*R). We assume that the reflectance is formatted as a percent from 1-100 and first correct the 
                                                                    intensity by dividing by 100 so that it fits the form expected by the equation. If none is selected, Open Specy
                                                                    assumes that the uploaded data is an absorbance spectrum."),
                                                            title =  prettySwitch(inputId = "intensity_decision",
                                                                                  label = "Intensity Adjustment",
                                                                                  value = F,
                                                                                  inline = T,
                                                                                  status = "success",
                                                                                  fill = T),
                                                            radioButtons("intensity_corr", "Intensity Units",
                                                                         c("Absorbance" = "none", "Transmittance" = "transmittance", "Reflectance" = "reflectance"))
                                                          )),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("This algorithm automatically fits to the baseline by fitting 
                                                                                     polynomials of the provided order to the whole spectrum using the iModPolyFit algorithm."),
                                                              title = prettySwitch("baseline_decision",
                                                                                   label = "Baseline Correction",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              sliderInput("baseline", "Baseline Correction Polynomial", min = 1, max = 20, value = 8)
                                                          )),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching.
                                                                                     These options control the maximum and minimum wavenumbers in the range to crop the spectra."),
                                                              title =  prettySwitch("range_decision",
                                                                                    label = "Range Selection",
                                                                                    inline = T,
                                                                                    value = F,
                                                                                    status = "success",
                                                                                    fill = T),
                                                              numericInput(
                                                                "MinRange",
                                                                "Minimum Wavenumber",
                                                                value = 0,
                                                                min = NA,
                                                                max = NA,
                                                                step = NA,
                                                                width = NULL
                                                              ),
                                                              numericInput(
                                                                "MaxRange",
                                                                "Maximum Wavenumber",
                                                                value = 6000,
                                                                min = NA,
                                                                max = NA,
                                                                step = NA,
                                                                width = NULL
                                                              ))),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Sometimes peaks are undersireable. 
                                                                                     These options will replace peak regions with the mean of their edges. 
                                                                                     Specify the edge locations of the peaks minimum and maximum wavenumbers to use for flattening.
                                                                                     Defaults are set to flatten the CO2 region in infrared spectra."),
                                                              title = prettySwitch("co2_decision",
                                                                                   label = "Flatten Region",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              numericInput(
                                                                "MinFlat",
                                                                "Minimum Wavenumber",
                                                                value = 2200,
                                                                min = 1,
                                                                max = 6000,
                                                                step = 1
                                                              ),
                                                              numericInput(
                                                                "MaxFlat",
                                                                "Maximum Wavenumber",
                                                                value = 2400,
                                                                min = 1,
                                                                max = 6000,
                                                                step = 1
                                                              )))
                                                        
                                                    )
                                                  )
                                           ))),
                                  column(2,
                                         selectInput(inputId = "download_selection",
                                                     label = downloadButton("download_data",
                                                                            style = "background-color: rgb(0,0,0); color: rgb(255,255,255);"),
                                                     choices = c("Test Data",
                                                                 "Test Map",
                                                                 "Your Spectra",
                                                                 "Library Spectra",
                                                                 "Top Matches",
                                                                 "Thresholded Particles")) %>%
                                           popover(
                                             title = "Options for downloading spectra and metadata from the analysis.
                                          Test Data is a Raman HDPE spectrum in csv format. Test Map is an FTIR ENVI file of a CA particle.
                                          Your Spectra will download your data with whatever processing options are active. Library Spectra
                                          will download the current library selected. Top Matches downloads the top identifications in the
                                          active analysis. Thresholded Particles will download a version of your spectra using the active
                                          thresholds selected to infer where particles are in spectral maps, particle spectra are collapsed
                                          to their medians and locations to their centroids.",
                                             content = "Download Options", placement = "left"
                                           )
                                  )
                                ),
                                ## Plot ----
                                fluidRow(
                                  #verbatimTextOutput("event_test"),
                                  box(title = HTML(paste0("Cleaned Data")), 
                                      maximizable = T,
                                      width = 12,
                                      #background = "black",
                                      # label = uiOutput("correlation_head"),
                                      # h4(id = "placeholder1", "Upload some data to get started..."),
                                      # uiOutput("progress_bars"),
                                      fluidRow(
                                        # dataTableOutput('contents5'),
                                        div(style = "overflow-x: scroll",
                                            DT::dataTableOutput("contents5")
                                        # plotlyOutput("heatmap",inline = T),
                                        # plotlyOutput("MyPlotC", inline = T),
                                        # div(style = "overflow-x: scroll",
                                        # DT::dataTableOutput("eventmetadata")
                                       ))
                                      # dropdownMenu = boxDropdown(
                                      #   boxDropdownItem("Bad Processing or Library Spectra", id = "bad_spec", icon = icon("face-sad-tear"))
                                      # ),
                                       # sidebar = boxSidebar(
                                       #   id = "mycardsidebar",
                                       #   fluidRow(style = "padding:1rem; overflow-x: scroll",
                                       #            DT::dataTableOutput("event"))
                                       # )
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
                                         img(width = "100%", src = "db_diagram.JPG"),
                                         #HTML('<iframe width="560" height="315" src='https://dbdiagram.io/embed/5f3d9342cf48a141ff557dfe'> </iframe>'),
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
                                
                                
                                fluidRow(
                                  column(3),
                                  column(6,
                                         
                                         shiny::HTML("<br><br><center> <h1>How To Use</h1> </center><br>"),
                                         shiny::HTML("<h5>In order to assist your navigation through both the relational tables and query tool functions of this app, please refer to the video tutorial below.</h5>"),
                                         shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sqeLaJKyol8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                         
                                  ),
                                  column(3)
                                ),
                                
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
                                         
                                         #downloadButton(NOAA, label="Download")
                                         #tags$div(align = "center",
                                         #        tags$a("Sample Data",
                                         #              onclick = "window.open('https://drive.google.com/file/d/1YKyEDf4VbZaeSlV6yxgh0XVqstvel6LQ/view', '_blank')",
                                         #             class="btn btn-primary btn-lg")
                                         #     )
                                         
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



