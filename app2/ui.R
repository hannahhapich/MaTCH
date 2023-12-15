ui <- dashboardPage(dark = T, 
                    help = T, 
                    fullscreen = T,
  dashboardHeader(title = "MaTCH"),
  dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("About", 
             tabName = "About", 
             icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
               fluidRow(
                 column(3),
                 column(6,
                        
                        shiny::HTML("<br><br><center> <h1>Overview</h1> </center><br>"),
                        shiny::HTML("<h5>Trash Taxonomy website is a portal for relational tables that relate trash survey nomenclature. It includes 7 relational tables and a tool that can be used to query the relational tables with trash survey sheets.
                                  This tool was developed by collating and comparing categories and terms used from over 50 commonly used in trash survey sheets.</h5>"),
                        shiny::HTML("<h5>We are grateful for the funding for this project provided by the National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>")
                        
                 ),
                 column(3)
               )
               
               
               #end of about panel
      )
    )
  )
)