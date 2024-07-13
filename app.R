# getting libraries, data, and fcns
source("election-functions.R")

# customized function to add static text below sidebar
# source: https://stackoverflow.com/questions/52544228/r-shiny-display-static-text-outside-sidebar-panel
sidebarPanel_plustext <- function (..., below = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      below
  )
}

# define UI
ui <- navbarPage(
  # title
  p(
    style = "color: white;", # I bet this line could be removed!
    strong("2022 Midterm Elections in Minnesota")
  ),
  
  # customizing colors and creating CSS classes
  tags$head(
    HTML(
      "
    <style>
    .navbar {background-color: #003865;}
    .navbar-default .navbar-nav > li > a {color: white;}
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover { background-color: #78BE21; color: black;}
    .navbar-default .navbar-nav > li > a:hover {color: gray;}
    
    .rounded-box-solid {
          display: inline-block;
          border-radius: 10px;
          border: 2px solid #003865;
          padding: 8px;
    }
    
    .rounded-box-dashed {
          display: inline-block;
          border-radius: 10px;
          border: 2px dashed #003865;
          padding: 4px;
    }
    

    .custom-column p {
          max-width: 80%;
          justify-content: center;
          align-items: center;
    }
    
    </style>
    "
    )
  ),
  
  tabPanel("Overview",
           
           fluidRow(
             column(width = 7,
                    h3(strong("Welcome to the 2022 Minnesota Midterms app!")),
                    
                    p(strong("In November 2022,"),
                      "Minnesotans cast their ballots for hundreds of legislative and executive candidates. This was a consequential election, with the DFL (Democrat-Farmer-Labor) party retaining all statewide offices -- and claiming a majority in both chambers of the legislature for the first time in several years."),
                    
                    div(class = "rounded-box-solid",
                        
                        p("The offices on the ballot were:"),
                        
                        tags$li(strong("Governor")),
                        tags$li(strong("Secretary of State")),
                        tags$li(strong("State Auditor")),
                        tags$li(strong("Attorney General")),
                        tags$li(strong("U.S. Representative"), " (8, by district)"),
                        tags$li(strong("Minnesota State Senator"), " (67, by district)"),
                        tags$li(strong("Minnesota State Representative"), " (134, by district)"),
                        
                    ),
                    
                    br(),
                    
                    p("This app has four tabs: one for statewide elections, another for congressional elections, and two more for state legislative races. Statewide election results are reported by county; all other results are at the precinct level. Contact me at mcgow241@umn.edu with any suggestions for this app!")
             ),
             
             column(width = 5, class = "custom-column",
                    div(
                      img(src = "mn-logo.png", style = "max-width: 100%; height: auto;"),
                      style = "text-align: center;"
                    ),
                    
                    br(),
                    br(),
                    
                    div(class = "rounded-box-dashed",
                        
                        p(strong("About:"),
                          " This app was created by David McGowan, a graduate student in the Division of Biostatistics at the University of Minnesota. David is also a proud St. Olaf College alum, and he's still an Iowa farmer in spirit. David loves programming in R (most of the time), and he would love a job where he continues to do that!")
                    )
             )
           )
  ),
  
  tabPanel("Statewide",
           sidebarLayout(
             sidebarPanel_plustext(
               selectInput("statewide_office",
                           "Choose a statewide office:",
                           choices = unique(statewide_table$Office),
                           selected = "Governor"),
               
               below = "*(i) denotes incumbent"
             ),
             
             mainPanel(leafletOutput("statewide_map"),
                       
                       tableOutput("statewide_table")
             )
           )
  ),
  
  tabPanel("U.S. Congress",
           sidebarLayout(
             sidebarPanel_plustext(
               selectInput("congress_district",
                           "Choose a congressional district:",
                           choices = 1:8,
                           selected = 1),
               
               textInput("congress_city",
                         "Search for a specific city:",
                         placeholder = "Type a city name..."),
               
               textOutput("congress_matches"),
               
               below = "*(i) denotes incumbent"
             ),
             
             mainPanel(leafletOutput("congress_map"),
                       
                       tableOutput("congress_table")
             )
           )
  ),
  
  tabPanel("State Senate",
           sidebarLayout(
             sidebarPanel_plustext(
               selectInput("senate_district",
                           "Choose a Senate district:",
                           choices = 1:67,
                           selected = 1),
               
               radioButtons("senate_choice",
                            "What areas are you interested in?",
                            choices = c("All",
                                        "Twin Cities metro",
                                        "Non-metro"),
                            selected = "All"),
               
               textInput("senate_city",
                         "Search for a specific city:",
                         placeholder = "Type a city name..."),
               
               textOutput("senate_matches"),
               
               below = "*(i) denotes incumbent"
             ),
             
             mainPanel(leafletOutput("senate_map"),
                       
                       tableOutput("senate_table")
             )
           )
  ),
  
  tabPanel("State House",
           sidebarLayout(
             sidebarPanel_plustext(
               selectInput("house_district",
                           "Choose a House district:",
                           choices = alphanumeric(c(str_c(1:67, "A"),
                                                    str_c(1:67, "B"))),
                           selected = "1A"),
               
               radioButtons("house_choice",
                            "What areas are you interested in?",
                            choices = c("All",
                                        "Twin Cities metro",
                                        "Non-metro"),
                            selected = "All"),
               
               textInput("house_city",
                         "Search for a specific city:",
                         placeholder = "Type a city name..."),
               
               textOutput("house_matches"),
               
               below = "*(i) denotes incumbent"
               
             ),
             
             mainPanel(leafletOutput("house_map"),
                       
                       tableOutput("house_table")
             )
           )
  )
)

# define server
server <- function(input, output, session) {
  
  # change senate district choices based on radio buttons
  observe({
    if(input$senate_choice == "Twin Cities metro") {
      updateSelectInput(session, "senate_district",
                        choices = senate_metro,
                        selected = 31)
    } else if(input$senate_choice == "Non-metro") {
      updateSelectInput(session, "senate_district",
                        choices = senate_nonmetro,
                        selected = 1)
    } else {
      updateSelectInput(session, "senate_district",
                        choices = 1:67,
                        selected = 1)
    }
  })
  
  # change house district choices based on radio buttons
  observe({
    if(input$house_choice == "Twin Cities metro") {
      updateSelectInput(session, "house_district",
                        choices = alphanumeric(house_metro),
                        selected = "31A")
    } else if(input$house_choice == "Non-metro") {
      updateSelectInput(session, "house_district",
                        choices = alphanumeric(house_nonmetro),
                        selected = "1A")
    } else {
      updateSelectInput(session, "house_district",
                        choices = alphanumeric(c(str_c(1:67, "A"),
                                                 str_c(1:67, "B"))),
                        selected = "1A")
    }
  })
  
  # statewide
  output$statewide_table <- reactive(
    statewide_table.fcn(input$statewide_office)
  )
  
  output$statewide_map <- renderLeaflet(
    statewide_map.fcn(input$statewide_office)
  )
  
  # congress
  output$congress_table <- reactive(
    congress_table.fcn(as.integer(input$congress_district))
  )
  
  output$congress_map <- renderLeaflet(
    congress_map.fcn(as.integer(input$congress_district))
  )
  
  output$congress_matches <- renderText({
    congress_match.fcn(input$congress_city)
  })
  
  # senate
  output$senate_table <- reactive(
    senate_table.fcn(as.integer(input$senate_district))
  )
  
  output$senate_map <- renderLeaflet(
    senate_map.fcn(as.integer(input$senate_district))
  )
  
  output$senate_matches <- renderText({
    senate_match.fcn(input$senate_city)
  })
  
  # house
  output$house_table <- reactive(
    house_table.fcn(input$house_district)
  )
  
  output$house_map <- renderLeaflet(
    house_map.fcn(input$house_district)
  )
  
  output$house_matches <- renderText({
    house_match.fcn(input$house_city)
  })
}

# run the application
shinyApp(ui = ui, server = server)
