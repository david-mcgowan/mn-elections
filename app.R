source("pre-processing.R")

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
    strong("Election Results in Minnesota (2012-2022)")
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
                    h3(strong("Welcome to the",
                              em("Election Results in Minnesota"),
                              "app!")),
                    
                    p("Minnesota has two histories that are both true: a legacy of strong performance by Democrats (or DFLers, for Democrat-Farmer-Labor), and yet a long closely divided electorate. A transformation has taken place since 2012, where rural Minnesotans are much more likely to vote Republican while the Twin Cities metro shifts ever toward the DFL. In 2022, the most recent election, Minnesotans cast their ballots for hundreds of legislative and executive candidates. This was a consequential election, with the DFL retaining all statewide offices -- and claiming a majority in both chambers of the legislature for the first time in several years."),
                    
                    div(class = "rounded-box-solid",
                        
                        p("This app shows results for each of the following races since 2012:"),
                        
                        tags$li(strong("President")),
                        tags$li(strong("U.S. Senate")),
                        tags$li(strong("Governor")),
                        tags$li(strong("Secretary of State")),
                        tags$li(strong("State Auditor")),
                        tags$li(strong("Attorney General")),
                        tags$li(strong("U.S. Representative"), " (8, by district)"),
                        tags$li(strong("Minnesota State Senator"), " (67, by district)"),
                        tags$li(strong("Minnesota State Representative"), " (134, by district)"),
                        
                    ),
                    
                    br(),
                    
                    p("This app has four tabs: one for statewide elections, another for congressional elections, and two more for state legislative races. Statewide election results can be viewed either by county or by precinct; all other results are at the precinct level. Contact me at dave.s.mcgowan@gmail.com with any suggestions for this app! The code and datasets for this app can be found on GitHub.")
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
                          " This app was created by David McGowan, a data scientist with the Federal Reserve Bank of Minneapolis (note: the Fed is not associated with this work). David is a proud alumnus of St. Olaf College (BA '22) with an MS in biostatistics, and he's still an Iowa farmer in spirit. David loves programming in R (when it's going well), and he also enjoys classical singing and 90s movies!")
                    )
             )
           )
  ),
  
  tabPanel("Statewide",
           sidebarLayout(
             sidebarPanel_plustext(
               selectInput("statewide_office",
                           "Choose a statewide office:",
                           choices = unique(statewide_totals$Office),
                           selected = "Governor"),
               
               selectInput("statewide_year",
                           "Choose a year:",
                           choices = c(2012, 2014, 2016, 2018, 2020, 2022),
                           selected = 2022),
               
               selectInput("statewide_size",
                           "Counties or precincts?",
                           choices = c("Counties", "Precincts"),
                           selected = "Counties"),
               
               actionButton("statewide_button", label = "Submit"),
               
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
               
               selectInput("congress_year",
                           "Choose a year:",
                           choices = c(2012, 2014, 2016, 2018, 2020, 2022),
                           selected = 2022),
               
               actionButton("congress_button", label = "Submit"),
               
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
               
               selectInput("senate_year",
                           "Choose a year:",
                           choices = c(2012, 2016, 2020, 2022),
                           selected = 2022),
               
               actionButton("senate_button", label = "Submit"),
               
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
               
               selectInput("house_year",
                           "Choose a year:",
                           choices = c(2012, 2014, 2016, 2018, 2020, 2022),
                           selected = 2022),
               
               actionButton("house_button", label = "Submit"),
               
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
  
  # statewide
  statewide_office <- reactiveVal("Governor")
  statewide_year <- reactiveVal(2022)
  statewide_size <- reactiveVal("Counties")
  
  observeEvent(input$statewide_button, {
    req(input$statewide_office)
    statewide_office(input$statewide_office)
  })
  
  observeEvent(input$statewide_button, {
    req(input$statewide_year)
    statewide_year(input$statewide_year)
  })
  
  observeEvent(input$statewide_button, {
    req(input$statewide_size)
    statewide_size(input$statewide_size)
  })
  
  output$statewide_table <- reactive({
    statewide_table.fcn(statewide_office(), as.numeric(statewide_year()))
  })
  
  output$statewide_map <- renderLeaflet(
    statewide_map.fcn(statewide_office(), statewide_year(), statewide_size())
  )
  
  observe({
    updateSelectInput(session, "statewide_year",
                      choices = unique(pull(filter(statewide_totals,
                                                   Office == input$statewide_office),
                                            Year)))
  })
  
  # congress
  congress_district <- reactiveVal(1)
  congress_year <- reactiveVal(2022)

  observeEvent(input$congress_button, {
    req(input$congress_district)
    congress_district(input$congress_district)
  })
  
  observeEvent(input$congress_button, {
    req(input$congress_year)
    congress_year(input$congress_year)
  })
  
  output$congress_table <- reactive(
    congressional_table.fcn(congress_district(), congress_year())
  )
  
  output$congress_map <- renderLeaflet(
    congressional_map.fcn(congress_district(), congress_year())
  )
  
  output$congress_matches <- renderText({
    match.fcn(input$congress_city, "Congress", input$congress_year)
  })
  
  # senate
  senate_district <- reactiveVal(1)
  senate_year <- reactiveVal(2022)
  
  observeEvent(input$senate_button, {
    req(input$senate_district)
    senate_district(input$senate_district)
  })
  
  observeEvent(input$senate_button, {
    req(input$senate_year)
    senate_year(input$senate_year)
  })
  
  output$senate_table <- reactive(
    mnsen_table.fcn(senate_district(), senate_year())
  )
  
  output$senate_map <- renderLeaflet(
    mn_senate_map.fcn(senate_district(), senate_year())
  )
  
  output$senate_matches <- renderText({
    match.fcn(input$senate_city, "State Senate", input$senate_year)
  })
  
  # house
  house_district <- reactiveVal("1A")
  house_year <- reactiveVal(2022)
  
  observeEvent(input$house_button, {
    req(input$house_district)
    house_district(input$house_district)
  })
  
  observeEvent(input$house_button, {
    req(input$house_year)
    house_year(input$house_year)
  })
  
  output$house_table <- reactive(
    mnhouse_table.fcn(house_district(), house_year())
  )
  
  output$house_map <- renderLeaflet(
    mn_house_map.fcn(house_district(), house_year())
  )
  
  output$house_matches <- renderText({
    match.fcn(input$house_city, "State House", input$house_year)
  })
}

shinyApp(ui = ui, server = server)
