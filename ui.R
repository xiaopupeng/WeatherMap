library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("WeatherMap", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 800, height = "auto",

        # h2("Equifax Weather Forecast"),
        # mainPanel(
        #   img(src='rain.png', align = "right")
        #   ### the rest of your code
        # ),
        fluidRow(
          column(6, h2("Weather Map")),
          column(3,
                 HTML("<div style=font-size:500%>"),
                 textOutput("test1"), 
                 HTML("</div>"),
                 offset = 6
          ),
         
          column(3,
                 HTML("<div style='height: 100px;'>"),
                 imageOutput("weatherIcon"),
                 HTML("</div>")
                 )
          
        ),
        
        fluidRow(
          column(3,
                 selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
          ),
          column(3,
                 conditionalPanel("input.states",
                                  selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                 )
          ),
          column(3,
                 conditionalPanel("input.states",
                                  selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                 )
          )
        ),
        
        tabsetPanel(
          tabPanel("DailyPlot", plotOutput("Dailyplot")), 
          tabPanel("HourPlot", plotOutput("Hourplot")), 
          tabPanel("MinPlot", plotOutput("MinPlot"))
        ),
        
        h2("Check History tmp"),
        
        fluidRow(
          column(6,
                 dateInput('date',
                           label = 'Date input: yyyy-mm-dd',
                           value = Sys.Date()
                 )
                 )
        ),
        
        tabsetPanel(
          tabPanel("hisHourPlot", plotOutput("hisHourplot"))
        )
        
        # tabsetPanel(
        #   tabPanel("MinPlot", plotOutput("Minplot")), 
        #   tabPanel("MinSummary", verbatimTextOutput("Minsummary")), 
        #   tabPanel("MinTable", tableOutput("Mintable"))
        # )
        

        # selectInput("color", "Color", vars),
        # selectInput("size", "Size", vars, selected = "adultpop"),
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #   # Only prompt for threshold when coloring or sizing by superzip
        #   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),
        # 
        # 
        # plotOutput("histCentile", height = 200),
        # plotOutput("scatterCollegeIncome", height = 250),
        # h3("hourly Equifax Weather Forecast"),
        # plotOutput("plotwether", height = 600)
        # h3(" daily Weather Forecast"),
        # plotOutput("plotwether", height = 600)
        # h3("Daily Equifax Weather Forecast"),
        # plotOutput("plotwether", height = 600)
        
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  # tabPanel("Data explorer",
  #   # fluidRow(
  #   #   column(3,
  #   #     selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=FALSE)
  #   #   ),
  #   #   column(3,
  #   #     conditionalPanel("input.states",
  #   #       selectInput("cities", "Cities", c("All cities"=""), multiple=FALSE)
  #   #     )
  #   #   ),
  #   #   column(3,
  #   #     conditionalPanel("input.states",
  #   #       selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=FALSE)
  #   #     )
  #   #   )
  #   # ),
  #   fluidRow(
  #     column(1,
  #       numericInput("minScore", "Min score", min=0, max=100, value=0)
  #     ),
  #     column(1,
  #       numericInput("maxScore", "Max score", min=0, max=100, value=100)
  #     )
  #   ),
  #   hr(),
  #   DT::dataTableOutput("ziptable")
  # ),

  conditionalPanel("false", icon("crosshair"))
)
