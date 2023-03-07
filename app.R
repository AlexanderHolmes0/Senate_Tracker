library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyquant)
library(fpp3)
library(seasonal)
library(plotly)
library(spsComps)
library(DT)
library(fresh)
library(waiter)

Valid_ticks <- read.csv("Tickers.csv")


Senator <- read.csv("SenatorCleaned.csv")
Senator <- Senator %>%
  select(-X)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0066B6",
    
  )
)

interp <- tq_get("AA", get = "stock.prices" ,from="1980-01-01",complete_cases = TRUE) %>% # add something with date ranges to capture the trades
  tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
  mutate(
    date = yearmonth(date),
    across(2:7, \(x) round(x, 3))
  ) %>%
  as_tsibble(index = date)

interp_senate <- Senator %>% 
  filter(Ticker == 'AA') %>% 
  mutate(Transaction.Date = yearmonth(mdy(Transaction.Date)))

## ui.R ##
sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "shortcut icon", href = "dollar.ico"),
            HTML('<!-- Primary Meta Tags -->
              <title>Senate Stock Tracker</title>
              <meta name="title" content="Senate Tracker">
              <meta name="description" content="Dive into the stock trades of elected officials! Can you find anything sus?">
              
              <!-- Open Graph / Facebook -->
              <meta property="og:type" content="website">
              <meta property="og:url" content="https://aholmes23.shinyapps.io/Senate_Tracker/">
              <meta property="og:title" content="Senate Stock Tracker">
              <meta property="og:description" content="Dive into the stock trades of elected officials! Can you find anything sus?">
              <meta property="og:image" content="https://i.imgur.com/Uq04iXx.jpeg">
              
              <!-- Twitter -->
              <meta property="twitter:card" content="summary_large_image">
              <meta property="twitter:url" content="https://aholmes23.shinyapps.io/Senate_Tracker/">
              <meta property="twitter:title" content="Senate Stock Tracker">
              <meta property="twitter:description" content="Dive into the stock trades of our elected officials! Can you find anything sus?">
              <meta property="twitter:image" content="https://i.imgur.com/Uq04iXx.jpeg">')),
  minified = FALSE,
  sidebarMenu(
    menuItem("Home", tabName = "dashboard", icon = icon("home")),
    menuItem("Seasonality",
      icon = icon("leaf"), tabName = "seasonality"
    ),
    menuItem("Autocorrelation",
      icon = icon("signal"), tabName = "auto"
    ),
    menuItem("Decomposition",
      icon = icon("trash"), tabName = "decomp"
    ),
    menuItem("User Guide",
      icon = icon("user"), tabName = "user",
      badgeLabel = "README", badgeColor = "green"
    ),
    menuItem("Example Interpretations",
             icon = icon("pen"), tabName = "interpret",
             badgeLabel = "In Dev", badgeColor = "orange"
    ),
    menuItem("Github",
      icon = icon("github"),
      href = "https://github.com/AlexanderHolmes0/Senate_Tracker"
    ),
    selectizeInput("asset",
      "Stock Name:",
      choices = Valid_ticks$Symbol,
      options = list(maxOptions = 8000),
      selected = "AA"
    ),
    selectizeInput("column",
      "Plotted Variable:",
      selected = "open",
      choices = c("open", "high", "low", "close", "volume", "adjusted")
    ),
    selectizeInput("type",
      "Transaction Type",
      selected = unique(Senator$Type),
      choices = unique(Senator$Type),
      multiple = TRUE
    ),
    dateRangeInput("dates",
      label = "Date range",
      start = "2019-01-01", end = Sys.Date()
    ),
    selectizeInput("model",
      "Model Type",
      selected = "X11",
      choices = c("X11", "SEATS", "STL", "Classic - Multi", "Classic - Add")
    ),
    img(src = "giphy.gif", align = "center", height = "230px", width = "230px")
  )
)

body <- dashboardBody(
  autoWaiter(id=c("series","season","ggseason","autoq","decompq",
                  "interp_series","interp_season","interp_ggseason",
                  "interp_autoq","interp_decompq"),
             fadeout = TRUE), 
             #html = img(src = "giphy.gif", align = "center", height = "200px", width = "200px")),
  use_theme(mytheme),
  tabItems(
    tabItem(
      tabName = "dashboard",
      h2("Stock Information"),
      fluidRow(
        box(title = textOutput("title"), background = "maroon", plotlyOutput("series", height = 490), width = 7),
        setShadow(id = "series"),
        box(title = "Stock Quotes", DTOutput("stonk"), width = 5),
        setShadow(id = "stonk")
      ),
      fluidRow(box(title = "Senator Trades", DTOutput("sentable"), width = 12)),
      setShadow(id = "sentable")
    ),
    tabItem(
      tabName = "seasonality",
      h2("Seasonality"),
      box(title = textOutput("seas"), background = "maroon", plotlyOutput("season")),
      setShadow(id = "season"),
      box(title = textOutput("seeass"), background = "maroon", plotlyOutput("ggseason")),
      setShadow(id = "ggseason")
    ),
    tabItem(
      tabName = "auto",
      h2("Autocorrelation"),
      box(title = textOutput("autoo"), background = "maroon", plotlyOutput("autoq"), width = 8),
      setShadow(id = "autoq"),
      box(title = "Maximum Lag", sliderInput("lags", "Lag Max Value", value = 12, min = 1, max = 100,
                                             animate = animationOptions(interval = 1500, loop = TRUE)), width = 4)
    ),
    tabItem(
      tabName = "decomp",
      h2("Decomposition"),
      box(title = textOutput("decompi"), background = "maroon", plotlyOutput("decompq"), width = 8),
      setShadow(id = "decompq")
    ),
    tabItem(
      tabName = "user",
      h2(id='userss', "WELCOME: Stonks with Math"),
         animateUI('userss',animation = 'float'),
      box(title = "User Guide" ,verbatimTextOutput("guide"), hr(),width = 10),
      setShadow(id = "guide")
    ),
    tabItem(
      tabName = "interpret",
      h2(id='tittle',"EXAMPLE: Interpretation of Plots (Sry Plots Locked)"),
      animateUI('tittle',animation = 'float'),
      fluidRow(box(id='box1',title = "Series: Alcoa Corporation Common Stock" ,plotlyOutput("interp_series"),verbatimTextOutput('words_series'), background = "maroon"),
               box(id='box2',title = "Seasonality: Alcoa Corporation Common Stock" ,plotlyOutput("interp_season"),verbatimTextOutput('words_seasonal'),background = "maroon")),
      fluidRow(box(id='box3',title = "Seasonaltiy: Alcoa Corporation Common Stock" ,plotlyOutput("interp_ggseason"),verbatimTextOutput('words_ggseason'),background = "maroon"),
               box(id='box4',title = "Autocorrelation: Alcoa Corporation Common Stock" ,plotlyOutput("interp_autoq"),verbatimTextOutput('words_autoq'),background = "maroon"),
               box(id='box5',title = "Decomposition: Alcoa Corporation Common Stock" ,plotlyOutput("interp_decompq"),verbatimTextOutput('words_decompq'),background = "maroon")),
      setShadow(id ="box1"),
      setShadow(id ="box2"),
      setShadow(id ="box3"),
      setShadow(id ="box4"),
      setShadow(id ="box5")

    )
  )
)

# Put them together into a dashboardPage
ui <- shinydashboardPlus::dashboardPage(
  title = "Stonks with Math",
  dashboardHeader(title = "Stonks with Math"),
  sidebar,
  body,
  controlbar = dashboardControlbar(collapsed = TRUE, skinSelector())
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$title <- renderText({
    paste0("Stock Series: ", Valid_ticks[which(Valid_ticks$Symbol == input$asset), 2])
  })
  output$seas <- renderText({
    paste0("Seasonality of: ", Valid_ticks[which(Valid_ticks$Symbol == input$asset), 2])
  })
  output$seeass <- renderText({
    paste0("Seasonality of: ", Valid_ticks[which(Valid_ticks$Symbol == input$asset), 2])
  })
  output$autoo <- renderText({
    paste0("Autocorrelation of: ", Valid_ticks[which(Valid_ticks$Symbol == input$asset), 2])
  })
  output$decompi <- renderText({
    paste0("Decomposition of: ", Valid_ticks[which(Valid_ticks$Symbol == input$asset), 2])
  })
  output$guide <- renderText({
     paste("This guide is designed to bring YOU up to speed on what this app is all about.",
           "First, I would like to thank you for your interest in this project :)",
           "",
           "To get started exploring stock trades and when politicians bought/sold them, click on the side bar menu and find 'Home' (House Icon)",
           "",
           "From this page you can explore any of the 8,000 available stocks to choose from but not all will have senate trades.",
           "The table to the right of the time series plot are monthly stock quotes for your selected stock. This updates daily.",
           "The table below the plot features ANY trades that senators have made and discolsed. (A pop-up will inform if no senators have traded the selected stonk)",
           "The sidebar also houses a variable selection. Pick from any to see it within the plot.", 
           "",
           "Red/Gren/Orange lines appear on the plot indicating the 'Type' of transaction the senator did and when they did.",
           "Let's keep it goin!",
           "So what about maths with stonks.. That's why I'm here!",
           "Alright alright, I was getting to it",
           "",
           "",
           "Click on the 'Seasonality' (Leaf) tab to see the seasonality of the selected stock. The plot next to it will be the yearly breakdown by color of the seasonality.",
           "This makes finding seasonal trends easier.",
           "Another nifty feature is that YOU can decide how this decomposition is run. More on this lil later.",
           "'Decomp' in this case is us trying to make sense of the past by taking into account certain features of the series (Trend, Seasonality, Random)",
           "",
           "As we continue down the sidebar, we arrive at this thing called 'Autocorrelation' (Bar Graph) tab ",
           "...",
           "What is that you ask?",
           "Its correlation with lags and YOU get to choose how much lag YOU want (plus animations) woo hoo!",
           "Lag is just how far we are pushing earlier observations down and seeing how they correlate with those values.",
           "",
           "Table example: | Month | SomeNum | Lagged |",
           "               |-------|---------|--------|",
           "               |   1   |    2    |   NA   | |" ,
           "               |-------|---------|--------| |",
           "               |   2   |    3    |   2    | v",
           "",
           "See how the first month got pushed down to the second month?",
           "That's Autocorrelation! 'Correlation with lags'",
           "",
           "Next Menu Item!",
           "Decomposition (Trash Can) tab is all about breaking down a series into a multitude of different parts.",
           "There are 5 options to choose when performing decomposition on a series.",
           " Classical contains both additive and multiplicative types. Seasonality does not change which is usually not a good thing",
           "",
           "Additive is concerned about adding up the different components Trend + Seasonality + Random = Y",
           "  --Use Additive when the trend is mostly going one direction and seasonal differences are minimal",
           "  --Expressed in 'things' above the trend i.e. 'We are $400 above the trend'",
           "",
           "Multiplicative is concerned about multipling the different componenets Trend * Seasonality * Random = Y",
           "  --Use Multiplication when the trend is parabolic or the seasonal variation is getting 'cone' shaped.",
           "  --Expressed in percentages above the trend (middle is 0%)",
           "",
           " STL - Seasonal and Trend decomposition using Loess, this uses additive but multiplicate can be obtained through transformations.",
           "  --ALso uses locally fitted regression models to fit the line and bring out the true features of the series as well as being robust to outliers.",
           "",
           "Seats & X11 methods",
           "X11 was developed by the US Census Bureau for better forecasting and evaluation of census data. It only allows for monthyl and quarterly data.",
           "Classical decomp but on steroids! Seasonality allowed to change. Many extra steps involved we won't dive into here but know its awesome.",
           "",
           "SEATS (Seasonal Extraction in ARIMA Time Series) was developed by the Bank of Spain. Seasonality is also allowed to change.",
           "It's a pretty great method as well.",
           "",
           "Play around and test all different stuff! This is YOUR playground for discovering stock patterns or politician 'shadiness'",
           "",
           "",
           "",
           sep="\n")
    
  })

  
  
  output$interp_series <- renderPlotly({
    
       series <- autoplot(interp, adjusted) +
        labs(y = paste0("Alcoa Common Stock: Adjusted Price"))
      
      cols <- c("Purchase" = "green", "Sale (Full)" = "red", "Sale (Partial)" = "orange", "Exchange" = "steelblue")
      
      series <- series +
        geom_vline(data = interp_senate, aes(xintercept = Transaction.Date, color = Type, text = Name)) +
        scale_color_manual(values = cols)
      series
    
  })
  output$words_series <- renderText({
    paste("Interpretation: This is a non-monotonic trend series that has great direction changes.",
          "Seasonality is also non-monotonicly changing with a increase and decrease",
          "throughout the years in seasonal variance.",
          "",
          sep="\n")
  })
  output$interp_decompq <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
      autoplot()
    
  })
  output$words_decompq <- renderText({
    paste("",
          "",
          sep="\n")
  })
  output$interp_autoq <- renderPlotly({
        interp %>%
          ACF(adjusted, lag_max = 36) %>%
          autoplot()
  })
  output$words_autoq <- renderText({
    paste("",
          "",
          sep="\n")
  })
  output$interp_season <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
        autoplot(season_year)
  })
  output$words_seasonal <- renderText({
    paste("",
          "",
          "",
          "",
          sep="\n")
  })
  output$interp_ggseason <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
      gg_season(season_year)
  })
  output$words_ggseason <- renderText({
    paste("",
          "",
          sep="\n")
  })
  
  
  senate <- reactive({
    if (any(input$asset %in% Senator$Ticker) & !is.null(input$type)) {
      senate <- Senator %>%
        filter(Ticker == input$asset) %>%
        mutate(Transaction.Date = yearmonth(mdy(Transaction.Date))) %>%
        select(-3)

      updateDateRangeInput(session,
        "dates",
        start = min(senate$Transaction.Date) - 6,
      )
    
      senate <- senate %>%
        filter(Type %in% input$type)
      
      senate
    } else {
      shinyCatch(message("No Stonk Trading Senators YET!"), position = "top-right")
      NULL
    }
  })


  stock <- reactive({
    if (any(input$asset %in% senate()$Ticker) & input$asset != "" ) {
      tq_get(input$asset, get = "stock.prices" , from=input$dates[1],complete_cases = TRUE) %>% # add something with date ranges to capture the trades
        tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
        mutate(
          date = yearmonth(date),
          across(2:7, \(x) round(x, 3))
        ) %>%
        as_tsibble(index = date)
    } else if(input$asset != ""){
      tq_get(input$asset, get = "stock.prices", complete_cases = TRUE) %>% # add something with date ranges to capture the trades
        tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
        mutate(
          date = yearmonth(date),
          across(2:7, \(x) round(x, 3))
        ) %>%
        as_tsibble(index = date)
    }else{
      NULL
      
    }
  })


  output$stonk <- renderDataTable({
    if (!(is.null(stock()))) {
      stock <- stock()
      stock$date <- format(as.Date(stock()$date), "%Y-%m")
      stock %>%
        arrange(desc(date))
    }
  })

  output$sentable <- renderDataTable({
    if (!(is.null(stock())) & !is.null(senate())) {
      sen <- senate()
      sen$Transaction.Date <- format(as.Date(sen$Transaction.Date), "%Y-%m")
      sen %>%
        arrange(desc(Transaction.Date)) %>%
        select(-Comment)
    }
  })

  output$series <- renderPlotly({
    if (!(is.null(stock())) & ( is.null(senate()))){  # | nrow(senate()) == 0 )) {
      series <- autoplot(stock(), !!sym(input$column)) +
        labs(y = paste0(input$asset, " ", input$column))
      series
    } else if ( !is.null(senate()) & !is.null(stock())) {
      series <- autoplot(stock(), !!sym(input$column)) +
        labs(y = paste0(input$asset, " ", input$column))
      cols <- c("Purchase" = "green", "Sale (Full)" = "red", "Sale (Partial)" = "orange", "Exchange" = "steelblue")
      
      series <- series +
        geom_vline(data = senate(), aes(xintercept = Transaction.Date, color = Type, text = Name)) +
        scale_color_manual(values = cols)
      series
    }
    
  })

  models <- reactive({
    if(!is.null(stock())){
    if (input$model == "X11" & nrow(stock()) >= 36) {
      stock() %>%
        model(X_13ARIMA_SEATS(!!sym(input$column) ~ x11())) %>%
        components()
    } else if (input$model == "STL" & nrow(stock()) >= 36) {
      stock() %>%
        model(STL(!!sym(input$column),robust=T)) %>%
        components()
    } else if (input$model == "SEATS" & nrow(stock()) >= 36) {
      stock() %>%
        model(X_13ARIMA_SEATS(!!sym(input$column) ~ seats())) %>%
        components()
    } else if (input$model == "Classic - Multi") {
      stock() %>%
        model(classical_decomposition(!!sym(input$column), type = "multiplicative")) %>%
        components()
    } else if (input$model == "Classic - Add") {
      stock() %>%
        model(classical_decomposition(!!sym(input$column), type = "additive")) %>%
        components()
    } else {
      shinyCatch(position = "top-right", message("This stonk is too young! (Less than 36 months old)"))

    }}else { NULL}
  })

  output$season <- renderPlotly({
    if(!is.null(models())){
    if (input$model == "STL") {
      autoplot(models(), season_year)
    } else{
      autoplot(models(), seasonal)
    }}
    
  })

  output$ggseason <- renderPlotly({
    if(!is.null(models())){
      if (input$model == "STL") {
        gg_season(models(), season_year)
      } else{
        gg_season(models(), seasonal)
      }
    }
  })

  output$autoq <- renderPlotly({
    if(!is.null(models())){
    if (nrow(stock()) > 1) {
      stock() %>%
        ACF(!!sym(input$column), lag_max = input$lags) %>%
        autoplot()
    } else {
      shinyCatch(position = "top-right", message("Not enuff time mannn!"))
    }}
  })

  output$decompq <- renderPlotly({
    if( !is.null(models())){
      autoplot(models())
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

# library(tidyquant)
# #install.packages("tidyquant")
# library(tidyverse)
# library(fpp3)
# stonk <- tq_get("AA", get = "stock.prices" ,complete_cases = TRUE) %>% # add something with date ranges to capture the trades
#   tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
#   mutate(
#     date = yearmonth(date),
#     across(2:7, \(x) round(x, 3))
#   ) %>%
#   as_tsibble(index = date)
# 
# try <- stonk %>%
#   model(X_13ARIMA_SEATS(adjusted ~ seats())) %>%
#   components()
# print(try)
# autoplot(try)
