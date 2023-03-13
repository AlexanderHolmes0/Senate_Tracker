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

interp <- tq_get("AA", get = "stock.prices" ,from="2000-01-01",to='2023-01-01',complete_cases = TRUE) %>% # add something with date ranges to capture the trades
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
      badgeLabel = "README", badgeColor = "light-blue"
    ),
    menuItem("Interpretation Example",
             icon = icon("pen"), tabName = "interpret",
             badgeLabel = "NEW", badgeColor = "green"
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
      label = "Date Range",
      start = "2019-01-01", end = Sys.Date()
    ),
    selectizeInput("model",
      "Model Type",
      selected = "X11",
      choices = c("X11", "STL", "Classic - Multi", "Classic - Add")#add seats back later once fixed
    ),
    img(src = "giphy.gif", align = "center", height = "230px", width = "230px")
  )
)

body <- dashboardBody(
  autoWaiter(color = "#0066B6" ,id=c("series","season","ggseason","autoq","autolag","decompq",
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
        box(title = textOutput("title"), background = "light-blue", plotlyOutput("series", height =490), width = 7),
        setShadow(id = "series"),
        tabBox(tabPanel("Stonk Quotes", DTOutput("stonk"), icon=icon("table")),
               tabPanel("Seasonality", plotlyOutput("season1"), icon = icon("leaf")),
               tabPanel("Autocorrelation", plotlyOutput("autoq1"), icon = icon("signal")),
               tabPanel("Decomposition", plotlyOutput("decompq1"), icon=icon("trash")),width = 5),
        setShadow(id = "stonk"),
        setShadow(id = "season1"),
        setShadow(id = "autoq1"),
        setShadow(id = "decompq1")),
      fluidRow(box(title = "Senator Trades", DTOutput("sentable"), width = 12)),
      setShadow(id = "sentable")
    ),
    tabItem(
      tabName = "seasonality",
      h2("Seasonality"),
      box(title = textOutput("seas"), background = "light-blue",plotlyOutput("season")),
      setShadow(id = "season"),
      box(title = textOutput("seeass"), background = "light-blue", plotlyOutput("ggseason")),
      setShadow(id = "ggseason")
    ),
    tabItem(
      tabName = "auto",
      h2("Autocorrelation"),
      box(title = textOutput("autoo"), background = "light-blue",plotlyOutput("autoq"),hr(), plotlyOutput("autolag") ,width = 8),
      setShadow(id = "autoq"),
      setShadow(id = "autolag"),
      box(title = "Maximum Lag", sliderInput("lags", "Lag Max Value", value = 12, min = 1, max = 100,
                                             animate = animationOptions(interval = 1500, loop = TRUE)), width = 4)
    ),
    tabItem(
      tabName = "decomp",
      h2("Decomposition"),
      box(title = textOutput("decompi"), background = "light-blue", plotlyOutput("decompq"), width = 8),
      setShadow(id = "decompq")
    ),
    tabItem(
      tabName = "user",
      fluidPage(
      h2(id='userss', "WELCOME: Stonks with Math"),
      htmltools::tags$iframe(src = "Guide.html", width = '100%',height=4100,  style = "border:none;")),
         animateUI('userss',animation = 'float')
    )
    
  ,
    
    tabItem(
      tabName = "interpret",
      h2(id='tittle',"EXAMPLE: Interpretation of Plots (Sry Plots Locked)"),
      animateUI('tittle',animation = 'float'),
      fluidRow(box(id='box1',title = "Series: Alcoa Corporation Common Stock" ,plotlyOutput("interp_series"),hr(), box(title = "Interpretation",textOutput('words_series'),background = "blue", width=12), background = "light-blue"),
               box(id='box2',title = "Seasonality: Alcoa Corporation Common Stock" ,plotlyOutput("interp_season"),hr(), box(title = "Interpretation",textOutput('words_seasonal'),background = "blue", width=12),background = "light-blue")),
      fluidRow(box(id='box3',title = "Seasonaltiy: Alcoa Corporation Common Stock" ,plotlyOutput("interp_ggseason"),hr(), box(title = "Interpretation",textOutput('words_ggseason'),background = "blue", width=12),background = "light-blue"),
               box(id='box4',title = "Autocorrelation: Alcoa Corporation Common Stock" ,plotlyOutput("interp_autoq"),hr(), box(title = "Interpretation",textOutput('words_autoq'),background = "blue", width=12),background = "light-blue"),
               box(id='box5',title = "Decomposition: Alcoa Corporation Common Stock" ,plotlyOutput("interp_decompq"),hr(), box(title = "Interpretation",textOutput('words_decompq'), width=12, background = "blue"),background = "light-blue")),
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
    paste("This series exhibits some irregular trend and seasonal behaviors.",
          "It also exhibits a non-monotonic seasonal variance increase with drastic changes throughout the years.",
          "Pretty huge drop during the 2008-2009 financial crisis.",
          "In recent history, there has been a huge increase in Aloca stock price due to the Russia-Ukraine war.",
          "The different colors of vertical lines indicate where a senator bought or sold Alcoa stock."
          )
  })
  output$interp_decompq <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
      autoplot()
    
  })
  output$words_decompq <- renderText({
    paste("This plot displays all the above in one plot for easy viewing.",
          "Contains one additional interesting plot (remainder).",
          "May 2008 and March 2022 both show up as pretty big outliers",
          "the selected model can not account for with trend and seasonal components.",
          "Trend is also seen for the first time here which is just a moving average of the series.",
          "Alcoa stock had a pretty wavy trend with some major drops aligning with the highlighted dates above. 
           The selected decomposition method has a robust method of dealing with outliers so that they do not impact the trend very much.")
  })
  output$interp_autoq <- renderPlotly({
        interp %>%
          ACF(adjusted, lag_max = 36) %>%
          autoplot()
  })
  output$words_autoq <- renderText({
    paste("Since trend is pretty important to this series, we see a downward slide.",
          "At 24 months, we see a reversal in the slide to show more and more positive correlation values.",
          "This makes sense because the values are somewhat closer together when lagged beyond 24 months.",
          "The graphic depicts the correlation of the series with the lagged series for certain months.",
          "*'ACF' is the correlation factor with the lagged series.*"
          )
  })
  output$interp_season <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
        autoplot(season_year)
  })
  output$words_seasonal <- renderText({
    paste("Due to how crazy this plot is, there is a consistent drop in seasonal variance.",
          "Seasonal variance during September was at all time lows during the 2014-2015 era.",
          "Seasonality began increasing after that period.",
          "There's a cone like shape for the seasonal variance in this plot.")
  })
  output$interp_ggseason <- renderPlotly({
    interp %>%
      model(STL(adjusted,robust=T)) %>%
      components() %>% 
      gg_season(season_year)
  })
  output$words_ggseason <- renderText({
    paste("This plot identifies the massive changes in seasonal variance by the year.",
          "The cyclical nature of the aluminum industry is exhibited well here.",
          "September in recent years has become less of a month of decline for Alcoa common stock.",
          "Seasonality overall has become less variant in nature and stays relatively close to the trend.")
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
    if (any(input$type %in% senate$Type)){
      senate <- senate %>%
        filter(Type %in% input$type)
     senate 
    }else {
      shinyCatch(message("No Transaction Types like that YET!"), position = "top-right")
      NULL
    }
      
    } else {
      shinyCatch(message("No Stonk Trading Senators YET!"), position = "top-right")
      NULL
    }
  })


  stock <- reactive({
    if (any(input$asset %in% senate()$Ticker) & input$asset != "" ) {
      tq_get(input$asset, get = "stock.prices" , from=input$dates[1], to=input$dates[2], complete_cases = TRUE) %>% # add something with date ranges to capture the trades
        tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
        mutate(
          date = yearmonth(date),
          across(2:7, \(x) round(x, 3))
        ) %>%
        as_tsibble(index = date)
    } else if(input$asset != ""){
      tq_get(input$asset, get = "stock.prices",from=input$dates[1], to=input$dates[2], complete_cases = TRUE) %>% # add something with date ranges to capture the trades
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
    if (input$model == "X11" & nrow(stock()) >= 60) {
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
      shinyCatch(position = "top-right", message("This stonk is too young! (Move the date range further out to >=60 weeks)"))

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
  
  output$season1 <- renderPlotly({
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
      shinyCatch(position = "top-right", message("Not enuff time!"))
    }}
  })
  
  output$autoq1 <- renderPlotly({
    if(!is.null(models())){
      if (nrow(stock()) > 1) {
        stock() %>%
          ACF(!!sym(input$column)) %>%
          autoplot()
      } else {
        shinyCatch(position = "top-right", message("Not enuff time!"))
      }}
  })
  
  output$autolag <- renderPlotly({
    if(!is.null(models())){
      if (nrow(stock()) > 1) {
        lagged <- stock() %>% 
          select(date,!!sym(input$column))
        
        
        lagged %>%
          autoplot()+
          geom_line(aes(x=date,y=lag(!!sym(input$column),input$lags)),color="#0066B6")+
          ggtitle("Lagged Series")
        
      } else {
        shinyCatch(position = "top-right", message("Not enuff time!"))
      }}
  })
  
  output$decompq <- renderPlotly({
    if( !is.null(models())){
      autoplot(models())
    }
  })
  
  output$decompq1 <- renderPlotly({
    if( !is.null(models())){
      autoplot(models())
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)


