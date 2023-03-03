library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyquant)
library(fpp3)
library(plotly)
library(seasonal)
library(spsComps)
library(fresh)
library(DT)
library(shinyjs)
library(waiter)
Valid_ticks <- read.csv("Tickers.csv")


Senator <- read.csv("SenatorCleaned.csv")
Senator <- Senator %>%
  select(-X)

mytheme <- create_theme(
  adminlte_color(
    red = "maroon"
  )
)

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
      icon = icon("leaf"), tabName = "seasonality",
      badgeLabel = "New", badgeColor = "green"
    ),
    menuItem("Autocorrelation",
      icon = icon("signal"), tabName = "auto"
    ),
    menuItem("Decomposition",
      icon = icon("trash"), tabName = "decomp"
    ),
    menuItem("User Guide",
      icon = icon("user"), tabName = "user",
      badgeLabel = "In Dev", badgeColor = "green"
    ),
    menuItem("Github",
      icon = icon("github"),
      href = "https://github.com/AlexanderHolmes0"
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
  autoWaiter(id=c("series","season","ggseason","autoq","decompq"),
             fadeout = TRUE), 
             #html = img(src = "giphy.gif", align = "center", height = "200px", width = "200px")),
  use_theme(mytheme),
  tabItems(
    tabItem(
      tabName = "dashboard",
      h2("Stock Information"),
      fluidRow(
        box(title = textOutput("title"), background = "maroon", plotlyOutput("series", height = 490), width = 7),
        box(title = "Stock Quotes", DTOutput("stonk"), width = 5)
      ),
      fluidRow(box(title = "Senator Trades", DTOutput("sentable"), width = 12))
    ),
    tabItem(
      tabName = "seasonality",
      h2("Seasonality"),
      box(title = textOutput("seas"), background = "maroon", plotlyOutput("season")),
      box(title = textOutput("seeass"), background = "maroon", plotlyOutput("ggseason"))
    ),
    tabItem(
      tabName = "auto",
      h2("Autocorrelation"),
      box(title = textOutput("autoo"), background = "maroon", plotlyOutput("autoq"), width = 8),
      box(title = "Maximum Lag", sliderInput("lags", "Lag Max Value", value = 12, min = 1, max = 100), width = 4)
    ),
    tabItem(
      tabName = "decomp",
      h2("Decomposition"),
      box(title = textOutput("decompi"), background = "maroon", plotlyOutput("decompq"), width = 8)
    )
  )
)

# Put them together into a dashboardPage
ui <- shinydashboardPlus::dashboardPage(
  title = "Stonks with Math",
  skin = "red",
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


  senate <- reactive({
    if (any(input$asset %in% Senator$Ticker)) {
      senate <- Senator %>%
        filter(Ticker == input$asset) %>%
        mutate(Transaction.Date = yearmonth(mdy(Transaction.Date))) %>%
        select(-3)

      updateDateRangeInput(session,
        "dates",
        start = min(senate$Transaction.Date) - 2,
      )
      senate <- senate %>%
        filter(Type %in% input$type)

      senate
    } else {
      shinyCatch(position = "top-right", stop("No Stonk Trading Senators YET!"))
    }
  })


  stock <- reactive({
    if (any(input$asset %in% senate()$Ticker)) {
      tq_get(input$asset, get = "stock.prices", from = input$dates[1], to = input$dates[2] ,complete_cases = TRUE) %>% # add something with date ranges to capture the trades
        tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
        mutate(
          date = yearmonth(date),
          across(2:7, round, 3)
        ) %>%
        as_tsibble(index = date)
    } else {
      tq_get(input$asset, get = "stock.prices", complete_cases = TRUE) %>% # add something with date ranges to capture the trades
        tq_transmute(mutate_fun = to.monthly, indexAt = "lastof") %>%
        mutate(
          date = yearmonth(date),
          across(2:7, round, 3)
        ) %>%
        as_tsibble(index = date)
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
    if (!(is.null(stock())) & any(input$asset %in% Senator$Ticker)) {
      sen <- senate()
      sen$Transaction.Date <- format(as.Date(sen$Transaction.Date), "%Y-%m")
      sen %>%
        arrange(desc(Transaction.Date)) %>%
        select(-Comment)
    }
  })

  output$series <- renderPlotly({
    if (!(is.null(stock()))) {
      series <- autoplot(stock(), !!sym(input$column)) +
        labs(y = paste0(input$asset, " ", input$column))
    }
    if (any(input$asset %in% Senator$Ticker) & !is.null(stock()) & !is.null(input$type)) {
      cols <- c("Purchase" = "green", "Sale (Full)" = "red", "Sale (Partial)" = "orange", "Exchange" = "steelblue")
      series <- series +
        geom_vline(data = senate(), aes(xintercept = Transaction.Date, color = Type, text = Name)) +
        scale_color_manual(values = cols)
    }
    series
  })

  models <- reactive({
    if (input$model == "X11" & nrow(stock()) >= 36) {
      stock() %>%
        model(X_13ARIMA_SEATS(!!sym(input$column) ~ x11())) %>%
        components()
    } else if (input$model == "STL") {
      stock() %>%
        model(STL(!!sym(input$column))) %>%
        components()
    } else if (input$model == "SEATS") {
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
      shinyCatch(position = "top-right", stop("This stonk is too young! (Less than 36 months old)"))
    }
  })

  output$season <- renderPlotly({
    if (input$model == "STL") {
      autoplot(models(), season_year)
    } else {
      autoplot(models(), seasonal)
    }
  })

  output$ggseason <- renderPlotly({
    if (input$model == "STL") {
      gg_season(models(), season_year)
    } else {
      gg_season(models(), seasonal)
    }
  })

  output$autoq <- renderPlotly({
    if (nrow(stock()) > 1) {
      stock() %>%
        ACF(!!sym(input$column), lag_max = input$lags) %>%
        autoplot()
    } else {
      shinyCatch(position = "top-right", stop("Not enuff time mannn!"))
    }
  })

  output$decompq <- renderPlotly({
    autoplot(models())
  })
}


# Run the application
shinyApp(ui = ui, server = server)
