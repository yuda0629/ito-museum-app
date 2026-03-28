library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)

# ===== CSV読み込み（相対パス）=====
if (!file.exists("ito_sites_exhibition_fixed.csv")) {
  stop("CSVファイルが見つかりません")
}

data <- read_csv("ito_sites_exhibition_fixed.csv", show_col_types = FALSE)

# ===== UI =====
ui <- fluidPage(

  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { font-family: sans-serif; }
      .card {
        background: #fff;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.2);
      }
    "))
  ),

  titlePanel("伊都国デジタルミュージアム"),

  sidebarLayout(

    sidebarPanel(
      h4("🔎 フィルター"),

      selectInput("period", "時代",
                  choices = c("すべて", unique(data$period))),

      selectInput("type", "種類",
                  choices = c("すべて", unique(data$type))),

      checkboxInput("kids_mode", "やさしい解説モード", FALSE),

      hr(),

      h4("📖 ストーリー"),
      selectInput("story", "",
                  choices = c("伊都国とは", "弥生時代", "古墳時代")),

      uiOutput("story_text")
    ),

    mainPanel(
      leafletOutput("map", height = 500),
      br(),
      div(class="card",
          h4("📍 遺跡詳細"),
          uiOutput("detail")
      )
    )
  )
)

# ===== サーバー =====
server <- function(input, output, session) {

  filteredData <- reactive({
    df <- data
    if (input$period != "すべて") {
      df <- df %>% filter(period == input$period)
    }
    if (input$type != "すべて") {
      df <- df %>% filter(type == input$type)
    }
    df
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 130.2, lat = 33.55, zoom = 11)
  })

  observe({
    df <- filteredData()
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      addMarkers(
        ~lng, ~lat,
        label = ~name,
        layerId = ~name
      )
  })

  output$detail <- renderUI({
    req(input$map_marker_click)
    clicked <- input$map_marker_click$id
    row <- data %>% filter(name == clicked)
    if (nrow(row) == 0) return(NULL)

    desc <- if (input$kids_mode) {
      "むかしの人がくらしていた場所です"
    } else {
      row$desc
    }

    HTML(paste0(
      "<h3>", row$name, "</h3>",
      "<p><b>時代：</b>", row$period, "</p>",
      "<p><b>種類：</b>", row$type, "</p>",
      "<p>", desc, "</p>",
      "<hr>",
      "<small>出典：伊都国関連資料（※サンプル）</small>"
    ))
  })

  output$story_text <- renderUI({
    if (input$story == "伊都国とは") {
      HTML("<p>伊都国は弥生時代に栄えたクニで、中国の史書にも登場します。</p>")
    } else if (input$story == "弥生時代") {
      HTML("<p>農耕が広まり、集落が発展した時代です。</p>")
    } else {
      HTML("<p>権力者の墓である古墳が築かれた時代です。</p>")
    }
  })
}

shinyApp(ui, server)
