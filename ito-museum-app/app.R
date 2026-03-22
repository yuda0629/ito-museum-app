library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)

# =========================
# CSV読み込み（安全）
# =========================
data <- read_csv(
  "data/ito_sites_clean.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
data <- data %>%
  mutate(across(where(is.character), ~iconv(.x, "", "UTF-8")))

# =========================
# UI
# =========================
ui <- fluidPage(
  titlePanel("🏛 伊都国デジタル遺跡マップ（完成版）"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "遺跡タイプ",
                  choices = c("全て", unique(data$type))),
      
      selectInput("period", "時代",
                  choices = c("全て", unique(data$period)))
    ),
    
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # フィルタデータ
  filtered_data <- reactive({
    df <- data
    
    if (input$type != "全て") {
      df <- df %>% filter(type == input$type)
    }
    
    if (input$period != "全て") {
      df <- df %>% filter(period == input$period)
    }
    
    df
  })
  
  # 🎨 安全なカラーパレット（固定型に変更＝安定）
  pal <- colorFactor(
    palette = "Set1",
    domain = data$type
  )
  
  # 初期地図
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 130.20, lat = 33.56, zoom = 12)
  })
  
  # マーカー更新（安定版）
  observe({
    
    df <- filtered_data()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = df,
        lng = ~lng,
        lat = ~lat,
        radius = 7,
        color = ~pal(type),
        fillColor = ~pal(type),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "タイプ: ", type, "<br>",
          "時代: ", period, "<br>",
          desc
        )
      )
  })
  
  # 凡例（安定）
  observe({
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = data$type,
        title = "遺跡タイプ",
        opacity = 1
      )
  })
}

shinyApp(ui, server)