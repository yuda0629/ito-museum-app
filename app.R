library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)
library(RColorBrewer)

# ===== CSV読み込み =====
data <- read_csv("ito_sites_clean.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# ===== カテゴリ確認 =====
data$type <- as.factor(data$type)

# ===== 色パレット（←ここが修正ポイント🔥）=====
n <- length(unique(data$type))

pal <- colorFactor(
  palette = colorRampPalette(
    brewer.pal(min(9, max(3, n)), "Set1")
  )(n),
  domain = data$type
)

# ===== UI =====
ui <- fluidPage(
  titlePanel("伊都国遺跡マップ"),
  leafletOutput("map", height = "600px")
)

# ===== サーバー =====
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet(data) %>%
      addTiles() %>%
      
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        color = ~pal(type),
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.9,
        
        popup = ~HTML(paste0(
          "<b>", name, "</b><br>",
          "種類: ", type, "<br>",
          "時代: ", period, "<br>",
          desc
        ))
      )
  })
}

# ===== 実行 =====
shinyApp(ui, server)