library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)

# ===== CSV読み込み =====
if (!file.exists("ito_sites_exhibition_fixed.csv")) {
  stop("CSVファイルが見つかりません: ito_sites_exhibition_fixed.csv")
}
data <- read_csv("ito_sites_exhibition_fixed.csv", show_col_types = FALSE)

# ===== UI =====
ui <- fluidPage(
  
  titlePanel("伊都国における遺跡分布と機能分析（デジタル展示）"),
  
  fluidRow(
    column(4,
           selectInput("filter_type", "遺跡タイプ",
                       choices = c("すべて", unique(data$type))),
           selectInput("filter_period", "時代",
                       choices = c("すべて", unique(data$period))),
           
           hr(),
           h4("遺跡解説"),
           htmlOutput("site_info"),
           
           hr(),
           h4("分布分析"),
           htmlOutput("summary")
    ),
    
    column(8,
           leafletOutput("map", height = "75vh")
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  
  filteredData <- reactive({
    df <- data
    
    if (input$filter_type != "すべて") {
      df <- df %>% filter(type == input$filter_type)
    }
    
    if (input$filter_period != "すべて") {
      df <- df %>% filter(period == input$filter_period)
    }
    
    df
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 130.20, lat = 33.57, zoom = 11)
  })
  
  observe({
    df <- filteredData()
    if (nrow(df) == 0) return()
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      clearControls() %>%
      
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        layerId = ~name,
        color = ~case_when(
          type == "王墓" ~ "red",
          type == "港湾" ~ "blue",
          type == "工房" ~ "green",
          type == "祭祀" ~ "purple",
          TRUE ~ "orange"
        ),
        fillOpacity = 0.9,
        popup = ~paste0("<b>", name, "</b>")
      )
  })
  
  observeEvent(input$map_marker_click, {
    click_id <- input$map_marker_click$id
    req(click_id)
    
    df <- filteredData()
    site <- df %>% filter(name == click_id)
    if (nrow(site) == 0) return()
    
    showModal(modalDialog(
      title = site$name,
      HTML(paste0(
        "<b>種類：</b>", site$type, "<br>",
        "<b>時代：</b>", site$period, "<br><br>",
        site$desc
      )),
      easyClose = TRUE
    ))
  })
  
  output$site_info <- renderUI({
    click <- input$map_marker_click
    
    if (is.null(click)) {
      return("地図上の遺跡をクリックしてください")
    }
    
    df <- filteredData()
    site <- df %>% filter(name == click$id)
    
    if (nrow(site) == 0) return(NULL)
    
    HTML(paste0(
      "<h4>", site$name, "</h4>",
      "<b>種類：</b>", site$type, "<br>",
      "<b>時代：</b>", site$period, "<br><br>",
      site$desc
    ))
  })
  
  output$summary <- renderUI({
    df <- filteredData()
    if (nrow(df) == 0) return(NULL)
    
    counts <- df %>% count(type)
    
    HTML(paste0(
      paste0(counts$type, "：", counts$n, "件", collapse = "<br>")
    ))
  })
}

# ===== 起動 =====
shinyApp(ui, server)