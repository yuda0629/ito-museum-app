# =============================
# 伊都国デジタル展示アプリ（完全版）
# コンセプト：伊都国における権力構造の可視化
# =============================

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# ===== データ読み込み =====
data <- read_csv("ito_sites_master.csv", show_col_types = FALSE)

# ===== 緯度経度の表示用（WGS84 / 度） =====
fmt_deg <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x[[1]])) {
    return("—")
  }
  sprintf("%.5f", x[[1]])
}

# ===== 遺跡種別ごとの配色（マスタに登場する全 type に対応） =====
type_levels <- sort(unique(as.character(data$type)))
n_types <- length(type_levels)
type_hues <- seq(15, 375, length.out = n_types + 1)[seq_len(n_types)]
type_palette_vec <- grDevices::hcl(h = type_hues, c = 78, l = 52)
names(type_palette_vec) <- type_levels
site_type_pal <- colorFactor(palette = type_palette_vec, domain = type_levels)

# ===== UI =====
ui <- fluidPage(
  titlePanel("伊都国デジタル展示 - 権力構造の可視化"),

  sidebarLayout(
    sidebarPanel(
      h3("展示ナビゲーション"),

      selectInput("era", "時代選択",
                  choices = c("すべて", unique(data$period))),

      checkboxGroupInput(
        "type", "遺跡種別",
        choiceValues = type_levels,
        choiceNames = lapply(type_levels, function(t) {
          tags$span(
            tags$span(
              style = paste0("color:", type_palette_vec[[t]], ";margin-right:0.35em;"),
              "●"
            ),
            t
          )
        }),
        selected = type_levels
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("① 伊都国とは",
                 h3("伊都国の概要"),
                 p("伊都国は弥生時代において外交・政治の中心的役割を担った地域である。"),
                 p("本展示では、その権力構造を遺跡データから読み解く。")
        ),

        tabPanel("② 分布を見る",
                 fluidRow(
                   column(8, leafletOutput("map", height = 600)),
                   column(4,
                          h3("遺跡詳細"),
                          htmlOutput("detail")
                   )
                 )
        ),

        tabPanel("③ 比較する",
                 selectInput("siteA", "比較対象A", choices = data$name),
                 selectInput("siteB", "比較対象B", choices = data$name),
                 tableOutput("compare")
        ),

        tabPanel("④ 結論",
                 h3("展示からわかること"),
                 p("伊都国では首長層を頂点とする階層的社会構造が形成されていたと考えられる。"),
                 p("大型墳墓による権威の表象と、それを支える集落の分布は、政治的統合の進展を示している。")
        )
      )
    )
  )
)

# ===== サーバー =====
server <- function(input, output, session) {

  selected_site <- reactiveVal(NULL)

  filtered <- reactive({
    df <- data

    if (input$era != "すべて") {
      df <- df %>% filter(period == input$era)
    }

    df <- df %>% filter(type %in% input$type)
    df
  })

  output$map <- renderLeaflet({
    df <- filtered()

    m <- leaflet(df) %>% addTiles()

    if (nrow(df) > 0) {
      m <- m %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          color = "white",
          fillColor = ~site_type_pal(type),
          fillOpacity = 0.88,
          layerId = ~name,
          popup = ~paste0(
            "<b>", name, "</b><br>",
            "種別：", type, "<br>",
            "緯度：", fmt_deg(lat), "　経度：", fmt_deg(lng)
          ),
          popupOptions = popupOptions(maxWidth = 240)
        )
    }

    m %>%
      addLegend(
        "bottomright",
        pal = site_type_pal,
        values = type_levels,
        title = "遺跡種別",
        opacity = 1
      )
  })

  observeEvent(input$map_marker_click, {
    selected_site(input$map_marker_click$id)
  })

  output$detail <- renderUI({
    req(selected_site())

    site <- data %>% filter(name == selected_site()) %>% slice(1)
    lat_s <- fmt_deg(site$lat)
    lng_s <- fmt_deg(site$lng)
    osm <- paste0(
      "https://www.openstreetmap.org/?mlat=", site$lat, "&mlon=", site$lng, "#map=15/"
    )

    HTML(paste0(
      "<h4>", site$name, "</h4>",
      "<p><b>緯度（WGS84）：</b>", lat_s, "</p>",
      "<p><b>経度（WGS84）：</b>", lng_s, "</p>",
      "<p><small><a href=\"", osm, "\" target=\"_blank\" rel=\"noopener\">地図で開く（OpenStreetMap）</a></small></p>",
      "<p><b>時代：</b>", site$period, "</p>",
      "<p><b>種別：</b>", site$type, "</p>",
      "<p>", site$desc, "</p>"
    ))
  })

  output$compare <- renderTable({
    a <- data %>% filter(name == input$siteA) %>% slice(1)
    b <- data %>% filter(name == input$siteB) %>% slice(1)

    data.frame(
      `項目` = c("緯度（WGS84）", "経度（WGS84）", "時代", "種別", "説明"),
      A = c(fmt_deg(a$lat), fmt_deg(a$lng), a$period, a$type, a$desc),
      B = c(fmt_deg(b$lat), fmt_deg(b$lng), b$period, b$type, b$desc)
    )
  })
}

# ===== 実行 =====
shinyApp(ui, server)
