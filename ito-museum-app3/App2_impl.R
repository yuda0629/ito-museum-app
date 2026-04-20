# =============================
# 伊都国デジタル展示アプリ（完全版）
# コンセプト：伊都国における権力構造の可視化
# （App2.r / app.R で UTF-8 ロケールを設定したあと parse して読み込む）
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
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$style(HTML("
      body { background-color: #ffffff !important; }
      .container-fluid { background-color: #ffffff; }
      .tab-content { background-color: #ffffff; padding-top: 0.5rem; }
      .leaflet-container { background-color: #d8ecf8 !important; }
    "))
  ),
  titlePanel("伊都国デジタル展示 - 権力構造の可視化"),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; border: 1px solid #e8e8e8; border-radius: 6px;",
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
      style = "background-color: #ffffff;",
      tabsetPanel(
        id = "view_tabs",
        tabPanel(
          value = "tab_intro",
          title = "① 伊都国とは",
          h3("伊都国の概要"),
          p("伊都国は弥生時代において外交・政治の中心的役割を担った地域である。"),
          p("本展示では、その権力構造を遺跡データから読み解く。")
        ),

        tabPanel(
          value = "tab_map",
          title = "② 分布を見る",
          fluidRow(
            column(12, leafletOutput("map", height = 600))
          ),
          fluidRow(
            column(12, h3("遺跡詳細"), htmlOutput("detail"))
          )
        ),

        tabPanel(
          value = "tab_compare",
          title = "③ 比較する",
          selectInput("siteA", "比較対象A", choices = data$name),
          selectInput("siteB", "比較対象B", choices = data$name),
          tableOutput("compare")
        ),

        tabPanel(
          value = "tab_outro",
          title = "④ 結論",
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
  map_render_tick <- reactiveVal(0L)

  observeEvent(input$view_tabs, {
    if (identical(input$view_tabs, "tab_map")) {
      map_render_tick(map_render_tick() + 1L)
    }
  }, ignoreNULL = TRUE)

  filtered <- reactive({
    df <- data

    if (input$era != "すべて") {
      df <- df %>% filter(period == input$era)
    }

    types <- input$type
    if (is.null(types) || !length(types)) {
      types <- type_levels
    }
    df <- df %>% filter(type %in% types)
    df
  })

  output$map <- renderLeaflet({
    map_render_tick()
    req(identical(input$view_tabs, "tab_map"))

    df <- filtered()
    sel <- selected_site()

    if (nrow(df) == 0) {
      return(
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addTiles() %>%
          setView(lng = 130.2, lat = 33.55, zoom = 10) %>%
          addLegend(
            "bottomright",
            pal = site_type_pal,
            values = type_levels,
            title = "遺跡種別",
            opacity = 1
          )
      )
    }

    is_sel <- rep(FALSE, nrow(df))
    if (!is.null(sel)) {
      is_sel <- df$name == sel
    }
    df <- df %>%
      mutate(
        mr = ifelse(is_sel, 16L, 8L),
        mw = ifelse(is_sel, 4L, 2L),
        mcol = ifelse(is_sel, "#f5b000", "white")
      )

    m <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = df$lng,
        lat = df$lat,
        radius = df$mr,
        stroke = TRUE,
        weight = df$mw,
        color = df$mcol,
        fillColor = site_type_pal(df$type),
        fillOpacity = 0.88,
        layerId = df$name,
        popup = paste0(
          "<b>", df$name, "</b><br>",
          "種別：", df$type, "<br>",
          "緯度：", sprintf("%.5f", suppressWarnings(as.numeric(df$lat))),
          "　経度：",
          sprintf("%.5f", suppressWarnings(as.numeric(df$lng)))
        ),
        popupOptions = popupOptions(maxWidth = 240)
      )

    lng1 <- min(df$lng, na.rm = TRUE)
    lng2 <- max(df$lng, na.rm = TRUE)
    lat1 <- min(df$lat, na.rm = TRUE)
    lat2 <- max(df$lat, na.rm = TRUE)
    if (is.finite(lng1) && is.finite(lng2) && is.finite(lat1) && is.finite(lat2)) {
      if (lng1 == lng2 && lat1 == lat2) {
        m <- m %>% setView(lng = lng1, lat = lat1, zoom = 13)
      } else {
        m <- m %>% fitBounds(lng1, lat1, lng2, lat2)
      }
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
