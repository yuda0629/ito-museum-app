# shinyapps.io / Posit Connect 用の既定エントリ（app.R）
# App2.r の定義を読み込み、最後の shinyApp 行は重複起動を避けるため除外する
library(shiny)
library(leaflet)
library(dplyr)
library(readr)

app_env <- new.env(parent = globalenv())
lines <- readLines("App2.r", encoding = "UTF-8", warn = FALSE)
lines <- lines[!grepl("^\\s*shinyApp\\s*\\(", lines)]
eval(parse(text = paste(lines, collapse = "\n"), keep.source = FALSE), envir = app_env)
shiny::shinyApp(ui = app_env$ui, server = app_env$server)
