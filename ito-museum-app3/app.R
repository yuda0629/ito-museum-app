# shinyapps.io / Posit Connect 用の既定エントリ（app.R）
# LC_CTYPE=C だと日本語 UI が <U+XXXX> になるため、parse 前に UTF-8 ロケールを試す。
for (.loc in c("C.UTF-8", "en_US.UTF-8", "ja_JP.UTF-8")) {
  if (!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) {
    break
  }
}

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

app_env <- new.env(parent = globalenv())
eval(parse("App2_impl.R", encoding = "UTF-8"), envir = app_env)
shiny::shinyApp(ui = app_env$ui, server = app_env$server)
