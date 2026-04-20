# Entry: UTF-8 LC_CTYPE を設定してから App2_impl.R を parse（LC_CTYPE=C 時の <U+XXXX> 対策）
for (.loc in c("C.UTF-8", "en_US.UTF-8", "ja_JP.UTF-8")) {
  if (!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) {
    break
  }
}
eval(parse("App2_impl.R", encoding = "UTF-8"))
shinyApp(ui, server)
