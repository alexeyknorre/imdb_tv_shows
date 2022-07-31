library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = "shiny",
  host = '0.0.0.0',
  port = as.numeric(port)
)
