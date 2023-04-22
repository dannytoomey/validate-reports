library(shiny)
install.packages('DT',repos='https://cran.rstudio.com')
library(DT)

port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)