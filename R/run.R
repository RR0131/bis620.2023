#' this function run the app
#' @return a shiny app
#' @export

run <- function(){
  app <- system.file("App", package = "bis620.2023")
  shiny::runApp(app)
}
