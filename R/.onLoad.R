.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "shinyusermanagement") # path to resource in your package
  )
}
