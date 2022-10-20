formatted_log <- function(user, error = F, content) {
  message("user ", user, " -- ", if (error) "ERROR -- ", content, "\n")
}

generic_modal <- function(error = T, content) {
  title <- if (error) {
    shiny::tags$span(shiny::tags$img(src = "assets/img/icons/user_red.png", "Oops..."))
  } else {
    shiny::tags$span(shiny::tags$img(src = "assets/img/icons/user_green.png", "Success!"))
  }

  showModal(
    modalDialog(
      title = title,
      easyClose = TRUE,
      content
    )
  )
}

connect_to_db <- function() {
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = Sys.getenv(x = "DB_HOST"),
    port = Sys.getenv(x = "DB_PORT"),
    dbname = Sys.getenv(x = "DB_NAME"),
    user = Sys.getenv(x = "DB_USER"),
    password = Sys.getenv(x = "DB_PASSWORD")
  )
}

read_sql_file <- function(path) {
  path %>% readLines() %>% paste(collapse = " ")
}
