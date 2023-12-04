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


is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}
