sign_in_server <- function(id, active_user) {

  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

    shiny::observeEvent(input$sign_in_button, {
      shiny::req(input$sign_in_user, input$sign_in_password)

      # sign in validation -----
      formatted_log(user = input$sign_in_user, content = "sign in validation")

      shiny::withProgress(
        value = 1,
        message = "Signing in",
        expr = {
          query <- DBI::sqlInterpolate(
            conn = DBI::ANSI(),
            sql = read_sql_file(path = system.file("SQL/sign_in_validation.sql", package = 'shinyusermanagement')),
            username = input$sign_in_user,
            password = digest::hmac(
              key = Sys.getenv(x = "ENCRYPTION_KEY"),
              object = input$sign_in_password,
              algo = "sha512"
            )
          )

          try_result <- try(
            silent = TRUE,
            expr = DBI::dbGetQuery(conn = db_con, statement = query)
          )

        }
      )

      # analyze sign in try result -----
      formatted_log(
        user = input$sign_in_user,
        content = "analyze sign in try result"
      )

      if (class(try_result) == "try-error") {
        # sign in failed for unknown reasons
        formatted_log(
          user = input$sign_in_user,
          error = TRUE,
          content = attributes(try_result)$condition$message
        )

        shiny::updateTextInput(session = session, inputId = "sign_in_user", value = "")
        shiny::updateTextInput(session = session, inputId = "sign_in_password", value = "")

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Sign In", "Click", "Something Failed")
        )

        generic_modal(content = "Something went wrong, please try again")

      } else if (nrow(try_result) == 0) {
        # sign in failed due to wrong credentials
        formatted_log(
          user = input$sign_in_user,
          error = TRUE,
          content = "sign in failed due to wrong credentials"
        )

        shiny::updateTextInput(session = session, inputId = "sign_in_user", value = "")
        shiny::updateTextInput(session = session, inputId = "sign_in_password", value = "")

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Sign In", "Click", "Credentials Failed")
        )

        generic_modal(content = "Username and/or password are wrong")

      } else {
        # successful sign in
        formatted_log(user = input$sign_in_user, content = "successful sign in")

        active_user$username <- try_result$username

        session$sendCustomMessage(type = "aboutSectionHandler", message = "hide")

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Sign In", "Click", "Success")
        )
      }
    })
  })
}
