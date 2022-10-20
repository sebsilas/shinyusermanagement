active_session_server <- function(id, active_user) {

  shiny::moduleServer(id, function(input, output, session) {

    output$signed_in_title <- shiny::renderText({
      paste0("Welcome, ", active_user$username, "!")
    })

    shiny::observeEvent(input$signed_in_new_password_button, {
      shiny::req(input$signed_in_new_password, input$signed_in_verify_new_password)

      # new password validation -----
      formatted_log(
        user = active_user$username,
        content = "new password validation"
      )

      validation_result <- all(
        input$signed_in_new_password %>% grepl(pattern = "^[A-z0-9]{6,12}$"),
        input$signed_in_new_password == input$signed_in_verify_new_password
      )

      if (!validation_result) {
        formatted_log(
          user = active_user$username,
          error = T,
          content = "new password validation failed"
        )

        shiny::updateTextInput(
          session = session,
          inputId = "signed_in_new_password",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "signed_in_verify_new_password",
          value = ""
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Password", "Validation Failed")
        )

        generic_modal(content = "Password must contain 6-12 letters or digits")
      }

      shiny::req(validation_result)

      # update new password -----
      formatted_log(user = active_user$username, content = "update new password")

      shiny::withProgress(
        value = 1,
        message = "Updating password",
        expr = {
          query <- DBI::sqlInterpolate(
            conn = DBI::ANSI(),
            sql = read_sql_file(path = system.file("SQL/update_new_password.sql", package = 'shinyusermanagement')),
            password = digest::hmac(
              key = Sys.getenv(x = "ENCRYPTION_KEY"),
              object = input$signed_in_new_password,
              algo = "sha512"
            ),
            username = active_user$username
          )

          conn <- connect_to_db()

          try_result <- try(
            silent = T,
            expr = DBI::dbExecute(conn = conn, statement = query),
          )

          DBI::dbDisconnect(conn = conn)
        }
      )

      # analyze update new password try result -----
      formatted_log(
        user = active_user$username,
        content = "analyze update new password try result"
      )

      if (class(try_result) == "try-error") {
        # update new password failed for unknown reasons
        formatted_log(
          user = active_user$username,
          error = TRUE,
          content = attributes(try_result)$condition$message
        )

        shiny::updateTextInput(
          session = session,
          inputId = "signed_in_new_password",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "signed_in_verify_new_password",
          value = ""
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Password", "Something Failed")
        )

        generic_modal(content = "Something went wrong, please try again")

      } else {
        # successful update new password
        formatted_log(
          user = active_user$username,
          content = "successful update new password"
        )

        shiny::updateTextInput(
          session = session,
          input = "signed_in_new_password",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          input = "signed_in_verify_new_password",
          value = ""
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Password", "Success")
        )

        generic_modal(error = FALSE, content = "Your password was updated successfully")
      }
    })

    shiny::observeEvent(input$sign_out_button, {
      active_user$username <- sign_out(session, active_user$username)
    })

    shiny::observeEvent(input$remove_account_button, {

      shinyalert::shinyalert("Are you sure you want to delete your account?",
                                           showCancelButton = TRUE,
                             type = "warning",
                             confirmButtonCol = '#f54842')
    })

    observeEvent(input$shinyalert, {

      active_user$username <- remove_account(session, active_user$username)

    })

  }) # End server module
}

remove_account <- function(session, username) {
  # remove account -----
  formatted_log(user = username, content = "remove account")

  shiny::withProgress(
    value = 1,
    message = "Removing account",
    expr = {
      query <- DBI::sqlInterpolate(
        conn = DBI::ANSI(),
        sql = read_sql_file(path = system.file("SQL/remove_account.sql", package = 'shinyusermanagement')),
        username = username
      )

      conn <- connect_to_db()

      try_result <- try(
        silent = TRUE,
        expr = DBI::dbExecute(conn = conn, statement = query),
      )

      DBI::dbDisconnect(conn = conn)
    }
  )

  # analyze remove account try result -----
  formatted_log(
    user = username,
    content = "analyze remove account try result"
  )

  if (class(try_result) == "try-error") {
    # remove account failed for unknown reasons
    formatted_log(
      user = username,
      error = TRUE,
      content = attributes(try_result)$condition$message
    )

    session$sendCustomMessage(
      type = "matomoEvent",
      message = c("Active Session", "Remove Account", "Something Failed")
    )

    generic_modal(content = "Something went wrong, please try again")

  } else {
    # successful remove account
    formatted_log(
      user = username,
      content = "successful remove account"
    )

    session$sendCustomMessage(type = "aboutSectionHandler", message = "show")

    session$sendCustomMessage(
      type = "matomoEvent",
      message = c("Active Session", "Remove Account", "Success")
    )

    generic_modal(error = FALSE, content = "Your account was removed successfully")

    username<- sign_out(session, username)

  }
  return(username)
}


sign_out <- function(session, username) {

  formatted_log(user = username, content = "sign out")

  session$sendCustomMessage(type = "aboutSectionHandler", message = "show")

  session$sendCustomMessage(
    type = "matomoEvent",
    message = c("Active Session", "Sign Out", "Success"))

    return(NULL)
}
