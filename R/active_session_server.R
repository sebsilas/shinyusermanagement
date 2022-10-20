active_session_server <- function(id, active_user) {

  shiny::moduleServer(id, function(input, output, session) {

    output$signed_in_title <- shiny::renderText({
      paste0("Welcome, ", active_user$username, "!")
    })

    output$signed_in_color <- shiny::renderUI({
      shiny::tags$p("Your favorite color is ",
        shiny::tags$span(class = "highlighted", active_user$color), ".")
    })

    shiny::observeEvent(input$signed_in_new_color_button, {

      shiny::req(input$signed_in_new_color)

      # new color validation -----
      formatted_log(user = active_user$username, content = "new color validation")

      validation_result <- input$signed_in_new_color %>%
        grepl(pattern = "^[A-z]{1}[A-z -]{0,18}[A-z]{1}$")

      if (validation_result == FALSE) {
        formatted_log(
          user = active_user$username,
          error = TRUE,
          content = "new color validation failed"
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Color", "Validation Failed")
        )

        generic_modal(content = "Color must contain 2-20 letters")
      }

      shiny::req(validation_result)

      # update new color -----
      formatted_log(user = active_user$username, content = "update new color")

      shiny::withProgress(
        value = 1,
        message = "Updating favorite color",
        expr = {
          query <- DBI::sqlInterpolate(
            conn = DBI::ANSI(),
            sql = read_sql_file(path = system.file("SQL/update_new_color.sql", package = 'shinyusermanagement')),
            color = input$signed_in_new_color,
            username = active_user$username
          )

          conn <- connect_to_db()

          try_result <- try(
            silent = TRUE,
            expr = DBI::dbExecute(conn = conn, statement = query),
          )

          DBI::dbDisconnect(conn = conn)
        }
      )

      # analyze update new color try result -----
      formatted_log(
        user = active_user$username,
        content = "analyze update new color try result"
      )

      if (class(try_result) == "try-error") {
        # update new color failed for unknown reasons
        formatted_log(
          user = active_user$username,
          error = TRUE,
          content = attributes(try_result)$condition$message
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Color", "Something Failed")
        )

        generic_modal(content = "Something went wrong, please try again")

      } else {
        # successful update new color
        formatted_log(
          user = active_user$username,
          content = "successful update new color"
        )

        active_user$color <- input$signed_in_new_color

        shiny::updateTextInput(
          session = session,
          input = "signed_in_new_color",
          value = ""
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "New Color", "Success")
        )
      }
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
      # sign out
      formatted_log(user = active_user$username, content = "sign out")

      active_user$username <- NULL
      active_user$color <- NULL

      session$sendCustomMessage(type = "aboutSectionHandler", message = "show")

      session$sendCustomMessage(
        type = "matomoEvent",
        message = c("Active Session", "Sign Out", "Success")
      )
    })

    shiny::observeEvent(input$remove_account_button, {
      # remove account -----
      formatted_log(user = active_user$username, content = "remove account")

      shiny::withProgress(
        value = 1,
        message = "Removing account",
        expr = {
          query <- DBI::sqlInterpolate(
            conn = DBI::ANSI(),
            sql = read_sql_file(path = system.file("SQL/remove_account.sql", package = 'shinyusermanagement')),
            username = active_user$username
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
        user = active_user$username,
        content = "analyze remove account try result"
      )

      if (class(try_result) == "try-error") {
        # remove account failed for unknown reasons
        formatted_log(
          user = active_user$username,
          error = T,
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
          user = active_user$username,
          content = "successful remove account"
        )

        active_user$username <- NULL
        active_user$color <- NULL

        session$sendCustomMessage(type = "aboutSectionHandler", message = "show")

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Active Session", "Remove Account", "Success")
        )

        generic_modal(error = FALSE, content = "Your account was removed successfully")
      }
    })
  })
}
