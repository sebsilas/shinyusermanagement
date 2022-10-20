sign_up_server <- function(id, active_user) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

    shiny::observeEvent(input$sign_up_button, {
      shiny::req(
        input$sign_up_user,
        input$sign_up_color,
        input$sign_up_password,
        input$sign_up_verify_password
      )

      # sign up validation -----
      formatted_log(user = input$sign_up_user, content = "sign up validation")


      validation_result <- all(
        input$sign_up_user %>% grepl(pattern = "^[A-z0-9]{4,10}$"),
        input$sign_up_color %>% grepl(pattern = "^[A-z]{1}[A-z -]{0,18}[A-z]{1}$"),
        input$sign_up_password %>% grepl(pattern = "^[A-z0-9]{6,12}$"),
        input$sign_up_password == input$sign_up_verify_password
      )

      if (!validation_result) {
        formatted_log(
          user = input$sign_up_user,
          error = TRUE,
          content = "sign up validation failed"
        )

        shiny::updateTextInput(
          session = session,
          inputId = "sign_up_password",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "sign_up_verify_password",
          value = ""
        )

        session$sendCustomMessage(
          type = "matomoEvent",
          message = c("Sign Up", "Click", "Validation Failed")
        )

        generic_modal(
          content = list(
            "Something went wrong.",
            br(),
            "Please try again with the following rules:",
            br(), br(),
            tags$li("Username of 4-10 letters or digits"),
            tags$li("Color of 2-20 letters"),
            tags$li("Password of 6-12 letters or digits")
          )
        )
      }

      shiny::req(validation_result)

      # sign up a new user -----
      formatted_log(user = input$sign_up_user, content = "sign up a new user")

      shiny::withProgress(
        value = 1,
        message = "Signing up",
        expr = {


          if (does_user_exists(input$sign_up_user)) {


            # sign up failed due to taken username
            formatted_log(
              user = input$sign_up_user,
              error = TRUE,
              content = "sign up failed due to taken username"
            )

            shiny::updateTextInput(
              session = session,
              inputId = "sign_up_password",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "sign_up_verify_password",
              value = ""
            )

            session$sendCustomMessage(
              type = "matomoEvent",
              message = c("Sign Up", "Click", "Username Taken Failed")
            )

            generic_modal(content = "Username is already taken")

          } else {

            # successful sign up
            formatted_log(user = input$sign_up_user, content = "successful sign up")

            active_user$username <- input$sign_up_user
            active_user$color <- input$sign_up_color

            session$sendCustomMessage(type = "aboutSectionHandler", message = "hide")

            session$sendCustomMessage(
              type = "matomoEvent",
              message = c("Sign Up", "Click", "Success")
            )

          }
          conn <- connect_to_db()

          new_row <- data.frame(
            username = input$sign_up_user,
            color = input$sign_up_color,
            password = digest::hmac(
              key = Sys.getenv(x = "ENCRYPTION_KEY"),
              object = input$sign_up_password,
              algo = "sha512"
            ),
            created_at = strftime(x = Sys.time(), tz = "UTC"),
            is_enabled = 'y'
          )

          try_result <- try(
            silent = TRUE,
            expr = DBI::dbWriteTable(
              conn = conn,
              name = "user_management",
              value = new_row,
              overwrite = FALSE,
              append = TRUE
            )
          )

          DBI::dbDisconnect(conn = conn)

          if(try_result == FALSE) {
            generic_modal(content = "Sorry. An unknown error occured. Please try again.")
          }
        }
      )

    })
  })
}


does_user_exists <- function(proposed_username) {

  conn <- connect_to_db()
  tables <- DBI::dbListTables(conn)

  eval <- function() {
    tab <- DBI::dbReadTable(conn, 'user_management')
    print(tab)
    DBI::dbDisconnect(conn = conn)
    if(proposed_username %in% tab$username) return(TRUE) else return(FALSE)
  }

  if(length(tables) > 0L) {
    if("user_management" %in% tables) {
      return(eval())
    } else {
      return(FALSE) # The table has not been created, so the user can't have been either.
    }
  }

}
