sign_up_server <- function(id, active_user) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

    shiny::observeEvent(input$sign_up_button, {
      shiny::req(
        input$sign_up_user,
        input$sign_up_password,
        input$sign_up_verify_password
      )

      # sign up validation -----
      formatted_log(user = input$sign_up_user, content = "Sign up validation.")


      validation_result <- all(
        input$sign_up_user %>% grepl(pattern = "^[A-z0-9]{3,10}$"),
        input$sign_up_password %>% grepl(pattern = "^[A-z0-9]{6,12}$"),
        input$sign_up_password == input$sign_up_verify_password
      )

      if (!validation_result) {
        formatted_log(
          user = input$sign_up_user,
          error = TRUE,
          content = "Sign up validation failed."
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
            tags$li("Username of 3-10 letters or digits"),
            tags$li("Password of 6-12 letters or digits")
          )
        )
      }

      shiny::req(validation_result)

      # sign up a new user -----
      formatted_log(user = input$sign_up_user, content = "Sign up a new user.")

      shiny::withProgress(
        value = 1,
        message = "Signing up",
        expr = {


          if (does_user_exists(input$sign_up_user)) {

            # Sign up failed due to taken username
            formatted_log(
              user = input$sign_up_user,
              error = TRUE,
              content = "Sign up failed due to taken username."
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
            formatted_log(user = input$sign_up_user, content = "Successful sign up!")

            active_user$username <- input$sign_up_user

            session$sendCustomMessage(type = "aboutSectionHandler", message = "hide")

            session$sendCustomMessage(
              type = "matomoEvent",
              message = c("Sign Up", "Click", "Success")
            )

          }
          conn <- connect_to_db()

          new_row <- data.frame(
            username = input$sign_up_user,
            password = digest::hmac(
              key = Sys.getenv(x = "ENCRYPTION_KEY"),
              object = input$sign_up_password,
              algo = "sha512"
            ),
            created_at = strftime(x = Sys.time(), tz = "UTC"),
            enabled = TRUE
          )

          try_result <- try(
            silent = TRUE,
            expr = DBI::dbWriteTable(
              conn = conn,
              name = "users",
              value = new_row,
              overwrite = FALSE,
              append = TRUE
            )
          )

          DBI::dbDisconnect(conn = conn)

          if(!try_result) {
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
    tab <- DBI::dbReadTable(conn, 'users')
    print(tab)
    DBI::dbDisconnect(conn = conn)
    return(proposed_username %in% tab$username)
  }

  if(length(tables) > 0L) {
    if("users" %in% tables) {
      return(eval())
    } else {
      return(FALSE) # The table has not been created, so the user can't have been either.
    }
  }

}
