
signed_in_ui <- function(id,
                         logged_in_ui,
                         logged_in_message = "Here is a login message!",
                         username) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::includeScript(path = system.file("js/signed_in_script.js", package = 'shinyusermanagement')),

    shiny::tags$div(

      shiny::tags$div(class = "content-title",
                      shiny::textOutput(outputId = ns("signed_in_title"))),

      shiny::tags$br(),

      shiny::tags$p(class = "logged_in_message", logged_in_message),

      shiny::tags$br(),

        logged_in_ui(
          account_settings = account_details(ns),
          username = username
          ),

      shiny::tags$div(
        id = "signed_in_buttons",
        shiny::actionButton(inputId = ns("sign_out_button"), label = "Sign Out"),
      )
    )
  )
}


account_details <- function(ns) {
  shiny::tabPanel("Account Details",

    shiny::tags$form(
      class = "signed-in-form",

      shiny::passwordInput(
        inputId = ns("signed_in_new_password"),
        label = "Change your password",
        placeholder = "New Password (6-12 characters)"
      ),

      shiny::tags$div(
        class = "inline-parent",

        shiny::passwordInput(
          inputId = ns("signed_in_verify_new_password"),
          label = NULL,
          placeholder = "Verify New Password"
        ),

        shiny::actionButton(inputId = ns("signed_in_new_password_button"), label = "Change Password"),
        shiny::tags$hr(),
        shiny::actionButton(
          inputId = ns("remove_account_button"),
          label = "Remove Account",
          class = "caution-button"
        ),
        shiny::tags$br(),
        shiny::tags$hr()
      )
    )
  )
}
