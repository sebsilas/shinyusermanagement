signed_in_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::includeScript(path = system.file("js/signed_in_script.js", package = 'shinyusermanagement')),

    shiny::tags$div(
      shiny::tags$div(class = "content-title",
                      shiny::textOutput(outputId = ns("signed_in_title"))),

      "This content is only accessible by you.",
      shiny::tags$br(),

      shiny::uiOutput(outputId = ns("signed_in_color")),
      shiny::tags$br(),

      shiny::tags$form(
        class = "inline-parent signed-in-form",

        shiny::textInput(
          inputId = ns("signed_in_new_color"),
          label = NULL,
          placeholder = "Change Favorite Color"
        ),

        shiny::actionButton(inputId = ns("signed_in_new_color_button"), label = "Apply")
      ),

      shiny::tags$form(
        class = "signed-in-form",

        shiny::passwordInput(
          inputId = ns("signed_in_new_password"),
          label = NULL,
          placeholder = "New Password (6-12 characters)"
        ),

        shiny::tags$div(
          class = "inline-parent",

          shiny::passwordInput(
            inputId = ns("signed_in_verify_new_password"),
            label = NULL,
            placeholder = "Verify New Password"
          ),

          shiny::actionButton(inputId = ns("signed_in_new_password_button"), label = "Apply")
        )
      ),

      shiny::tags$div(
        id = "signed_in_buttons",

        shiny::actionButton(inputId = ns("sign_out_button"), label = "Sign Out"),
        shiny::actionButton(
          inputId = ns("remove_account_button"),
          label = "Remove Account",
          class = "caution-button"
        )
      )
    )
  )
}
