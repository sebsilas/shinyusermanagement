
signed_out_ui <- function(id, pre_login_content, icon) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::includeScript(path = system.file("js/signed_out_script.js", package = 'shinyusermanagement')),

  shiny::tags$div(id = "prelogin", pre_login_content()),

    shiny::tags$br(),

    shiny::tags$form(
      id = ns("sign_in_section"),
      shiny::tags$div(class = "content-title", "Sign In"),

      shiny::textInput(
        inputId = ns("sign_in_user"),
        label = NULL,
        placeholder = "Username"
      ),

      shiny::passwordInput(
        inputId = ns("sign_in_password"),
        label = NULL,
        placeholder = "Password"
      ),

      shiny::actionButton(inputId = ns("sign_in_button"), label = "Sign In", type = "submit")
    ),

    shiny::tags$div(
      class = "vr-separator",

      shiny::tags$div(class = "vr-line"),
      shiny::tags$img(src = icon),
      shiny::tags$div(class = "vr-line")
    ),

    shiny::tags$form(
      id = "sign_up_section",
      shiny::tags$div(class = "content-title-alt", "Sign Up"),

      shiny::textInput(
        inputId = ns("sign_up_user"),
        label = NULL,
        placeholder = "Username (4-10 characters)"
      ),

      shiny::passwordInput(
        inputId = ns("sign_up_password"),
        label = NULL,
        placeholder = "Password (6-12 characters)"
      ),

      shiny::passwordInput(
        inputId = ns("sign_up_verify_password"),
        label = NULL,
        placeholder = "Verify Password"
      ),

      shiny::actionButton(inputId = ns("sign_up_button"), class = 'btn_alt', label = "Sign Up", type = "submit")
    )
  )

}
