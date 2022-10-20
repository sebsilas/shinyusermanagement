
#' Run the login panel
#'
#' @param title Title of login panel.
#' @param pre_login_content Content to go before login.
#' @param url Main URL of login panel.
#' @param footer_message Message to go in footer.
#' @param logged_in_ui UI extra to show on logged in page.
#' @param logged_in_message Message to show when someone is logged in.
#'
#' @return
#' @export
#'
#' @examples
run_app <- function(title = "R Shiny User Management & Authentication",
                    pre_login_content = pre_login_content_default,
                    url = "https://assets.yanirmor.com",
                    footer_message = shiny::tags$p("Need help? Contact
                                            ", shiny::tags$a(href = "mailto:sebsilas@gmail.com",
                                            "sebsilas@gmail.com")),
                    logged_in_ui = function() { shiny::tags$div() },
                    logged_in_message = "Here is a logged in message!") {

  ui <- shiny::basicPage(
    # page set up -----
    shiny::tags$head(
      shiny::tags$meta(charset = "UTF-8"),

      shiny::tags$link(href = "assets/img/icons/user.png", rel = "icon"),
      shiny::tags$link(href = "assets/img/icons/user.png", rel = "apple-touch-icon"),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css?family=Open+Sans"
      ),
      shiny::tags$title("R Shiny User Management & Authentication"),

    shiny::includeCSS(path = system.file("www/css/style.css", package = 'shinyusermanagement')),
    shiny::includeCSS(path = system.file("www/css/media_style.css", package = 'shinyusermanagement'),
    shiny::includeScript(path = system.file('www/js/script.js', package = 'shinyusermanagement'))
    ),

    shiny::tags$body(

      # header -----
      shiny::tags$header(
        shiny::tags$a(
          href = url,
        shiny::tags$div(
          id = "header_title",
          shiny::tags$img(src = "assets/img/icons/user.png"),
          title))

      ),

      # body -----
      shiny::tags$div(
        class = "wrapper",

        # dynamic ui section -----
        shiny::uiOutput("dynamic_ui")
      ),

      # footer -----
      shiny::tags$footer(
        shiny::tags$div(id = "footer_message", footer_message)
      )
    )
  ))



  server <- function(input, output, session) {

    # reactive values init -----
    active_user <- shiny::reactiveValues(username = NULL)

      # dynamic ui -----
      output$dynamic_ui <- shiny::renderUI({
        if (is.null(active_user$username)) {
          signed_out_ui("sign", pre_login_content)
        } else {
          signed_in_ui("sign", logged_in_ui, logged_in_message)
        }
      })

      # sign up
      sign_up_server("sign", active_user)

      # # sign in
      sign_in_server("sign", active_user)


      active_session_server("sign", active_user)

  }

    shiny::shinyApp(ui, server)

}



pre_login_content_default <- function() {
  shiny::tags$div(
    id = "about",

    shiny::tags$div(class = "content-title", "About"),
    "This app demonstrates a secured user management and authentication system in R Shiny.",
    shiny::tags$br(),
    "Users can sign up, and their credentials are stored in a PostgreSQL database.",
    shiny::tags$br(),
    "Once signed in, a user can see content specific to him/her (in this case, a favorite color), which no other user can access."
  )
}



