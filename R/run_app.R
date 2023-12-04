
#' Run the login panel
#'
#' @param title Title of login panel.
#' @param pre_login_content Content to go before login.
#' @param url Main URL of login panel.
#' @param footer_message Message to go in footer.
#' @param logged_in_ui UI extra to show on logged in page.
#' @param logged_in_message Message to show when someone is logged in.
#' @param on_start_fun Function to execute on launch
#' @param css_stylesheet An optional additional css stylesheet.
#' @param logo_image Logo image.
#' @param icon Icon.
#' @param on_sign_up_fun A function to run when the user signs up. Must have db_con and username arguments.
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
                    logged_in_message = "Here is a logged in message!",
                    on_start_fun = function() {
                      logging::loginfo("Initialising app")
                    },
                    css_stylesheet = NULL,
                    logo_image = "assets/img/icons/user.png",
                    icon = "assets/img/icons/user.png",
                    on_sign_up_fun = NULL) {

  stopifnot(
    is.null.or(css_stylesheet, is.scalar.character)
  )


  ui <- shiny::basicPage(
    # page set up -----
    shiny::tags$head(
      shiny::tags$meta(charset = "UTF-8"),

      shiny::tags$link(href = logo_image, rel = "icon"),
      shiny::tags$link(href = icon, rel = "apple-touch-icon"),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css?family=Open+Sans"
      ),
      shiny::tags$title("R Shiny User Management & Authentication"),

    if(is.null(css_stylesheet)) shiny::includeCSS(path = system.file("www/css/style.css", package = 'shinyusermanagement')),
    if(is.null(css_stylesheet)) shiny::includeCSS(path = system.file("www/css/media_style.css", package = 'shinyusermanagement')),
    if(!is.null(css_stylesheet)) shiny::tags$link(rel = "stylesheet", href = css_stylesheet),
    shiny::includeScript(path = system.file('js/script.js', package = 'shinyusermanagement'))
    ),

    shiny::tags$body(

      # header -----
      shiny::tags$header(
        shiny::tags$a(
          href = url,
        shiny::tags$div(
          id = "header_title",
          shiny::tags$img(src = logo_image),
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
  )



  server <- function(input, output, session) {



    # reactive values init -----
    active_user <- shiny::reactiveValues(username = NULL)

      # dynamic ui -----
      output$dynamic_ui <- shiny::renderUI({
        if (is.null(active_user$username)) {
          signed_out_ui("sign", pre_login_content, icon)
        } else {
          signed_in_ui("sign", logged_in_ui, logged_in_message, active_user$username)
        }
      })

      # sign up
      sign_up_server("sign", active_user, on_sign_up_fun)

      # # sign in
      sign_in_server("sign", active_user)


      active_session_server("sign", active_user)

  }

    shiny::shinyApp(ui, server,
                    onStart = on_start_fun)

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



