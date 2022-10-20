
run_app <- function() {

  ui <- shiny::basicPage(
    # page set up -----
    shiny::tags$head(
      shiny::tags$meta(charset = "UTF-8"),

      shiny::tags$meta(
        name = "keywords",
        content = "R Shiny, Shiny, Shiny User Management, Shiny Authentication, Yanir Mor"
      ),

      shiny::tags$meta(
        name = "description",
        content = "Demonstration of user management and authentication system in R Shiny. Users can see content specific to them, which no other user can access."
      ),

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
        shiny::tags$div(
          id = "header_title",

          shiny::tags$img(src = "assets/img/icons/user.png"),
          shiny::tags$span("R Shiny", class = "third-color"),
          "User Management",
          shiny::tags$span("&", class = "third-color"),
          "Authentication"
        ),

        shiny::tags$div(
          id = "header_buttons",

          shiny::tags$a(
            href = "https://assets.yanirmor.com",
            target = "_blank",
            shiny::tags$img(src = "assets/img/icons/website.png"),
            title = "My Website"
          ),

          shiny::tags$a(
            href = "https://github.com/yanirmor/shiny-user-management",
            target = "_blank",
            shiny::tags$img(src = "assets/img/icons/github.png"),
            title = "Source Code"
          )
        )
      ),

      # body -----
      shiny::tags$div(
        class = "wrapper",

        # about section -----
        shiny::tags$div(
          id = "about",

          shiny::tags$div(class = "content-title", "About"),
          "This app demonstrates a secured user management and authentication system in R Shiny.",
          shiny::tags$br(),
          "Users can sign up, and their credentials are stored in a PostgreSQL database.",
          shiny::tags$br(),
          "Once signed in, a user can see content specific to him/her (in this case, a favorite color), which no other user can access."
        ),

        # dynamic ui section -----
        shiny::uiOutput("dynamic_ui")
      ),

      # footer -----
      shiny::tags$footer(
        shiny::tags$div(
          id = "footer_copyright",
          "2019",
          shiny::tags$span(class = "third-color", "Yanir Mor"),
          shiny::HTML("&copy;"),
          "All Rights Reserved",
          shiny::tags$span(
            id = "licenses",
            shiny::tags$span(class = "third-color", "(Licenses)"),
            shiny::tags$div(
              "Icon by Typicons / Iconfinder (CC BY-SA 3.0)"
            )
          )
        )
      )
    )
  ))



  server <- function(input, output, session) {

    # reactive values init -----
    active_user <- shiny::reactiveValues(username = NULL, color = NULL)

      # dynamic ui -----
      output$dynamic_ui <- shiny::renderUI({
        if (is.null(active_user$username)) {
          signed_out_ui("sign")
        } else {
          signed_in_ui("sign")
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



