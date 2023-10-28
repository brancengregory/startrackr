#
# This is a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#' @title run_app
#'
#' @export
#'
run_app <- function() {
  star_data <- gh_stars()

  star_tbl <- star_data |>
    gt::gt() |>
    gt::fmt_url(columns = url) |>
    gt::opt_interactive(
      use_search = TRUE,
      use_filters = TRUE
    )

  ui <- bslib::page_sidebar(
    title = "💫 startrackr",
    sidebar = "Sidebar",
    bslib::card(
      bslib::card_header("Stars"),
      gt::gt_output(outputId = "star_table")
    )
  )

  server <- function(input, output) {
    output$star_table <- gt::render_gt(star_tbl)
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
