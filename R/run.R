#' Launch the PDF Toolkit Shiny app
#'
#' Opens the interactive PDF manipulation dashboard in your browser.
#'
#' @return A [shiny::shinyApp()] object (invisibly).
#' @export
run_app <- function() {
  shinyApp(ui = build_ui(), server = server)
}
