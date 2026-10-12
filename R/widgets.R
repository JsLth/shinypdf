passwordInputToggle <- function(inputId, label, value = "", width = NULL, placeholder = NULL) {
  div(
    class = c("form-group", "shiny-input-container"),
    width = width,
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      id = paste0(inputId, "-label"),
      `for` = inputId
    ),
    div(
      class = "input-group",
      tags$input(id = inputId, type = "password", class = "form-control"),
      tags$button(
        class = "input-group-text",
        onclick = sprintf("togglePassword('%s', '%s-toggle')", inputId, inputId),
        span(
          bsicons::bs_icon("eye"),
          bsicons::bs_icon("eye-slash", display = "none"),
          id = paste0(inputId, "-toggle")
        )
      )
    )
  )
}