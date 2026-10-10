# Standard two-column operation layout: settings (left) + result (right).
op_layout <- function(..., col_widths = c(4, 8)) {
  layout_columns(
    col_widths = col_widths,
    gap = "1rem",
    ...
  )
}

settings_card <- function(..., header_label, header_icon) {
  card(
    height = "100%",
    card_header(
      class = "d-flex align-items-center gap-2",
      bs_icon(header_icon, class = "text-secondary"),
      tags$span(header_label, class = "settings-label text-uppercase ls-wide")
    ),
    card_body(gap = "0.75rem", ...)
  )
}

result_card <- function(output_id) {
  card(
    height = "100%",
    card_header(
      class = "d-flex align-items-center gap-2",
      bs_icon("download", class = "text-secondary"),
      tags$span("Result", class = "settings-label text-uppercase")
    ),
    card_body(
      class = "result-zone p-0",
      uiOutput(output_id)
    )
  )
}

#' Render a "select a PDF" dropdown, namespaced for a module's server
#'
#' @param session   The module's `session` (needed to namespace the
#'   dynamically-created `selectInput` id).
#' @param pdf_names Reactive returning the character vector of loaded PDF names.
#' @param input_id  Local (unnamespaced) input id to assign the select input.
#' @param label_text,label_icon Label text / bs_icon name shown above the select.
#' @return A `renderUI` result, suitable for assignment to `output[[id]]`.
pdf_select_renderer <- function(session, pdf_names, input_id, label_text, label_icon = "file-pdf") {
  renderUI({
    nms <- pdf_names()
    if (length(nms) == 0) {
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("exclamation-circle"), " Upload PDFs in the sidebar first."
      )
    } else {
      selectInput(
        session$ns(input_id),
        label = tagList(bs_icon(label_icon), " ", label_text),
        choices = nms,
        width = "100%"
      )
    }
  })
}

#' Render a "this PDF has N pages" hint for a per-tab PDF selector
#'
#' @param input       The module's local `input`.
#' @param pdf_rv      Reactive returning the master PDF store (list keyed by name).
#' @param pdf_input_id Local input id of the tab's PDF select input.
#' @return A `renderUI` result, suitable for assignment to `output[[id]]`.
page_hint_renderer <- function(input, pdf_rv, pdf_input_id) {
  renderUI({
    req(input[[pdf_input_id]])
    p <- pdf_rv()[[input[[pdf_input_id]]]]
    req(!is.null(p), !is.na(p$pages))
    tags$p(
      class = "small text-secondary mt-1 mb-0",
      sprintf(
        "This PDF has %d page%s (1\u2013%d).",
        p$pages,
        if (p$pages == 1) "" else "s",
        p$pages
      )
    )
  })
}

#' Clear a tab's cached result reactiveVal whenever new PDFs are uploaded
#'
#' @param upload_event Reactive event from the sidebar module (fires with the
#'   raw `fileInput` value on each upload).
#' @param result_rv    The tab's own `reactiveVal` holding its last result.
reset_on_upload <- function(upload_event, result_rv) {
  observeEvent(upload_event(), result_rv(NULL), ignoreInit = TRUE, ignoreNULL = TRUE)
}

#' Remove a PDF from the shared store, shared by the sidebar and overview tab
#'
#' @param pdf_rv  The master `reactiveVal` holding the list of loaded PDFs.
#' @param name    Name of the PDF to remove.
#' @param session Optional module `session`, used to close an open sweet alert.
remove_pdf_from <- function(pdf_rv, name, session = NULL) {
  current <- pdf_rv()
  current[[name]] <- NULL
  pdf_rv(current)
  if (!is.null(session)) closeSweetAlert(session = session)
}