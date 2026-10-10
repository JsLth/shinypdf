# ── Module: Overview tab ───────────────────────────────────────────────────

mod_overview_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("overview_ui"))
}

mod_overview_server <- function(id, pdf_rv, ordered_pdfs) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$remove_pdf, remove_pdf_from(pdf_rv, input$remove_pdf, session))

    output$overview_ui <- renderUI({
      pdfs <- ordered_pdfs()
      if (length(pdfs) == 0) {
        return(tags$div(class = "p-4", empty_state_ui()))
      }

      total_pages <- sum(vapply(pdfs, function(p) if (is.na(p$pages)) 0L else p$pages, 0L))
      total_size  <- sum(vapply(pdfs, function(p) if (is.na(p$size)) 0 else p$size, 0))

      tagList(
        # Value boxes
        layout_column_wrap(
          width = "200px", fill = FALSE, class = "mb-3",
          value_box(
            title = "PDFs loaded",
            value = length(pdfs),
            showcase = bs_icon("files"),
            theme = "primary"
          ),
          value_box(
            title = "Total pages",
            value = total_pages,
            showcase = bs_icon("file-text"),
            theme = "info"
          ),
          value_box(
            title = "Total size",
            value = format_size(total_size),
            showcase = bs_icon("hdd"),
            theme = "success"
          )
        ),
        # Per-PDF cards
        layout_column_wrap(
          width = "280px",
          !!!lapply(names(pdfs), function(nm) {
            p <- pdfs[[nm]]
            card(
              class = "pdf-overview-card",
              card_header(
                class = "d-flex align-items-center justify-content-between gap-2",
                tags$div(
                  class = "d-flex align-items-center gap-2 overflow-hidden",
                  bs_icon("file-pdf-fill", class = "text-danger flex-shrink-0"),
                  tags$span(
                    nm, class = "text-truncate fw-medium",
                    style = "font-size:.875rem; max-width:190px;"
                  )
                ),
                tags$div(
                  class = "d-flex align-items-center gap-1 flex-shrink-0",
                  pdf_flag_icons(p),
                  tags$button(
                    class = "btn btn-sm btn-outline-danger border-0",
                    title = "Remove",
                    onclick = sprintf(
                      'event.stopPropagation(); pdfConfirmRemove(this, %s, %s)',
                      shQuote(nm), shQuote(session$ns("remove_pdf"))
                    ),
                    bs_icon("trash3")
                  )
                )
              ),
              card_body(
                class = "py-3",
                layout_column_wrap(
                  width = 1 / 2, fill = FALSE,
                  tags$div(
                    class = "text-center",
                    tags$div(
                      class = "fw-bold fs-3 lh-1 text-primary",
                      if (is.na(p$pages)) "?" else p$pages
                    ),
                    tags$div(class = "small text-secondary mt-1", "pages")
                  ),
                  tags$div(
                    class = "text-center",
                    tags$div(
                      class = "fw-bold fs-5 lh-1 text-success",
                      format_size(p$size)
                    ),
                    tags$div(class = "small text-secondary mt-1", "file size")
                  )
                ),
                pdf_meta_details(p)
              )
            )
          })
        )
      )
    })
  })
}
