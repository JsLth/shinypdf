# ── Module: Sidebar (upload / file list / remove / preview) ──────────────────
#
# Owns the master `pdf_rv` store of loaded PDFs. Returns reactives that every
# other tab module depends on: the store itself, a couple of convenience
# views over it, and an "upload happened" event tabs use to clear stale
# results.

mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  sidebar(
    id      = ns("main_sidebar"),
    width   = 295,
    padding = "0.85rem",
    accordion(
      id    = ns("sidebar_accordion"),
      open  = c("upload_panel", "files_panel"),

      # ---- Upload panel --------------------------------------------------------
      accordion_panel(
        title = "Load PDFs",
        value = "upload_panel",
        icon  = bs_icon("cloud-arrow-up"),
        fileInput(
          inputId     = ns("pdf_upload"),
          label       = NULL,
          multiple    = TRUE,
          accept      = ".pdf",
          buttonLabel = tagList(bs_icon("folder2-open"), " Browse"),
          placeholder = "No files selected"
        )
      ),

      # ---- Files panel -----------------------------------------------------
      accordion_panel(
        title = "Loaded Files",
        value = "files_panel",
        icon  = bs_icon("files"),
        uiOutput(ns("sidebar_files_ui")),
        tags$hr(class = "my-2"),
        uiOutput(ns("sidebar_stats_ui"))
      )
    )
  )
}

#' @param has_pdf_viewer Reactive returning the root session's
#'   `input$has_pdf_viewer`.
#' @return A list of reactives shared with the tab modules: `pdf_rv`,
#'   `ordered_pdfs`, `pdf_names`, and `upload_event`.
mod_sidebar_server <- function(id, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    pdf_rv <- reactiveVal(list())

    # ── File upload ───────────────────────────────────────────────────────
    observeEvent(input$pdf_upload, {
      req(input$pdf_upload)
      current <- pdf_rv()
      dest_dir <- spdf_dir()

      renamed <- character(0)

      for (i in seq_len(nrow(input$pdf_upload))) {
        raw_name <- input$pdf_upload$name[[i]]
        src <- input$pdf_upload$datapath[[i]]
        name <- unique_name(raw_name, names(current))
        if (name != raw_name) renamed <- c(renamed, name)

        dest <- file.path(dest_dir, "uploads", paste0(tempfilename(), ".pdf"))
        ok <- file.copy(src, dest)
        if (!ok) {
          showNotification(
            paste0("Could not load \u201c", raw_name, "\u201d. The file may be in use."),
            type = "error", duration = 8
          )
          next
        }
        meta <- pdf_meta(dest)
        current[[name]] <- c(
          list(
            name = name,
            path = dest
          ),
          meta
        )
      }
      pdf_rv(current)

      if (length(renamed) > 0) {
        showNotification(
          paste0(
            "A file with that name was already loaded, so ",
            if (length(renamed) == 1) "it was" else "they were",
            " renamed to: ", paste(renamed, collapse = ", ")
          ),
          type = "message", duration = 6
        )
      }
    })

    # ── Remove a PDF ──────────────────────────────────────────────────────
    observeEvent(input$remove_pdf, remove_pdf_from(pdf_rv, input$remove_pdf, session))

    ordered_pdfs <- reactive(pdf_rv())
    pdf_names <- reactive(names(ordered_pdfs()))

    # ── File list ─────────────────────────────────────────────────────────
    output$sidebar_files_ui <- renderUI({
      pdfs <- ordered_pdfs()
      if (length(pdfs) == 0) {
        tags$p(
          class = "small text-secondary text-center py-2 mb-0",
          bs_icon("inbox"), " No PDFs loaded yet"
        )
      } else {
        tags$div(
          class = "list-group list-group-flush sidebar-file-list",
          lapply(names(pdfs), function(nm) {
            tags$div(
              class = "list-group-item d-flex align-items-center gap-1",
              tags$a(
                href = "#",
                class = "d-flex align-items-center gap-2 text-body text-decoration-none flex-grow-1 overflow-hidden",
                onclick = sprintf(
                  'Shiny.setInputValue(%s, %s, {priority: "event"}); return false;',
                  shQuote(session$ns("view_pdf")), shQuote(nm)
                ),
                bs_icon("file-pdf-fill", class = "text-danger flex-shrink-0"),
                tags$span(
                  nm, class = "text-truncate",
                  style = "font-size:.8125rem;"
                )
              ),
              tags$button(
                class = "btn btn-sm btn-outline-danger border-0 flex-shrink-0 py-0 px-1",
                title = "Remove",
                onclick = sprintf(
                  'event.stopPropagation(); pdfConfirmRemove(this, %s, %s)',
                  shQuote(nm), shQuote(session$ns("remove_pdf"))
                ),
                bs_icon("trash3")
              )
            )
          })
        )
      }
    })

    # ── Preview modal ─────────────────────────────────────────────────────
    observeEvent(input$view_pdf, {
      p <- pdf_rv()[[input$view_pdf]]
      req(!is.null(p))

      sendSweetAlert(
        session = session,
        title = p$name,
        html = TRUE,
        width = "900px",
        animation = FALSE,
        btn_labels = NA,
        showCloseButton = TRUE,
        closeOnClickOutside = TRUE,
        text = tagList(
          pdf_display(
            p$path,
            url = file.path("uploads", basename(p$path)),
            pages = p$pages,
            download = TRUE,
            has_pdf_viewer = has_pdf_viewer()
          ),
          tags$div(
            class = "d-flex justify-content-between align-items-center mt-3",
            tags$button(
              type = "button",
              class = "btn btn-outline-danger",
              onclick = sprintf(
                'pdfConfirmRemove(this, %s, %s)',
                shQuote(p$name), shQuote(session$ns("remove_pdf"))
              ),
              bs_icon("trash3"), " Remove"
            ),
            tags$span(class = "small text-secondary", format_size(p$size))
          )
        )
      )
    })

    output$sidebar_stats_ui <- renderUI({
      pdfs <- pdf_rv()
      if (length(pdfs) == 0) return(NULL)
      n_files <- length(pdfs)
      total_p <- sum(vapply(pdfs, function(p) if (is.na(p$pages)) 0L else p$pages, 0L))
      total_s <- sum(vapply(pdfs, function(p) if (is.na(p$size)) 0 else p$size, 0))
      tags$div(
        class = "d-flex justify-content-between stat-strip text-secondary",
        style = "width: 90%; padding-left: 10%;",
        tags$span(tagList(
          bs_icon("files"),
          " ",
          n_files,
          if (n_files == 1) " file" else " files"
        )),
        tags$span(tagList(bs_icon("file-text"), " ", total_p, "pp")),
        tags$span(format_size(total_s))
      )
    })

    list(
      pdf_rv = pdf_rv,
      ordered_pdfs = ordered_pdfs,
      pdf_names = pdf_names,
      upload_event = reactive(input$pdf_upload)
    )
  })
}
