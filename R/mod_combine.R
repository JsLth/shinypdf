# ── Module: Combine tab ────────────────────────────────────────────────────

mod_combine_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    col_widths = c(5, 7),
    card(
      height = "100%",
      card_header(
        class = "d-flex align-items-center gap-2",
        bs_icon("sliders", class = "text-secondary"),
        tags$span("PDF Order", class = "settings-label text-uppercase")
      ),
      card_body(
        tags$p(
          class = "small text-secondary mb-2",
          bs_icon("grip-vertical"), " ",
          "Drag to reorder. PDFs are merged top to bottom."
        ),
        uiOutput(ns("combine_rank_list_ui")),
        tags$hr(class = "my-2"),
        input_task_button(
          ns("combine_run"), "Combine PDFs",
          icon  = bs_icon("union"),
          class = "btn-primary w-100"
        ),
        tags$hr(class = "my-2"),
        uiOutput(ns("combine_download_ui"))
      )
    ),
    result_card(ns("combine_result_ui"))
  )
}

mod_combine_server <- function(id, pdf_rv, ordered_pdfs, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    combine_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, combine_result)

    output$combine_rank_list_ui <- renderUI({
      nms <- pdf_names()
      if (length(nms) == 0) {
        tags$p(
          class = "small text-secondary mb-0",
          bs_icon("exclamation-circle"), " Upload at least 2 PDFs in the sidebar."
        )
      } else {
        rank_list(
          text = NULL,
          labels = nms,
          input_id = session$ns("combine_order"),
          class = "default-sortable"
        )
      }
    })

    output$combine_download_ui <- renderUI({
      # Note: this previously read `subset_result()` instead of
      # `combine_result()` — fixed as part of the module split.
      res <- combine_result()
      if (!is.null(res)) {
        tags$div(
          class = "result-success text-center",
          bs_icon(
            "check-circle-fill",
            class = "text-success"
          ),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            sprintf(
              "Combined %d PDFs, %d pages",
              res$n_files, res$n_pages
            )
          ),
          tags$p(
            class = "small text-secondary mb-3",
            format_size(res$size)
          ),
          downloadButton(
            session$ns("combine_download"),
            "Download PDF",
            icon  = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    output$combine_result_ui <- renderUI({
      res <- combine_result()
      if (is.null(res)) {
        empty_state_ui(
          "union",
          "Ready to combine",
          "Arrange your PDFs and click \u201cCombine PDFs\u201d."
        )
      } else {
        pdf_display(
          res$path,
          url = file.path("combine", basename(res$path)),
          pages = res$n_pages,
          download = FALSE,
          has_pdf_viewer = has_pdf_viewer()
        )
      }
    })

    observeEvent(input$combine_run, {
      on.exit(update_task_button(
        session = session,
        "combine_run",
        state = "ready"
      ))
      all_pdfs <- ordered_pdfs()

      # Respect the rank list order from the combine tab
      ordered_names <- input$combine_order
      if (!is.null(ordered_names) && length(ordered_names) > 0) {
        valid <- intersect(ordered_names, names(all_pdfs))
        rest <- setdiff(names(all_pdfs), valid)
        all_pdfs <- all_pdfs[c(valid, rest)]
      }

      if (length(all_pdfs) < 2) {
        showNotification(
          "Load at least 2 PDFs before combining.",
          type = "warning",
          duration = 5
        )
        return()
      }

      paths <- vapply(all_pdfs, `[[`, character(1L), "path")
      out <- file.path(spdf_dir(), "combine", tempfilename(fileext = ".pdf"))

      tryCatch({
        pdf_combine(paths, output = out)
        meta <- pdf_meta(out)
        combine_result(list(
          path = out,
          n_pages = meta$pages,
          n_files = length(paths),
          size = meta$size))
      }, error = function(e) {
        showNotification(
          paste("Combine failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    output$combine_download <- downloadHandler(
      filename = function() "combined.pdf",
      content  = function(file) file.copy(combine_result()$path, file)
    )
  })
}
