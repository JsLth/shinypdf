# ── Module: Subset tab ─────────────────────────────────────────────────────

mod_subset_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput(ns("subset_pdf_select_ui")),
      div(
        tags$label(
          class = "form-label settings-label mb-1",
          bs_icon("list-ol"), " Pages to extract"
        ),
        textInput(
          ns("subset_pages"), label = NULL,
          placeholder = "e.g.\u2002 1, 3, 5\u20148, 11"
        ),
        uiOutput(ns("subset_page_hint_ui"))
      ),
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("info-circle"), " ",
        "Extract a specific set of pages into a new PDF."
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        ns("subset_run"), "Extract Pages",
        icon  = bs_icon("collection"),
        class = "btn-primary w-100"
      ),
      tags$hr(class = "my-1"),
      uiOutput(ns("subset_download_ui"))
    ),
    result_card(ns("subset_result_ui"))
  )
}

mod_subset_server <- function(id, pdf_rv, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    subset_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, subset_result)

    output$subset_pdf_select_ui <- pdf_select_renderer(session, pdf_names, "subset_pdf_select", "Select PDF")
    output$subset_page_hint_ui <- page_hint_renderer(input, pdf_rv, "subset_pdf_select")

    output$subset_download_ui <- renderUI({
      res <- subset_result()
      if (!is.null(res)) {
        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            sprintf(
              "Extracted %d page%s",
              res$n_pages,
              if (res$n_pages == 1) "" else "s"
            )
          ),
          tags$p(class = "small text-secondary mb-3", res$pdf_name),
          downloadButton(
            session$ns("subset_download"),
            "Download PDF",
            icon = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    output$subset_result_ui <- renderUI({
      res <- subset_result()
      if (is.null(res)) {
        empty_state_ui(
          "collection",
          "Ready to extract",
          "Select pages and click \u201cExtract Pages\u201d."
        )
      } else {
        pdf_display(
          res$path,
          url = file.path("subset", basename(res$path)),
          pages = res$n_pages,
          download = FALSE,
          has_pdf_viewer = has_pdf_viewer()
        )
      }
    })

    observeEvent(input$subset_run, {
      on.exit(update_task_button(session = session, "subset_run", state = "ready"))
      req(input$subset_pdf_select)
      p <- pdf_rv()[[input$subset_pdf_select]]
      req(!is.null(p), !is.na(p$pages))
      pages <- parse_pages(input$subset_pages, max_page = p$pages)

      if (length(pages) == 0) {
        showNotification(
          "No valid page numbers found. Use a format like: 1, 3, 5-8",
          type = "warning", duration = 6
        )
        return()
      }

      out <- file.path(spdf_dir(), "subset", tempfilename(fileext = ".pdf"))
      tryCatch({
        pdf_subset(p$path, pages = pages, output = out)
        subset_result(list(
          path = out,
          n_pages = length(pages),
          pdf_name = p$name
        ))
      }, error = function(e) {
        showNotification(
          paste("Subset failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    output$subset_download <- downloadHandler(
      filename = function() {
        paste0("subset_", file_path_sans_ext(subset_result()$pdf_name), ".pdf")
      },
      content = function(file) file.copy(subset_result()$path, file)
    )
  })
}
