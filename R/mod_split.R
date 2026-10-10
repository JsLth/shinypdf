# ── Module: Split tab ──────────────────────────────────────────────────────

mod_split_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput(ns("split_pdf_select_ui")),
      uiOutput(ns("split_info_ui")),
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("info-circle"), " ",
        "Splits the PDF into individual single-page files, bundled as a ZIP."
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        ns("split_run"), "Split PDF",
        icon  = bs_icon("scissors"),
        class = "btn-primary w-100"
      )
    ),
    result_card(ns("split_result_ui"))
  )
}

mod_split_server <- function(id, pdf_rv, pdf_names, upload_event) {
  moduleServer(id, function(input, output, session) {
    split_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, split_result)

    output$split_pdf_select_ui <- pdf_select_renderer(session, pdf_names, "split_pdf_select", "Select PDF")

    output$split_info_ui <- renderUI({
      req(input$split_pdf_select)
      p <- pdf_rv()[[input$split_pdf_select]]
      req(!is.null(p), !is.na(p$pages))
      tags$p(
        class = "small text-secondary mb-0",
        sprintf("Will create %d file%s.", p$pages, if (p$pages == 1) "" else "s")
      )
    })

    output$split_result_ui <- renderUI({
      res <- split_result()
      if (is.null(res)) {
        empty_state_ui(
          "scissors",
          "Ready to split",
          "Select a PDF and click \u201cSplit PDF\u201d."
        )
      } else {
        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            sprintf("Split into %d pages", res$n_pages)
          ),
          tags$p(class = "small text-secondary mb-3", res$pdf_name),
          downloadButton(
            session$ns("split_download"),
            "Download ZIP",
            icon = bs_icon("file-zip"),
            class = "btn-primary"
          )
        )
      }
    })

    observeEvent(input$split_run, {
      on.exit(update_task_button(session = session, "split_run", state = "ready"))
      req(input$split_pdf_select)
      p <- pdf_rv()[[input$split_pdf_select]]
      req(!is.null(p))

      out_dir <- tempfile("split_")
      dir.create(out_dir)
      prefix <- file.path(out_dir, "page_")

      tryCatch({
        files <- pdf_split(p$path, output = prefix)
        zip_path <- tempfile(fileext = ".zip")
        zip(zip_path, files = basename(files), root = out_dir)
        split_result(list(
          zip_path = zip_path,
          n_pages = length(files),
          pdf_name = p$name
        ))
      }, error = function(e) {
        showNotification(
          paste("Split failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    output$split_download <- downloadHandler(
      filename = function() {
        paste0("split_", file_path_sans_ext(split_result()$pdf_name), ".zip")
      },
      content = function(file) file.copy(split_result()$zip_path, file)
    )
  })
}
