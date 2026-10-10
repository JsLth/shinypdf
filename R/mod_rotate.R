mod_rotate_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput(ns("rotate_pdf_select_ui")),
      div(
        tags$label(
          class = "form-label settings-label mb-1",
          bs_icon("list-ol"), " Pages to rotate"
        ),
        textInput(
          ns("rotate_pages"), label = NULL,
          placeholder = "Leave blank for all pages"
        ),
        uiOutput(ns("rotate_page_hint_ui"))
      ),
      radioButtons(
        ns("rotate_angle"),
        label = tagList(bs_icon("arrow-clockwise"), " Rotation angle"),
        choices = c(
          "90\u00b0 clockwise" = "90",
          "180\u00b0" = "180",
          "270\u00b0 clockwise" = "270"
        ),
        selected = "90"
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        ns("rotate_run"), "Rotate Pages",
        icon  = bs_icon("arrow-clockwise"),
        class = "btn-primary w-100"
      ),
      tags$hr(class = "my-1"),
      uiOutput(ns("rotate_download_ui"))
    ),
    result_card(ns("rotate_result_ui"))
  )
}

mod_rotate_server <- function(id, pdf_rv, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    rotate_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, rotate_result)

    output$rotate_pdf_select_ui <- pdf_select_renderer(session, pdf_names, "rotate_pdf_select", "Select PDF")
    output$rotate_page_hint_ui  <- page_hint_renderer(input, pdf_rv, "rotate_pdf_select")

    output$rotate_download_ui <- renderUI({
      res <- rotate_result()
      if (!is.null(res)) {
        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            sprintf(
              "Rotated %d page%s by %d\u00b0",
              res$n_pages,
              if (res$n_pages == 1) "" else "s",
              res$angle
            )
          ),
          tags$p(class = "small text-secondary mb-3", res$pdf_name),
          downloadButton(
            session$ns("rotate_download"),
            "Download PDF",
            icon = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    output$rotate_result_ui <- renderUI({
      res <- rotate_result()
      if (is.null(res)) {
        empty_state_ui(
          "arrow-clockwise",
          "Ready to rotate",
          "Choose pages and angle, then click \u201cRotate Pages\u201d."
        )
      } else {
        pdf_display(
          res$path,
          url = file.path("rotate", basename(res$path)),
          pages = res$n_pages,
          download = FALSE,
          has_pdf_viewer = has_pdf_viewer()
        )
      }
    })

    observeEvent(input$rotate_run, {
      on.exit(update_task_button(
        session = session,
        "rotate_run",
        state = "ready"
      ))
      req(input$rotate_pdf_select)
      p <- pdf_rv()[[input$rotate_pdf_select]]
      req(!is.null(p), !is.na(p$pages))
      angle <- as.integer(input$rotate_angle)

      pages_str <- trimws(input$rotate_pages %||% "")
      pages <- if (nzchar(pages_str)) {
        parse_pages(pages_str, max_page = p$pages)
      } else {
        seq_len(p$pages)
      }

      if (length(pages) == 0) {
        showNotification(
          "No valid pages specified.",
          type = "warning",
          duration = 5
        )
        return()
      }

      out <- file.path(spdf_dir(), "rotate", tempfilename(fileext = ".pdf"))
      tryCatch({
        pdf_rotate_pages(p$path, pages = pages, angle = angle, output = out)
        rotate_result(list(
          path = out,
          n_pages = length(pages),
          angle = angle,
          pdf_name = p$name
        ))
      }, error = function(e) {
        showNotification(
          paste("Rotate failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    output$rotate_download <- downloadHandler(
      filename = function() {
        paste0("rotated_", file_path_sans_ext(rotate_result()$pdf_name), ".pdf")
      },
      content = function(file) file.copy(rotate_result()$path, file)
    )
  })
}
