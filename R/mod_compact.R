# ── Module: Compact tab ────────────────────────────────────────────────────

mod_compact_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    col_widths = c(5, 7),
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",

      # PDF selector
      uiOutput(ns("compact_pdf_select_ui")),

      # Tool selector — rendered server-side so only available tools appear.
      # Shows a warning card instead when neither gs nor qpdf is installed.
      uiOutput(ns("compact_tool_ui")),

      # ── Ghostscript options (only visible when gs is selected) ──────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'gs'", ns("compact_tool")),
        tags$div(
          class = "border rounded-3 p-3 mt-1",
          style = "background: var(--bs-tertiary-bg);",

          tags$p(
            class = "small fw-semibold text-uppercase text-secondary mb-3",
            bs_icon("gear"), " Ghostscript options"
          ),

          # Quality preset
          selectInput(
            ns("gs_preset"),
            label = tagList(bs_icon("stars"), " Quality preset"),
            choices = c(
              "None (manual settings below)" = "",
              "Screen \u2014 72 dpi, smallest file"  = "screen",
              "eBook \u2014 150 dpi, balanced"        = "ebook",
              "Printer \u2014 300 dpi, print quality" = "printer",
              "Prepress \u2014 300 dpi, color-safe"   = "prepress"
            ),
            selected = "ebook",
            width = "100%"
          ),

          # PDF compatibility level
          selectInput(
            ns("gs_compat"),
            label = tagList(bs_icon("file-earmark-check"), " PDF compatibility"),
            choices = c(
              "PDF 1.3 (Acrobat 4)" = "1.3",
              "PDF 1.4 (Acrobat 5)" = "1.4",
              "PDF 1.5 (Acrobat 6)" = "1.5",
              "PDF 1.6 (Acrobat 7)" = "1.6",
              "PDF 1.7 (Acrobat 8)" = "1.7"
            ),
            selected = "1.5",
            width = "100%"
          ),

          tags$hr(class = "my-2"),
          tags$p(
            class = "small fw-semibold text-secondary mb-2",
            bs_icon("image"), " Image downsampling"
          ),

          layout_column_wrap(
            width = 1 / 2, fill = FALSE, gap = "0.5rem",
            numericInput(ns("gs_color_dpi"), "Color (DPI)",
                         value = 150, min = 36, max = 2400, step = 1, width = "100%"),
            numericInput(ns("gs_gray_dpi"), "Gray (DPI)",
                         value = 150, min = 36, max = 2400, step = 1, width = "100%")
          ),
          layout_column_wrap(
            width = 1 / 2, fill = FALSE, gap = "0.5rem",
            numericInput(ns("gs_mono_dpi"), "Mono (DPI)",
                         value = 300, min = 36, max = 2400, step = 1, width = "100%"),
            selectInput(
              ns("gs_downsample"), "Downsample method",
              choices = c("Bicubic", "Average", "Subsample"),
              selected = "Bicubic",
              width = "100%"
            )
          ),

          tags$hr(class = "my-2"),
          tags$p(
            class = "small fw-semibold text-secondary mb-2",
            bs_icon("fonts"), " Font handling"
          ),

          layout_column_wrap(
            width = 1 / 2, fill = FALSE,
            input_switch(ns("gs_embed_fonts"),    "Embed all fonts", value = TRUE),
            input_switch(ns("gs_subset_fonts"),   "Subset fonts",    value = TRUE)
          ),
          input_switch(ns("gs_compress_fonts"), "Compress fonts", value = TRUE)
        )
      ),

      tags$hr(class = "my-1"),
      input_task_button(
        ns("compact_run"), "Compact PDF",
        icon  = bs_icon("file-zip"),
        class = "btn-primary w-100"
      )
    ),
    result_card(ns("compact_result_ui"))
  )
}

mod_compact_server <- function(id, pdf_rv, pdf_names, upload_event) {
  moduleServer(id, function(input, output, session) {
    compact_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, compact_result)

    # Detect tools once at session start (PATH won't change mid-session)
    gs_path <- find_gs()
    qpdf_path <- find_qpdf_bin()
    gs_ok <- nzchar(gs_path)
    qpdf_ok <- nzchar(qpdf_path)

    output$compact_pdf_select_ui <- pdf_select_renderer(session, pdf_names, "compact_pdf_select", "Select PDF")

    # Tool selector: only shows tools that are actually installed.
    # Falls back to a warning card when neither is available.
    output$compact_tool_ui <- renderUI({
      if (!gs_ok && !qpdf_ok) {
        card(
          class = "bg-warning-subtle border-warning-subtle mt-1",
          card_body(
            class = "py-2 px-3",
            tags$p(
              class = "small mb-0",
              bs_icon("exclamation-triangle"), " ",
              tags$strong("Neither Ghostscript nor qpdf found on PATH."),
              " Only internal redundant-object removal will be attempted.",
              " Install either tool and restart the app for full compression."
            )
          )
        )
      } else {
        choices <- c("Default (auto-detect)" = "default")
        if (qpdf_ok) choices <- c(choices, "qpdf: linearise & clean" = "qpdf")
        if (gs_ok) choices <- c(choices, "Ghostscript: full re-distill" = "gs")
        selectInput(
          session$ns("compact_tool"),
          label = tagList(bs_icon("wrench"), " Compression tool"),
          choices  = choices,
          selected = if (gs_ok) "gs" else "qpdf",
          width    = "100%"
        )
      }
    })

    output$compact_result_ui <- renderUI({
      res <- compact_result()
      if (is.null(res)) {
        empty_state_ui(
          "file-zip",
          "Ready to compact",
          "Select a PDF and click \u201cCompact PDF\u201d."
        )
      } else {
        savings_pct <- if (!is.na(res$original_size) && res$original_size > 0) {
          round(100 * (1 - res$new_size / res$original_size), 1)
        } else NA_real_

        savings_label <- if (!is.na(savings_pct) && savings_pct > 0) {
          badge(sprintf("\u2212%.1f%%", savings_pct), "success")
        } else if (!is.na(savings_pct) && savings_pct <= 0) {
          badge("No reduction achieved", "secondary")
        } else {
          badge("Size unknown", "secondary")
        }

        tool_badge <- switch(
          res$tool %||% "default",
          gs = badge("Ghostscript", "info"),
          qpdf = badge("qpdf", "primary"),
          default = badge("auto", "secondary")
        )

        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(class = "mt-2 mb-1 fw-semibold", "Compacted successfully"),
          tags$div(class = "mb-2", tool_badge),
          tags$div(
            class = "d-flex justify-content-center align-items-center gap-3 mb-3",
            tags$div(
              tags$div(class = "small text-secondary", "Before"),
              tags$div(class = "fw-semibold", format_size(res$original_size))
            ),
            bs_icon("arrow-right", class = "text-secondary"),
            tags$div(
              tags$div(class = "small text-secondary", "After"),
              tags$div(class = "fw-semibold text-success", format_size(res$new_size))
            ),
            tags$div(savings_label)
          ),
          downloadButton(
            session$ns("compact_download"),
            "Download PDF",
            icon  = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    observeEvent(input$compact_run, {
      on.exit(update_task_button(session = session, "compact_run", state = "ready"))
      req(input$compact_pdf_select)
      p <- pdf_rv()[[input$compact_pdf_select]]
      req(!is.null(p))
      tool <- input$compact_tool %||% "default"

      out <- tempfile(fileext = ".pdf")
      file.copy(p$path, out)

      tryCatch({
        if (tool == "gs") {
          preset <- if (nzchar(input$gs_preset %||% "")) input$gs_preset else NULL

          # Additional arguments beyond the preset
          extras <- c(
            # Compatibility
            paste0("-dCompatibilityLevel=", input$gs_compat %||% "1.5"),
            # Image downsampling
            paste0("-dColorImageResolution=", input$gs_color_dpi %||% 150),
            paste0("-dGrayImageResolution=", input$gs_gray_dpi  %||% 150),
            paste0("-dMonoImageResolution=", input$gs_mono_dpi  %||% 300),
            paste0("-dColorImageDownsampleType=/", input$gs_downsample %||% "Bicubic"),
            paste0("-dGrayImageDownsampleType=/", input$gs_downsample %||% "Bicubic"),
            paste0("-dMonoImageDownsampleType=/", input$gs_downsample %||% "Bicubic"),
            # Font handling
            paste0("-dEmbedAllFonts=", tolower(isTRUE(input$gs_embed_fonts))),
            paste0("-dSubsetFonts=", tolower(isTRUE(input$gs_subset_fonts))),
            paste0("-dCompressFonts=", tolower(isTRUE(input$gs_compress_fonts)))
          )

          compactPDF(
            out,
            gs_cmd = gs_path,
            gs_quality = preset,
            gs_extras = extras
          )
        } else if (tool == "qpdf") {
          compactPDF(out, qpdf = qpdf_path)
        } else {
          compactPDF(out)
        }

        compact_result(list(
          path = out,
          pdf_name = p$name,
          tool = tool,
          original_size = p$size,
          new_size = file.size(out)
        ))
      }, error = function(e) {
        showNotification(
          paste("Compact failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    output$compact_download <- downloadHandler(
      filename = function() {
        paste0(
          "compacted_",
          file_path_sans_ext(compact_result()$pdf_name),
          ".pdf"
        )
      },
      content = function(file) file.copy(compact_result()$path, file)
    )
  })
}
