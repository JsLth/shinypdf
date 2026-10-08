server <- function(input, output, session) {
  create_shinypdf_dir()

  # ── State ───────────────────────────────────────────────────────────────────
  # pdf_rv: named list  name -> list(name, path, pages, size)
  pdf_rv <- reactiveVal(list())

  # Per-operation result storage
  split_result <- reactiveVal(NULL)
  subset_result <- reactiveVal(NULL)
  combine_result <- reactiveVal(NULL)
  rotate_result <- reactiveVal(NULL)
  compact_result <- reactiveVal(NULL)

  # ── File upload ─────────────────────────────────────────────────────────────
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
      dest <- file.path(dest_dir, "uploads", name)
      file.copy(src, dest, overwrite = TRUE)
      meta <- pdf_meta(dest)
      current[[name]] <- list(
        name = name,
        path = dest,
        pages = meta$pages,
        size = meta$size
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

    # Clear stale results when new files arrive
    split_result(NULL); subset_result(NULL)
    combine_result(NULL); rotate_result(NULL); compact_result(NULL)
  })

  # ── Remove a PDF ────────────────────────────────────────────────────────────
  observeEvent(input$remove_pdf, {
    current <- pdf_rv()
    current[[input$remove_pdf]] <- NULL
    pdf_rv(current)
    closeSweetAlert(session = session)
  })

  # ── Ordered PDFs (insertion order; combine tab has its own drag order) ──────
  ordered_pdfs <- reactive(pdf_rv())

  pdf_names <- reactive(names(ordered_pdfs()))

  # ── Sidebar: file list ───────────────────────────────────────────────────────
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
                'Shiny.setInputValue("view_pdf", %s, {priority: "event"}); return false;',
                shQuote(nm)
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
                'event.stopPropagation(); pdfConfirmRemove(this, %s)',
                shQuote(nm)
              ),
              bs_icon("trash3")
            )
          )
        })
      )
    }
  })

  # ── Sidebar: click a file to preview it in a popup ──────────────────────────
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
        tags$iframe(
          src   = paste0("uploads/", utils::URLencode(p$name)),
          style = "width: 100%; height: 70vh; border: none;"
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center mt-3",
          tags$button(
            type    = "button",
            class   = "btn btn-outline-danger",
            onclick = sprintf(
              'pdfConfirmRemove(this, %s)',
              shQuote(p$name)
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

  # ── Overview tab ────────────────────────────────────────────────────────────
  output$overview_ui <- renderUI({
    pdfs <- ordered_pdfs()
    if (length(pdfs) == 0) {
      return(tags$div(class = "p-4", empty_state_ui()))
    }

    total_pages <- sum(vapply(pdfs, function(p) if (is.na(p$pages)) 0L else p$pages, 0L))
    total_size  <- sum(vapply(pdfs, function(p) if (is.na(p$size))  0   else p$size,  0))

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
              tags$button(
                class = "btn btn-sm btn-outline-danger border-0 flex-shrink-0",
                title = "Remove",
                onclick = sprintf(
                  'event.stopPropagation(); pdfConfirmRemove(this, %s)',
                  shQuote(nm)
                ),
                bs_icon("trash3")
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
              )
            )
          )
        })
      )
    )
  })

  # ── Helpers: per-operation PDF selector ─────────────────────────────────────
  pdf_select_ui <- function(input_id, label_text, label_icon = "file-pdf") {
    renderUI({
      nms <- pdf_names()
      if (length(nms) == 0) {
        tags$p(
          class = "small text-secondary mb-0",
          bs_icon("exclamation-circle"), " Upload PDFs in the sidebar first."
        )
      } else {
        selectInput(
          input_id,
          label = tagList(bs_icon(label_icon), " ", label_text),
          choices = nms,
          width = "100%"
        )
      }
    })
  }

  page_hint_ui <- function(pdf_input_id) {
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

  # ── SPLIT ───────────────────────────────────────────────────────────────────
  output$split_pdf_select_ui <- pdf_select_ui("split_pdf_select", "Select PDF")

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
          "split_download",
          "Download ZIP",
          icon  = bs_icon("file-zip"),
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
        n_pages  = length(files),
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

  # ── SUBSET ──────────────────────────────────────────────────────────────────
  output$subset_pdf_select_ui  <- pdf_select_ui("subset_pdf_select", "Select PDF")
  output$subset_page_hint_ui   <- page_hint_ui("subset_pdf_select")

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
          "subset_download",
          "Download PDF",
          icon  = bs_icon("download"),
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
      tags$iframe(
        style = "height: 100%; width: 100%;",
        src = paste0("subset/", basename(res$path))
      )
    }
  })

  observeEvent(input$subset_run, {
    on.exit(update_task_button(session = session, "subset_run", state = "ready"))
    req(input$subset_pdf_select)
    p <- pdf_rv()[[input$subset_pdf_select]]
    req(!is.null(p), !is.na(p$pages))
    pages  <- parse_pages(input$subset_pages, max_page = p$pages)

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

  # ── COMBINE ─────────────────────────────────────────────────────────────────
  output$combine_rank_list_ui <- renderUI({
    nms <- pdf_names()
    if (length(nms) == 0) {
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("exclamation-circle"), " Upload at least 2 PDFs in the sidebar."
      )
    } else {
      rank_list(
        text     = NULL,
        labels   = nms,
        input_id = "combine_order",
        class    = "default-sortable"
      )
    }
  })

  output$combine_download_ui <- renderUI({
    res <- subset_result()
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
          "combine_download",
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
      tags$iframe(
        style = "height: 100%; width: 100%;",
        src = paste0("combine/", basename(res$path))
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

  # ── ROTATE ──────────────────────────────────────────────────────────────────
  output$rotate_pdf_select_ui <- pdf_select_ui("rotate_pdf_select", "Select PDF")
  output$rotate_page_hint_ui  <- page_hint_ui("rotate_pdf_select")

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
          "rotate_download",
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
      tags$iframe(
        style = "height: 100%; width: 100%;",
        src = paste0("rotate/", basename(res$path))
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

  # ── COMPACT ─────────────────────────────────────────────────────────────────

  # Detect tools once at session start (PATH won't change mid-session)
  gs_path <- find_gs()
  qpdf_path <- find_qpdf_bin()
  gs_ok <- nzchar(gs_path)
  qpdf_ok <- nzchar(qpdf_path)

  output$compact_pdf_select_ui <- pdf_select_ui("compact_pdf_select", "Select PDF")

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
        "compact_tool",
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
          "compact_download",
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
}

# Null-coalescing operator (base R >= 4.4 has it, provide fallback)
`%||%` <- function(x, y) if (is.null(x)) y else x
