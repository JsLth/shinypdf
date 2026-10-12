mod_extract_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon = "sliders",
      uiOutput(ns("pdf_select_ui")),
      radioButtons(
        ns("type"),
        label = tagList(bs_icon("box-arrow-up"), " What to extract?"),
        choices = c(
          "Embedded images" = "images",
          "Text" = "text",
          "Attachments" = "attachments"
        ),
        selected = "images"
      ),
      div(
        tags$label(
          bs_icon("list-ol"), " Pages to extract"
        ),
        textInput(
          ns("n_pages"), label = NULL,
          placeholder = "e.g.\u2002 1, 3, 5\u20138, 11"
        ),
        uiOutput(ns("page_hint_ui"))
      ),
      tags$hr(class = "my-1"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'images'", ns("type")),
        selectInput(
          ns("image_format"),
          label = span(bs_icon("file-image"), "Image format"),
          choices = c(
            "PNG" = "png",
            "JPEG" = "jpeg",
            "WebP" = "webp",
            "WebP (lossless)" = "webp_lossless"
          ),
          selected = "png"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'png'", ns("image_format")),
          numericInput(
            "image_quality",
            min = 0,
            max = 100,
            step = 1,
            value = 92,
            label = span(
              bs_icon("noise-reduction"),
              " Image quality (only JPEG / WebP)",
              popover(
                bs_icon("question-circle"),
                tags$p(HTML(
                  "For lossy image formats (JPEG and WebP), this option specifies the degree to which
                  discrete cosine transform (DCT) coefficients are quantized during lossy compression.
                  This can be a number between 0 and 100.<br>
                  <ul>
                    <li><b>90%+:</b> Considerable size reduction at barely any perceivable quality loss</li>
                    <li><b>80%+:</b> Good size-versus-quality tradeoff with barely perceivable quality loss at massive size reudctions</li>
                    <li><b>below 80%: </b>Image quality loss becomes more obvious</li>
                  </ul><br>Keep in mind that a quality of 100% does not mean lossless."
                ))
              )
            )
          )
        ),
        div(
          tags$label(
            bs_icon("arrows-expand-vertical"),
            " Minimum size to extract (in pixels)",
            popover(
              bs_icon("question-circle"),
              tags$p(
                "Specifies the minimum width of images to be extracted (width \u00d7 height).
                By default, 1x1 stubs are not included. You can customize this to only extract
                over a certain size."
              )
            )
          ),
          div(
            style = "display: flex; alight-items: center; gap: 8px;",
            div(
              class = "form-group shiny-input-container",
              style = "width: 70px;",
              tags$input(
                id = "image_width",
                type = "text",
                class = "shiny-input-text form-control text-input-light",
                value = 2,
                placeholder = "Width"
              )
            ),
            span("\u00D7"),
            div(
              class = "form-group shiny-input-container",
              style = "width: 70px;",
              tags$input(
                id = "image_height",
                type = "text",
                class = "shiny-input-text form-control text-input-light",
                value = 2,
                placeholder = "Height"
              )
            )
          )
        ),
        div(
          class = "form-group shiny-input-container",
          div(
            class = "checkbox",
            tags$input(
              id = ns("image_inline"),
              type = "checkbox",
              class = "shiny-input-checkbox"
            ),
            tags$label("Include inline images"),
            popover(
              bs_icon("question-circle"),
              tags$p(
                "By default, only larger images are extracted. When this option is checked,
                also extracts inline images."
              )
            )
          )
        ),
        div(
          class = "form-group shiny-input-container",
          div(
            class = "checkbox",
            tags$input(
              id = ns("image_dedupe"),
              type = "checkbox",
              class = "shiny-input-checkbox"
            ),
            tags$label("Skip duplicates"),
            popover(
              bs_icon("question-circle"),
              tags$p(
                "By default, all images are extracted. When this option is checked,
                compares image hashes of already extracted images to prevent pixel-perfect
                matches to be extracted twice."
              )
            )
          )
        )
      ),
      tags$hr(class = "my-1"),
      div(
        id = ns("run_tip"),
        input_task_button(
          ns("run"), "Extract",
          icon  = bs_icon("box-arrow-up"),
          class = "btn-primary w-100"
        )
      ),
      tags$hr(class = "my-1"),
      uiOutput(ns("download_ui"))
    ),
    result_card(ns("result_ui"))
  )
}


mod_extract_server <- function(id, pdf_rv, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    iv <- InputValidator$new()
    iv$add_rule("pdf_select", sv_required())
    iv$enable()

    extract_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, extract_result)

    output$pdf_select_ui <- pdf_select_renderer(session, pdf_names, "pdf_select", "Select PDF")
    output$page_hint_ui  <- page_hint_renderer(input, pdf_rv, "pdf_select")

    output$download_ui <- renderUI({
      type <- input$type
      res <- input$extracted_images$images
      if (!is.null(res)) {
        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            switch(
              input$type,
              text = sprintf(
                "Extracted %d characters and %d words of text.)",
                0,
                0
              ),
              {
                n_pages <- length(unique(vapply(res, "[[", "page", FUN.VALUE = numeric(1))))
                sprintf(
                  "Extracted %d %s from %d page%s",
                  length(res),
                  if (length(res) > 1) type else substr(type, 1, nchar(type) - 1),
                  n_pages,
                  if (n_pages > 1) "s" else ""
                )
              }
            )
          ),
          tags$p(class = "small text-secondary mb-3", res$pdf_name),
          downloadButton(
            session$ns("download"),
            "Download ZIP",
            icon = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    observe({
      on.exit(update_task_button(
        session = session,
        "run",
        state = "ready"
      ))
      req(iv$is_valid())
      p <- pdf_rv()[[input$pdf_select]]

      pages_str <- trimws(input$n_pages %||% "")
      pages <- if (nzchar(pages_str)) {
        parse_pages(pages_str, max_page = p$pages)
      } else {
        seq_len(p$pages)
      }

      iv$add_rule("n_pages", ~ if (!length(pages)) "No valid pages specified.")
      req(iv$is_valid())

      extract_pdf_images(
        file.path("uploads", basename(p$path)),
        format = input$image_format,
        quality = input$image_quality,
        pages = pages,
        min_width = input$image_width,
        min_height = input$image_height,
        inline = input$image_inline,
        dedupe = input$image_dedupe
      )
    }) |>
      bindEvent(input$run)
    
    observe({
      img <- input$extracted_images
      if (!is.null(img$error)) {
        showNotification(
          img$error,
          type = "error"
        )
      }
    }) |>
      bindEvent(input$extracted_images)

    output$download <- downloadHandler(
      filename = function() {
        paste0(input$type, file_path_sans_ext(input$pdf_select), ".zip")
      },
      content = function(file) {
        imgs <- input$extracted_images$images
        tmp_dir <- tempfile("imgzip_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

        for (i in seq_along(imgs)) {
          img <- imgs[[i]]
          data_url <- img$dataUrl
          b64 <- sub("^data:.*;base64,", "", data_url)

          ext <- case_switch(
            grepl("^data:image/jpeg", data_url) ~ "jpg",
            grepl("^data:image/webp", data_url) ~ "webp",
            .default = "png"
          )

          fname <- sprintf("page_%s_%s.%s", img$page, img$objId, ext)
          out_path <- file.path(tmp_dir, fname)
          write_b64(b64, out_path)
        }

        zip::zip(
          zipfile = file,
          files = list.files(tmp_dir),
          root = tmp_dir
        )
      },
      contentType = "application/zip"
    )
  })
}