mod_extract_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput(ns("extract_pdf_select_ui")),
      radioButtons(
        ns("extract_type"),
        label = tagList(bs_icon("arrow-clockwise"), " What to extract?"),
        choices = c(
          "Embedded images" = "images",
          "Text" = "text",
          "Attachments" = "attachments"
        ),
        selected = "images"
      ),
      div(
        tags$label(
          class = "form-label settings-label mb-1",
          bs_icon("list-ol"), " Pages to extract"
        ),
        textInput(
          ns("extract_pages"), label = NULL,
          placeholder = "e.g.\u2002 1, 3, 5\u20148, 11"
        ),
        uiOutput(ns("extract_page_hint_ui"))
      ),
      tags$hr(class = "my-1"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'images'", ns("extract_type")),
        selectInput(
          ns("extract_image_format"),
          label = "Image format",
          choices = c(
            "PNG" = "png",
            "JPEG" = "jpeg",
            "WebP" = "webp"
          ),
          selected = "png"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'png'", ns("extract_image_format")),
          numericInput("extract_image_quality", value = 0.92, label = "Image quality (only JPEG / WebP)")
        ),
        div(
          tags$label(
            class = "form-label settings-label mb-1",
            bs_icon("arrows-expand-vertical"), " Minimum size to extract (in pixels)"
          ),
          span(
            textInput("extract_image_width", label = NULL, width = 150),
            "\u2a2f",
            textInput("extract_image_height", label = NULL, width = 150)
          )
        ),
        checkboxInput("extract_image_inline", label = "Include inline images"),
        checkboxInput("extract_image_dedupe", label = "Skip duplicates")
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        ns("extract_run"), "Extract",
        icon  = bs_icon("box-arrow-up"),
        class = "btn-primary w-100"
      ),
      tags$hr(class = "my-1"),
      uiOutput(ns("extract_download_ui"))
    ),
    result_card(ns("extract_result_ui"))
  )
}


mod_extract_server <- function(id, pdf_rv, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    extract_result <- reactiveVal(NULL)
    reset_on_upload(upload_event, extract_result)

    output$extract_pdf_select_ui <- pdf_select_renderer(session, pdf_names, "extract_pdf_select", "Select PDF")
    output$extract_page_hint_ui  <- page_hint_renderer(input, pdf_rv, "extract_pdf_select")
  })
}