# ── UI builder (called at runtime, not at package-load time) ──────────────────

build_ui <- function() {

# ── Theme ─────────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version      = 5,
  primary      = "#2563eb",
  secondary    = "#64748b",
  success      = "#16a34a",
  info         = "#0891b2",
  warning      = "#d97706",
  danger       = "#dc2626",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius"       = "0.5rem",
  "btn-border-radius"   = "0.4rem",
  "card-border-radius"  = "0.75rem",
  "card-cap-bg"         = "transparent",
  "card-cap-padding-y"  = "0.875rem",
  "card-cap-padding-x"  = "1.1rem",
  "card-box-shadow"     = "0 1px 4px rgba(0,0,0,.07), 0 1px 2px rgba(0,0,0,.04)",
  "accordion-button-active-bg" = "rgba(37,99,235,.07)",
  "accordion-button-active-color" = "#2563eb"
)

# ── Sidebar ────────────────────────────────────────────────────────────────────

app_sidebar <- sidebar(
  id      = "main_sidebar",
  width   = 295,
  padding = "0.85rem",
  accordion(
    id    = "sidebar_accordion",
    open  = c("upload_panel", "files_panel"),

    # ---- Upload panel --------------------------------------------------------
    accordion_panel(
      title = "Load PDFs",
      value = "upload_panel",
      icon  = bs_icon("cloud-arrow-up"),
      fileInput(
        inputId     = "pdf_upload",
        label       = NULL,
        multiple    = TRUE,
        accept      = ".pdf",
        buttonLabel = tagList(bs_icon("folder2-open"), " Browse"),
        placeholder = "No files selected"
      )
    ),

    # ---- Files panel ---------------------------------------------------------
    accordion_panel(
      title = "Loaded Files",
      value = "files_panel",
      icon  = bs_icon("files"),
      uiOutput("sidebar_files_ui"),
      tags$hr(class = "my-2"),
      uiOutput("sidebar_stats_ui")
    )
  )
)

# ── Tab helpers ────────────────────────────────────────────────────────────────

# Standard two-column operation layout: settings (left) + result (right).
op_layout <- function(..., col_widths = c(4, 8)) {
  layout_columns(
    col_widths = col_widths,
    gap        = "1rem",
    ...
  )
}

settings_card <- function(..., header_label, header_icon) {
  card(
    height = "100%",
    card_header(
      class = "d-flex align-items-center gap-2",
      bs_icon(header_icon, class = "text-secondary"),
      tags$span(header_label, class = "settings-label text-uppercase ls-wide")
    ),
    card_body(gap = "0.75rem", ...)
  )
}

result_card <- function(output_id) {
  card(
    height = "100%",
    card_header(
      class = "d-flex align-items-center gap-2",
      bs_icon("download", class = "text-secondary"),
      tags$span("Result", class = "settings-label text-uppercase")
    ),
    card_body(
      class = "result-zone p-0",
      uiOutput(output_id)
    )
  )
}

# ── Tab: Overview ──────────────────────────────────────────────────────────────

overview_tab <- nav_panel(
  title = tagList(bs_icon("grid-1x2"), " Overview"),
  value = "tab_overview",
  uiOutput("overview_ui")
)

# ── Tab: Split ─────────────────────────────────────────────────────────────────

split_tab <- nav_panel(
  title = tagList(bs_icon("scissors"), " Split"),
  value = "tab_split",
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput("split_pdf_select_ui"),
      uiOutput("split_info_ui"),
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("info-circle"), " ",
        "Splits the PDF into individual single-page files, bundled as a ZIP."
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        "split_run", "Split PDF",
        icon  = bs_icon("scissors"),
        class = "btn-primary w-100"
      )
    ),
    result_card("split_result_ui")
  )
)

# ── Tab: Subset ────────────────────────────────────────────────────────────────

subset_tab <- nav_panel(
  title = tagList(bs_icon("collection"), " Subset"),
  value = "tab_subset",
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput("subset_pdf_select_ui"),
      div(
        tags$label(
          class = "form-label settings-label mb-1",
          bs_icon("list-ol"), " Pages to extract"
        ),
        textInput(
          "subset_pages", label = NULL,
          placeholder = "e.g.\u2002 1, 3, 5-8, 11"
        ),
        uiOutput("subset_page_hint_ui")
      ),
      tags$p(
        class = "small text-secondary mb-0",
        bs_icon("info-circle"), " ",
        "Extract a specific set of pages into a new PDF."
      ),
      tags$hr(class = "my-1"),
      input_task_button(
        "subset_run", "Extract Pages",
        icon  = bs_icon("collection"),
        class = "btn-primary w-100"
      ),
      tags$hr(class = "my-1"),
      uiOutput("subset_download_ui")
    ),
    result_card("subset_result_ui")
  )
)

# ── Tab: Combine ───────────────────────────────────────────────────────────────

combine_tab <- nav_panel(
  title = tagList(bs_icon("union"), " Combine"),
  value = "tab_combine",
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
          "Drag to reorder — PDFs are merged top to bottom."
        ),
        uiOutput("combine_rank_list_ui"),
        tags$hr(class = "my-2"),
        input_task_button(
          "combine_run", "Combine PDFs",
          icon  = bs_icon("union"),
          class = "btn-primary w-100"
        ),
        tags$hr(class = "my-2"),
        uiOutput("combine_download_ui")
      )
    ),
    result_card("combine_result_ui")
  )
)

# ── Tab: Rotate ────────────────────────────────────────────────────────────────

rotate_tab <- nav_panel(
  title = tagList(bs_icon("arrow-clockwise"), " Rotate"),
  value = "tab_rotate",
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput("rotate_pdf_select_ui"),
      div(
        tags$label(
          class = "form-label settings-label mb-1",
          bs_icon("list-ol"), " Pages to rotate"
        ),
        textInput(
          "rotate_pages", label = NULL,
          placeholder = "Leave blank for all pages"
        ),
        uiOutput("rotate_page_hint_ui")
      ),
      radioButtons(
        "rotate_angle",
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
        "rotate_run", "Rotate Pages",
        icon  = bs_icon("arrow-clockwise"),
        class = "btn-primary w-100"
      ),
      tags$hr(class = "my-1"),
      uiOutput("rotate_download_ui")
    ),
    result_card("rotate_result_ui")
  )
)

# ── Tab: Compact ───────────────────────────────────────────────────────────────

compact_tab <- nav_panel(
  title = tagList(bs_icon("file-zip"), " Compact"),
  value = "tab_compact",
  op_layout(
    col_widths = c(5, 7),
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",

      # PDF selector
      uiOutput("compact_pdf_select_ui"),

      # Tool selector — rendered server-side so only available tools appear.
      # Shows a warning card instead when neither gs nor qpdf is installed.
      uiOutput("compact_tool_ui"),

      # ── Ghostscript options (only visible when gs is selected) ──────────────
      conditionalPanel(
        condition = "input.compact_tool === 'gs'",
        tags$div(
          class = "border rounded-3 p-3 mt-1",
          style = "background: var(--bs-tertiary-bg);",

          tags$p(
            class = "small fw-semibold text-uppercase text-secondary mb-3",
            bs_icon("gear"), " Ghostscript options"
          ),

          # Quality preset
          selectInput(
            "gs_preset",
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
            "gs_compat",
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
            numericInput("gs_color_dpi", "Color (DPI)",
                         value = 150, min = 36, max = 2400, step = 1, width = "100%"),
            numericInput("gs_gray_dpi", "Gray (DPI)",
                         value = 150, min = 36, max = 2400, step = 1, width = "100%")
          ),
          layout_column_wrap(
            width = 1 / 2, fill = FALSE, gap = "0.5rem",
            numericInput("gs_mono_dpi", "Mono (DPI)",
                         value = 300, min = 36, max = 2400, step = 1, width = "100%"),
            selectInput(
              "gs_downsample", "Downsample method",
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
            input_switch("gs_embed_fonts",    "Embed all fonts", value = TRUE),
            input_switch("gs_subset_fonts",   "Subset fonts",    value = TRUE)
          ),
          input_switch("gs_compress_fonts", "Compress fonts", value = TRUE)
        )
      ),

      tags$hr(class = "my-1"),
      input_task_button(
        "compact_run", "Compact PDF",
        icon  = bs_icon("file-zip"),
        class = "btn-primary w-100"
      )
    ),
    result_card("compact_result_ui")
  )
)

# ── Root UI ────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = tagList(
    tags$span(bs_icon("file-pdf-fill"), class = "text-danger me-1"),
    tags$span("PDF Toolkit")
  ),
  theme = app_theme,
  navbar_options = navbar_options(
    bg = "#0f172a",
    inverse = TRUE
  ),
  sidebar = app_sidebar,
  header = tagList(add_external_resources()),

  overview_tab,
  split_tab,
  subset_tab,
  combine_tab,
  rotate_tab,
  compact_tab,

  nav_spacer(),
  nav_item(
    tooltip(
      input_dark_mode(id = "dark_mode", mode = "light"),
      "Toggle dark / light mode",
      placement = "bottom"
    )
  )
)

} # end build_ui()
