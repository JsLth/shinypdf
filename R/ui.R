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

# ── Root UI ────────────────────────────────────────────────────────────────────
# Each tab's body is rendered by its own module (R/mod_*.R); this file is just
# responsible for navigation chrome (titles, icons, nav values) and theming.

page_navbar(
  title = tagList(
    tags$span(bs_icon("file-pdf-fill"), class = "text-danger me-1"),
    tags$span("PDF Toolkit")
  ),
  theme = app_theme,
  navbar_options = navbar_options(
    bg = "#0f172a",
    inverse = TRUE
  ),
  sidebar = mod_sidebar_ui("sidebar"),
  header = tagList(add_external_resources()),

  nav_panel(
    title = tagList(bs_icon("grid-1x2"), " Overview"),
    value = "tab_overview",
    mod_overview_ui("overview")
  ),
  nav_panel(
    title = tagList(bs_icon("scissors"), " Split"),
    value = "tab_split",
    mod_split_ui("split")
  ),
  nav_panel(
    title = tagList(bs_icon("collection"), " Subset"),
    value = "tab_subset",
    mod_subset_ui("subset")
  ),
  nav_panel(
    title = tagList(bs_icon("union"), " Combine"),
    value = "tab_combine",
    mod_combine_ui("combine")
  ),
  nav_panel(
    title = tagList(bs_icon("arrow-clockwise"), " Rotate"),
    value = "tab_rotate",
    mod_rotate_ui("rotate")
  ),
  nav_panel(
    title = tagList(bs_icon("file-zip"), " Compact"),
    value = "tab_compact",
    mod_compact_ui("compact")
  ),
  nav_panel(
    title = tagList(bs_icon("box-arrow-up"), " Extract"),
    value = "tab_extract",
    mod_extract_ui("extract")
  ),

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
