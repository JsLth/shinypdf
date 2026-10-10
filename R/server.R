# ── App server ─────────────────────────────────────────────────────────────
# Wires the sidebar module's shared reactives (the master PDF store, plus
# convenience views over it) into each tab module. `has_pdf_viewer` is set by
# client-side JS at the document/root level (see add_external_resources() in
# R/config.R), so it's read here on the root session and threaded down to
# whichever modules need it, rather than being module-scoped itself.

server <- function(input, output, session) {
  create_shinypdf_dir()

  has_pdf_viewer <- reactive(input$has_pdf_viewer)

  sb <- mod_sidebar_server("sidebar", has_pdf_viewer = has_pdf_viewer)

  mod_overview_server("overview", pdf_rv = sb$pdf_rv, ordered_pdfs = sb$ordered_pdfs)

  mod_split_server(
    "split",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event
  )

  mod_subset_server(
    "subset",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event,
    has_pdf_viewer = has_pdf_viewer
  )

  mod_combine_server(
    "combine",
    pdf_rv = sb$pdf_rv, ordered_pdfs = sb$ordered_pdfs, pdf_names = sb$pdf_names,
    upload_event = sb$upload_event, has_pdf_viewer = has_pdf_viewer
  )

  mod_rotate_server(
    "rotate",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event,
    has_pdf_viewer = has_pdf_viewer
  )

  mod_compact_server(
    "compact",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event
  )

  mod_extract_server(
    "extract",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event
  )
}
