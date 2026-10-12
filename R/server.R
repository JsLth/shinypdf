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

  mod_lock_server(
    "lock",
    pdf_rv = sb$pdf_rv, pdf_names = sb$pdf_names, upload_event = sb$upload_event
  )

  toggle_run_button(input, "split", pdf_rv = sb$pdf_rv)
  toggle_run_button(input, "subset", pdf_rv = sb$pdf_rv)
  # Combine has no `pdf_select` input (it uses a rank list of all PDFs), so it
  # needs its own validity check: at least 2 PDFs loaded.
  toggle_run_button(
    input, "combine", pdf_rv = sb$pdf_rv,
    valid = reactive(length(sb$pdf_rv()) >= 2)
  )
  toggle_run_button(input, "rotate", pdf_rv = sb$pdf_rv)
  toggle_run_button(input, "compact", pdf_rv = sb$pdf_rv)
  toggle_run_button(input, "extract", pdf_rv = sb$pdf_rv)
  toggle_run_button(input, "lock", pdf_rv = sb$pdf_rv)
}
