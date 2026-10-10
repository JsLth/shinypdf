#' Ask the browser to pull embedded raster images out of a PDF
#'
#' Extraction happens client-side via pdf.js (see
#' `inst/www/pdfjs-extract.mjs`/`pdfjs-extract-bridge.js`) so the large PDF
#' never has to round-trip through the server as a payload — only a URL is
#' sent down, and only the resulting (much smaller) image data URLs come
#' back. The call is fire-and-forget; the result arrives asynchronously as
#' `input[[input_id]]`, a list with:
#'   - `id`: echoes the `id` argument, to match responses to requests
#'   - `images`: list of `list(page, index, objId, width, height, dataUrl)`
#'   - `error`: `NULL` on success, otherwise a message string
#'
#' @param url URL to the PDF, already servable via `addResourcePath()`
#'   (e.g. `"uploads/<basename>.pdf"`, matching `create_shinypdf_dir()`).
#' @param input_id Name of the input the result will be delivered to.
#' @param id Identifier to echo back in the response. Defaults to `url`.
#' @param pages Page numbers from which to extract images. By default, 
#'   all pages are extracted.
#' @param min_width Minimum width of an image to be extracted in pixels.
#'   Defaults to 2.
#' @param min_height Minimum height of an image to be extracted in pixels.
#'   Defaults to 2.
#' @param inline Logical scalar specifying whether to extract small inline
#'   images. Defaults to `FALSE`.
#' @param session Shiny session.
extract_pdf_images <- function(url,
                               input_id = "extracted_images",
                               id = url,
                               format = "png",
                               quality = 1,
                               pages = NULL,
                               min_width = 2,
                               min_height = 2,
                               inline = FALSE,
                               dedupe = FALSE,
                               session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "shinypdf-extract-images",
    list(
      id = id,
      url = url,
      inputId = session$ns(input_id),
      format = format,
      quality = quality,
      pages = pages %||% list(),
      min_width = min_width,
      min_height = min_height,
      inline = inline,
      dedupe = dedupe
    )
  )
}