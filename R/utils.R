# ── Utility helpers ──────────────────────────────────────────────────────────

spdf_dir <- function() file.path(tempdir(), "shinypdf")
create_shinypdf_dir <- function() {
  pdir <- spdf_dir()
  dir.create(pdir, showWarnings = FALSE)
  dir.create(file.path(pdir, "uploads"), showWarnings = FALSE)
  dir.create(file.path(pdir, "split"), showWarnings = FALSE)
  dir.create(file.path(pdir, "subset"), showWarnings = FALSE)
  dir.create(file.path(pdir, "combine"), showWarnings = FALSE)
  dir.create(file.path(pdir, "rotate"), showWarnings = FALSE)
  dir.create(file.path(pdir, "compact"), showWarnings = FALSE)
  addResourcePath("uploads", file.path(pdir, "uploads"))
  addResourcePath("split", file.path(pdir, "split"))
  addResourcePath("subset", file.path(pdir, "subset"))
  addResourcePath("combine", file.path(pdir, "combine"))
  addResourcePath("rotate", file.path(pdir, "rotate"))
  addResourcePath("compact", file.path(pdir, "compact"))
}

tempfilename <- function(...) {
  basename(tempfile(...))
}

#' Format a byte count into a human-readable string
#' @param bytes Numeric. Size in bytes.
#' @return A character string, e.g. "1.4 MB".
format_size <- function(bytes) {
  if (is.null(bytes) || is.na(bytes) || bytes < 0) return("Unknown")
  if (bytes == 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  i     <- min(floor(log(bytes, 1024)), length(units) - 1L)
  value <- bytes / 1024^i
  paste0(round(value, if (value >= 10) 0L else 1L), "\u00a0", units[[i + 1L]])
}

#' Parse a page-range string into a sorted integer vector
#'
#' Accepts comma-separated values and dash-separated ranges, e.g. "1,3,5-8".
#'
#' @param pages_str Character string describing pages.
#' @param max_page  Upper bound; pages beyond this are silently dropped.
#' @return Integer vector of valid, unique, sorted page numbers.
parse_pages <- function(pages_str, max_page = Inf) {
  if (is.null(pages_str) || !nzchar(trimws(pages_str))) return(integer(0))

  parts <- unlist(strsplit(trimws(pages_str), "[,;\\s]+"))
  pages <- integer(0)

  for (part in parts) {
    part <- trimws(part)
    if (grepl("^\\d+$", part)) {
      pages <- c(pages, as.integer(part))
    } else if (grepl("^\\d+\\s*-\\s*\\d+$", part)) {
      bounds <- as.integer(trimws(strsplit(part, "-")[[1L]]))
      if (bounds[[1L]] <= bounds[[2L]]) {
        pages <- c(pages, seq.int(bounds[[1L]], bounds[[2L]]))
      }
    }
  }

  pages <- unique(sort(pages))
  pages[pages >= 1L & pages <= max_page]
}

#' Extract basic metadata for a PDF file
#' @param path Character. Path to a PDF file.
#' @return Named list with `pages` (integer) and `size` (numeric, bytes).
pdf_meta <- function(path) {
  list(
    pages = tryCatch(pdf_length(path), error = function(e) NA_integer_),
    size  = tryCatch(file.size(path),  error = function(e) NA_real_)
  )
}

#' Reusable empty-state UI element
#' @param icon     Bootstrap icon name string.
#' @param title    Primary message.
#' @param subtitle Supporting detail.
#' @return A `div` tag.
empty_state_ui <- function(
    icon     = "file-pdf",
    title    = "No PDFs loaded",
    subtitle = "Upload PDF files using the sidebar to get started.") {
  tags$div(
    class = "empty-state py-5 text-center text-secondary",
    tags$div(bs_icon(icon, size = "3em"), class = "mb-3 opacity-25"),
    tags$h6(title, class = "fw-semibold"),
    tags$p(subtitle, class = "small mb-0")
  )
}

#' Small info-badge helper
badge <- function(text, theme = "secondary") {
  tags$span(class = paste0("badge text-bg-", theme, " fw-normal"), text)
}

#' Find the Ghostscript executable path, or "" if not found
find_gs <- function() {
  candidates <- if (.Platform$OS.type == "windows") {
    c("gswin64c", "gswin32c", "gs")
  } else {
    c("gs")
  }
  for (cmd in candidates) {
    path <- Sys.which(cmd)
    if (nzchar(path)) return(unname(path))
  }
  ""
}

#' Find the qpdf executable path, or "" if not found
find_qpdf_bin <- function() {
  unname(Sys.which("qpdf"))
}

#' Generate a unique file name by appending a counter suffix if needed
#'
#' @param name     Character. Desired file name (including extension).
#' @param existing Character vector of names already in use.
#' @return A character string guaranteed not to appear in `existing`.
unique_name <- function(name, existing) {
  if (!(name %in% existing)) return(name)
  base <- tools::file_path_sans_ext(name)
  ext  <- tools::file_ext(name)
  ext  <- if (nzchar(ext)) paste0(".", ext) else ""
  i <- 1L
  repeat {
    candidate <- sprintf("%s (%d)%s", base, i, ext)
    if (!(candidate %in% existing)) return(candidate)
    i <- i + 1L
  }
}
