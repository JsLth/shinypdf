# ── Utility helpers ──────────────────────────────────────────────────────────

# Null-coalescing operator (base R >= 4.4 has it, provide fallback)
`%||%` <- function(x, y) if (is.null(x)) y else x

spdf_dir <- function() file.path(tempdir(), "shinypdf")
create_shinypdf_dir <- function() {
  pdir <- spdf_dir()
  dir.create(pdir, showWarnings = FALSE)
  dir.create(file.path(pdir, "uploads"), showWarnings = FALSE)
  dir.create(file.path(pdir, "thumb"), showWarnings = FALSE)
  dir.create(file.path(pdir, "split"), showWarnings = FALSE)
  dir.create(file.path(pdir, "subset"), showWarnings = FALSE)
  dir.create(file.path(pdir, "combine"), showWarnings = FALSE)
  dir.create(file.path(pdir, "rotate"), showWarnings = FALSE)
  dir.create(file.path(pdir, "compact"), showWarnings = FALSE)
  addResourcePath("uploads", file.path(pdir, "uploads"))
  addResourcePath("thumb", file.path(pdir, "thumb"))
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
#' @noRd
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
#' @noRd
find_qpdf_bin <- function() {
  unname(Sys.which("qpdf"))
}

#' Generate a unique file name by appending a counter suffix if needed
#'
#' @param name Character. Desired file name (including extension).
#' @param existing Character vector of names already in use.
#' @return A character string guaranteed not to appear in `existing`.
#' @noRd
unique_name <- function(name, existing) {
  if (!(name %in% existing)) return(name)
  base <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  ext <- if (nzchar(ext)) paste0(".", ext) else ""
  rgx <- "\\(([0-9])+\\)$"
  if (grepl(rgx, base)) {
    i <- strtoi(regex_match(base, rgx, i = 2))
    base <- trimws(gsub(rgx, "", base))
    repeat {
      i <- i + 1
      name <- sprintf("%s (%d)%s", base, i, ext)
      if (!name %in% existing) return(name)
    }
  } else {
    i <- 1
    unique_name(sprintf("%s (%d)%s", base, i, ext), existing)
  }
}


regex_match <- function (text, pattern, i = NULL, ...) {
  match <- regmatches(text, regexec(pattern, text, ...))
  if (!is.null(i)) {
    match <- vapply(match, FUN.VALUE = character(1), function(x) {
      if (length(x) >= i) {
        x[[i]]
      } else {
        NA_character_
      }
    })
  }
  match
}


#' Render the first page of a PDF file as an image file
#' 
#' @param path Path to the PDF file
#' @param out_path Destination path of the rendered image file
#' @param dpi Rendering resolution
#' @returns The output path, invisibly.
#' @noRd
pdf_thumbnail <- function(path, out_path, dpi = 110) {
  tryCatch({
    pdftools::pdf_convert(
      path,
      format = "png",
      pages = 1,
      filenames = out_path,
      dpi = dpi,
      verbose = FALSE
    )
    invisible(out_path)
  }, error = function(e) NULL)
}


thumb_cache <- new.env(parent = emptyenv())
get_pdf_thumbnail <- function(path) {
  cached <- thumb_cache[[path]]
  if (!is.null(cached) && file.exists(cached)) {
    return(cached)
  }

  out <- file.path(
    spdf_dir(),
    "thumb",
    paste0(tempfilename(), ".png")
  )

  result <- pdf_thumbnail(path, out)
  if (!is.null(result)) {
    thumb_cache[[path]] <- result
  }

  result
}


# `path` is the on-disk filesystem path (used to generate the thumbnail and
# read the file size); `url` is the matching Shiny resource-relative URL
# already registered via addResourcePath() in create_shinypdf_dir(), e.g.
# "uploads/foo.pdf" or "subset/bar.pdf".
#
# Note: Chromium-based embedded webviews (Electron, Positron) frequently
# kill <iframe src="data:..."> navigations outright, surfacing as a generic
# chrome-error://chromewebdata/ page. A same-origin resource URL is a normal
# network request and doesn't hit that restriction, so don't swap this back
# to a base64 data: URI.
pdf_display <- function(path, url, pages = NULL, download = FALSE, has_pdf_viewer = FALSE) {
  if (has_pdf_viewer) {
    tags$iframe(
      src = url,
      class = "pdf-viewer",
      type = "application/pdf"
    )
  } else {
    thumb_path <- get_pdf_thumbnail(path)
    
    if (!is.null(thumb_path)) {
      image <- tags$img(
        src = file.path("thumb", basename(thumb_path)),
        class = "thumbnail"
      )
    } else {
      image <- tags$p(
        class = "text-secondary small py-5 mb-0",
        "Preview unavailable."
      )
    }

    tags$div(
      class = "text-center",
      image,
      tags$p(
        class = "small text-secondary mt-3 mb-3",
        if (!is.null(pages) && !is.na(pages) && pages > 1) {
          sprintf("Showing page 1 of %d \u2014 ", pages)
        },
        format_size(file.info(path)$size)
      ),
      if (download) {
        tags$a(
          href = url,
          target = "_blank",
          class = "btn btn-primary",
          bs_icon("box-arrow-up-right"),
          " Open full PDF"
        )
      }
    )
  }
}