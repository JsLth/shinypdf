#' Extract basic metadata for a PDF file
#' @param path Character. Path to a PDF file.
#' @return Named list with `pages` (integer) and `size` (numeric, bytes).
pdf_meta <- function(path) {
  meta <- tryCatch(pdftools::pdf_info(path), error = function(e) list())
  size <- tryCatch(file.size(path),  error = function(e) NA_real_)
      
  list(
    pages = meta$pages,
    size = size,
    encrypted = meta$encrypted,
    locked = meta$locked,
    creator = meta$keys$Creator,
    producer = meta$keys$Producer,
    created = meta$created,
    modified = meta$modified,
    attachments = meta$attachments,
    linearized = meta$linearized
  )
}

#' Small icon badges flagging a PDF's encryption/lock/attachment status
#'
#' @param p List. One entry from the PDF store, as produced by `pdf_meta()`.
#' @return A `tagList` of zero or more tooltipped `bs_icon`s, for use in the
#'   overview card header.
pdf_flag_icons <- function(p) {
  flags <- list(
    list(
      cond = isTRUE(p$encrypted),
      icon = "shield-lock-fill",
      class = "text-warning",
      label = "Cryptographically protected"
    ),
    list(
      cond = isTRUE(p$locked),
      icon = "lock-fill",
      class = "text-danger",
      label = "Password protected"
    ),
    list(
      cond = isTRUE(p$attachments),
      icon = "paperclip",
      class = "text-secondary",
      label = "Has attachments"
    ),
    list(
      cond = isTRUE(p$linearized),
      icon = "speedometer",
      class = "text-primary",
      label = "Optimized for fast web view"
    )
  )

  tagList(lapply(Filter(function(f) f$cond, flags), function(f) {
    tooltip(
      bs_icon(f$icon, class = f$class),
      f$label
    )
  }))
}

#' Small metadata footer for an overview card (creator/producer/created date)
#'
#' @param p List. One entry from the PDF store, as produced by `pdf_meta()`.
#' @return A `tags$div`, or `NULL` if no such metadata is available.
pdf_meta_details <- function(p) {
  creator <- p$creator
  producer <- p$producer
  created <- format(p$created, "%Y-%m-%d")
  modified <- format(p$modified, "%Y-%m-%d")

  lines <- list()
  if (!is.null(creator) && nzchar(creator)) {
    lines <- c(lines, list(list(header = tagList(bs_icon("pencil"), tags$b("Creator: ")), value = creator)))
  }
  if (!is.null(producer) && nzchar(producer) && !identical(producer, creator)) {
    lines <- c(lines, list(list(header = tagList(bs_icon("gear"), tags$b("Producer: ")), value = producer)))
  }
  if (!is.null(created) && !is.na(created)) {
    lines <- c(lines, list(list(header = tagList(bs_icon("calendar3"), tags$b("Created: ")), value = created)))
  }
  if (!is.null(modified) && !is.na(modified) && !identical(modified, created)) {
    lines <- c(lines, list(list(header = tagList(bs_icon("calendar-plus"), tags$b("Modified: ")), value = modified)))
  }

  if (length(lines) == 0) return(NULL)

  tags$table(
    class = "small text-secondary border-top pt-2 mt-2",
    style = "width:70%; white-space:nowrap;",
    !!!lapply(lines, function(l) {
      tags$tr(
        tags$td(l$header),
        tags$td(class = "text-truncate", l$value)
      )
    })
  )
}
