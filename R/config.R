app_sys <- function(...) {
  path <- system.file(..., package = "shinypdf")
  
  if (!nchar(path)) {
    path <- file.path(...)
  }
  
  path
}


add_external_resources <- function() {
  addResourcePath("www", app_sys("www"))

  tags$head(
    tags$link(rel = "shortcut icon", href = "www/favicon.ico"),
    shinyjs::useShinyjs(),
    useSweetAlert(),
    includeCSS(app_sys("www/styles.css")),
    tags$script(type = "module", src = "www/pdfjs-extract.mjs"),
    tags$script(src = "www/pdfjs-extract-bridge.js"),
    tags$script(src = "www/tooltips.js"),
    tags$script(src = "www/utils.js")
  )
}


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
  dir.create(file.path(pdir, "lock"), showWarnings = FALSE)
  addResourcePath("uploads", file.path(pdir, "uploads"))
  addResourcePath("thumb", file.path(pdir, "thumb"))
  addResourcePath("split", file.path(pdir, "split"))
  addResourcePath("subset", file.path(pdir, "subset"))
  addResourcePath("combine", file.path(pdir, "combine"))
  addResourcePath("rotate", file.path(pdir, "rotate"))
  addResourcePath("compact", file.path(pdir, "compact"))
  addResourcePath("lock", file.path(pdir, "lock"))
}