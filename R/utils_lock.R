#' Ask the browser to lock or unlock a PDF
#' @param url Local or remote URL to a PDF. Usually, this should be
#'   a path to a PDF on a Shiny server.
#' @param input_id Name of the input the result will be delivered to.
#' @param id Identifier to echo back in the response. Defaults to `url`.
#' @param password Password to unlock the PDF.
#' @param userPassword Password to set for user permissions.
#' @param ownerPassword Password to set for owner permissions.
#' @param userProtectionFlag Permissions to set for user passwords.
#' @param session Shiny session.
#' @export
lock_pdf <- function(url,
                     input_id = "locked_pdf",
                     id = url,
                     password = NULL,
                     userPassword = NULL,
                     ownerPassword = NULL,
                     userProtectionFlag = 4,
                     session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "shinypdf-lock-pdf",
    list(
      id = id,
      url = url,
      inputId = session$ns(input_id),
      password = password,
      userPassword = userPassword,
      ownerPassword = ownerPassword,
      userProtectionFlag = userProtectionFlag
    )
  )
}


#' @rdname lock_pdf
#' @export
unlock_pdf <- function(url,
                       input_id = "locked_pdf",
                       id = url,
                       password = NULL,
                       session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "shinypdf-unlock-pdf",
    list(
      id = id,
      url = url,
      inputId = session$ns(input_id),
      password = password
    )
  )
}
