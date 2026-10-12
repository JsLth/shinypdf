add_tooltip <- function(id, content, placement = "bottom", session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("add-tooltip", list(
      id = id,
      content = content,
      placement = placement
    ))
}


remove_tooltip <- function(id, session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("remove-tooltip", list(id = id))
}