mod_lock_ui <- function(id) {
  ns <- NS(id)
  op_layout(
    settings_card(
      header_label = "Settings",
      header_icon  = "sliders",
      uiOutput(ns("pdf_select_ui")),
      radioButtons(
        ns("type"),
        label = tagList(bs_icon("unlock"), " Lock or unlock?"),
        choices = c("Lock" = "lock", "Unlock" = "unlock"),
        selected = "lock"
      ),
      passwordInputToggle(
        "password",
        label = tagList(
          bs_icon("shield-lock"),
          " Password",
          popover(
            bs_icon("question-circle"),
            tags$p("Password required to unlock the PDF if it is locked.")
          )
        )
      ),
      tags$hr(class = "my-1"),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lock'", ns("type")),
        div(
          class = "border rounded-3 p-3 mt-1",
          style = "background: var(--bs-tertiary-bg);",

          tags$p(
            class = "small fw-semibold text-uppercase text-secondary mb-3",
            bs_icon("gear"), " Locking options"
          ),

          passwordInputToggle(
            "upw",
            label = span(
              bs_icon("key"),
              " User password",
              popover(
                bs_icon("question-circle"),
                "Password for users of the document. You can control what
                users can do by setting permissions below. By default,
                users can only view and print a document."
              )
            )
          ),
          passwordInputToggle(
            "opw",
            label = span(
              bs_icon("key"),
              " Owner password",
              popover(
                bs_icon("question-circle"),
                "Password for owners of the document. Owners have all permissions
                to view and modify a document."
              )
            )
          ),
          selectInput(
            "permissions",
            label = tagList(
              bs_icon("shield-exclamation"),
              "User permissions",
              popover(
                bs_icon("question-circle"),
                "File permissions that apply when the user password is entered.
                This list is ranked in increasing order, that is, users have no
                permissions to do any of the actions below the selected one."
              )
            ),
            choices = c(
              "Printing" = 4,
              "Modify contents" = 8,
              "Extract text and graphics" = 16,
              "Modify annotations and comments" = 32,
              "Fill in form fields" = 256,
              "Extract for screen readers" = 512,
              "Assemble (insert / rotate / delete)" = 1024,
              "High-resolution printing" = 2048
            )
          )
        )
      ),
      tags$hr(class = "my-1"),
      div(
        id = ns("run_tip"),
        input_task_button(
          ns("run"), "Lock / unlock",
          icon  = bs_icon("unlock"),
          class = "btn-primary w-100"
        )
      ),
      tags$hr(class = "my-1"),
      uiOutput(ns("download_ui"))
    ),
    result_card(ns("result_ui"))
  )
}

mod_lock_server <- function(id, pdf_rv, pdf_names, upload_event, has_pdf_viewer) {
  moduleServer(id, function(input, output, session) {
    output$pdf_select_ui <- pdf_select_renderer(session, pdf_names, "pdf_select", "Select PDF")
    output$page_hint_ui  <- page_hint_renderer(input, pdf_rv, "pdf_select")

    output$download_ui <- renderUI({
      res <- input$locked_pdf
      if (!is.null(res)) {
        tags$div(
          class = "result-success text-center",
          bs_icon("check-circle-fill", class = "text-success"),
          tags$h6(
            class = "mt-2 mb-0 fw-semibold",
            switch(
              input$type,
              lock = "Locked document",
              unlock = "Unlocked document"
            )
          ),
          tags$p(class = "small text-secondary mb-3", basename(res$path)),
          downloadButton(
            session$ns("download"),
            "Download PDF",
            icon = bs_icon("download"),
            class = "btn-primary"
          )
        )
      }
    })

    output$result_ui <- renderUI({
      res <- input$locked_pdf
      if (is.null(res)) {
        empty_state_ui(
          "shield-lock",
          sprintf("Ready to %s", input$type),
          "Choose pages and angle, then click \u201cLock / unlock\u201d."
        )
      } else {
        pdf_display(
          res$path,
          url = file.path("lock", basename(res$path)),
          pages = res$n_pages,
          download = FALSE,
          has_pdf_viewer = has_pdf_viewer()
        )
      }
    })

    observe({
      on.exit(update_task_button(
        session = session,
        "run",
        state = "ready"
      ))
      req(input$pdf_select)
      p <- pdf_rv()[[input$pdf_select]]
      req(!is.null(p))

      out <- file.path(spdf_dir(), "lock", tempfilename(fileext = ".pdf"))
      tryCatch({
        switch(
          input$type,
          unlock = unlock_pdf(p$path, password = input$password),
          lock = lock_pdf(
            p$path,
            password = input$password,
            userPassword = input$upw,
            ownerPassword = input$opw,
            userProtectionFlag = input$permissions
          )
        )
      }, error = function(e) {
        showNotification(
          sprintf("%s failed: %s", to_title(input$type), conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    }) |>
      bindEvent(input$run)

    output$download <- downloadHandler(
      filename = function() {
        sprintf(
          "%sed_%s.pdf",
          input$type,
          file_path_sans_ext(basename(input$locked_pdf$path))
        )
      },
      content = function(file) file.copy(input$locked_pdf$path, file)
    )
  })
}
