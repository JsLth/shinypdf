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
    useSweetAlert(),
    includeCSS(app_sys("www/styles.css")),
    tags$script(HTML("
      $(document).on('shiny:sessioninitialized', function() {
        // navigator.pdfViewerEnabled reports true in Electron-based hosts
        // (e.g. Positron's Viewer pane) even when the embedded webview's
        // native PDF plugin is actually disabled, so an <iframe> pointed at
        // a PDF silently fails to render there. Treat any Electron host as
        // unable to show inline PDFs and fall back to the thumbnail +
        // \"Open full PDF\" link, which opens in the system's real browser.
        var isElectron = /Electron/.test(navigator.userAgent);
        Shiny.setInputValue(
          'has_pdf_viewer',
          navigator.pdfViewerEnabled === true && !isElectron
        );
      });
      
      function isCommandInstalled(command, flag = '--version') {
        try {
            execSync(`${command} ${flag}`, { stdio: 'ignore' });
            return true;
        } catch (error) {
            return false;
        }
      }

      function pdfConfirmRemove(btn, name) {
        if (btn.dataset.confirming === '1') {
          clearTimeout(btn._confirmTimer);
          Shiny.setInputValue('remove_pdf', name, { priority: 'event' });
          return;
        }
        btn.dataset.confirming = '1';
        if (!btn.dataset.originalHtml) btn.dataset.originalHtml = btn.innerHTML;
        btn.innerHTML = 'Confirm?';
        btn.classList.remove('btn-outline-danger');
        btn.classList.add('btn-danger');
        btn._confirmTimer = setTimeout(function () {
          btn.dataset.confirming = '0';
          btn.innerHTML = btn.dataset.originalHtml;
          btn.classList.remove('btn-danger');
          btn.classList.add('btn-outline-danger');
        }, 3000);
      }
    "))
  )
}