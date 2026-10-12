// Create input value `has_pdf_viewer`, which tries to
// indicate whether the browser has a pdf viewer.
$(document).on('shiny:sessioninitialized', function() {
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

function pdfConfirmRemove(btn, name, inputId) {
    inputId = inputId || 'remove_pdf';
    if (btn.dataset.confirming === '1') {
        clearTimeout(btn._confirmTimer);
        Shiny.setInputValue(inputId, name, { priority: 'event' });
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


function togglePassword(id, iconId) {
    var input = document.getElementById(id);
    var iconContainer = document.getElementById(iconId);
    var iconShow = iconContainer.getElementsByClassName('bi-eye')[0]
    var iconHide = iconContainer.getElementsByClassName('bi-eye-slash')[0]

    if (input.type === 'password') {
        input.type = 'text';
        iconShow.style.display = 'none';
        iconHide.style.display = '';
    } else {
        input.type = 'password';
        iconShow.style.display = '';
        iconHide.style.display = 'none';
    }
}
