Shiny.addCustomMessageHandler('add-tooltip', function(msg) {
    const el = document.getElementById(msg.id);
    if (!el || !window.bootstrap) return;

    const existing = bootstrap.Tooltip.getInstance(el);
    if (existing) existing.dispose();

    new bootstrap.Tooltip(el, {
        title: msg.content,
        trigger: 'hover',
        placement: msg.placement || 'bottom',
        container: 'body'
    });
});


Shiny.addCustomMessageHandler('remove-tooltip', function(msg) {
    const el = document.getElementById(msg.id);
    if (!el || !window.bootstrap) return;

    const existing = bootstrap.Tooltip.getInstance(el);
    if (existing) existing.dispose();
});