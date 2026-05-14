function resizeFrame() {
    if (window.frameElement) {
        var activeTab = document.querySelector('.tab-pane.active');
        var navbar = document.querySelector('.navbar');
        var navHeight = navbar ? navbar.offsetHeight : 0;
        var h = activeTab ? activeTab.scrollHeight + navHeight + 20 : document.body.scrollHeight;
        
        window.frameElement.style.height = h + 'px';
        window.frameElement.style.overflow = 'hidden';
    }
    document.documentElement.style.overflow = 'hidden';
    document.body.style.overflow = 'hidden';
}

// On tab click
document.addEventListener('click', function(e) {
    var tab = e.target.closest('a[data-bs-toggle=\"tab\"], a[data-toggle=\"tab\"]');
    if (tab) {
        setTimeout(resizeFrame, 400);
    }
});

// When any Shiny output finishes rendering, resize
$(document).on('shiny:value shiny:outputinvalidated', function() {
    setTimeout(resizeFrame, 300);
});

// Watch for image/plot loads specifically
$(document).on('shiny:idle', function() {
    setTimeout(resizeFrame, 300);
});

setTimeout(resizeFrame, 2000);