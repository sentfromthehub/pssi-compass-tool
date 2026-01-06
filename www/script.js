$(document).ready(function() {
    $('body').on('click', 'a.disabled-tab', function(e) {
        e.preventDefault();
        e.stopImmediatePropagation();
        return false;
    });
});