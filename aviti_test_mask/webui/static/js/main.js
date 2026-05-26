/**
 * Global utilities for VIB Nucleomics Flask webapps.
 * Loaded on every page via base.html.
 */

function logout() {
    $.ajax({
        url: '/logout',
        method: 'POST',
        success: function() { window.location.href = '/login'; },
        error:   function() { alert('Logout failed. Please try again.'); }
    });
}

function showSuccess(message) {
    const html = `<div class="alert alert-success alert-dismissible fade show position-fixed top-0 end-0 m-3" role="alert" style="z-index:9999;">
        ${message}
        <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
    </div>`;
    $('body').append(html);
    setTimeout(() => $('.alert').alert('close'), 3000);
}

function showError(message) {
    const html = `<div class="alert alert-danger alert-dismissible fade show position-fixed top-0 end-0 m-3" role="alert" style="z-index:9999;">
        ${message}
        <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
    </div>`;
    $('body').append(html);
    setTimeout(() => $('.alert').alert('close'), 5000);
}

function formatDateTime(dateString) {
    if (!dateString) return '-';
    return new Date(dateString).toLocaleString();
}

function formatDate(dateString) {
    if (!dateString) return '-';
    return new Date(dateString).toLocaleDateString();
}

function confirmAction(message, callback) {
    if (confirm(message)) callback();
}

// Redirect to login on 401; show permission error on 403
$(document).ajaxError(function(event, jqxhr) {
    if (jqxhr.status === 401) {
        window.location.href = '/login';
    } else if (jqxhr.status === 403) {
        showError('You do not have permission to perform this action.');
    }
});

// DataTables defaults
$.extend(true, $.fn.dataTable.defaults, {
    pageLength: 25,
    lengthMenu: [[10, 25, 50, 100, -1], [10, 25, 50, 100, 'All']],
    dom: '<"row"<"col-sm-12 col-md-6"l><"col-sm-12 col-md-6"f>>' +
         '<"row"<"col-sm-12"tr>>' +
         '<"row"<"col-sm-12 col-md-5"i><"col-sm-12 col-md-7"p>>',
    language: {
        search: 'Search:',
        lengthMenu: 'Show _MENU_ entries',
        info: 'Showing _START_ to _END_ of _TOTAL_ entries',
        infoEmpty: 'No entries to show',
        infoFiltered: '(filtered from _MAX_ total entries)',
        emptyTable: 'No data available',
        zeroRecords: 'No matching records found'
    }
});
