$(document).ready( function() {
	var activate = function(hash){
		$('#navigation a.active').removeClass('active');
        $('#navigation a[href="' + hash + '"]').addClass('active');

        $('#content .active').removeClass('active');
        $('#content ' + hash).addClass('active');
	};

    smoothScroll.init({
    	offset: 10,
    	easing: "easeInOutCubic",
    	callbackBefore: function (toggle, anchor) {
    		activate(window.location.hash);
    	}
    });
    if (window.location.hash == "" || window.location.hash == "#") {
        activate('#title');
    }
    else {
        activate(window.location.hash);
    }
    $('em:contains("git-vogue")').addClass('git-vogue-branding');
    $('em:contains("Anchor")').addClass('anchor-branding');
});