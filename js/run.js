$(document).ready( function() {
	var activate = function(hash){
		$('#navigation a.active').removeClass('active');
        $('#navigation a[href="' + hash + '"]').addClass('active');

        $('#content .active').removeClass('active');
        $('#content ' + hash).addClass('active');
	};

    var makeSmoothScroll = function(){
        var offset = 25;
        if (window.innerWidth >= 960) {
            offset = 35;
        }
        console.log(offset);

        smoothScroll.init({
            offset: offset,
            easing: "easeInOutCubic",
            callbackBefore: function (toggle, anchor) {
                activate(window.location.hash);
            }
        });
    };

    makeSmoothScroll();

    $(window).resize(function(){
        makeSmoothScroll();        
    });
    
    if (window.location.hash == "" || window.location.hash == "#") {
        activate('#content');
    }
    else {
        activate(window.location.hash);
    }
    $('em:contains("git-vogue")').addClass('git-vogue-branding');
    $('em:contains("Anchor")').addClass('anchor-branding');
});