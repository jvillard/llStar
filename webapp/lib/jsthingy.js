var editor = new Array();

$(document).ready(function() {
    // tabs
    $("#input-tabs").tabs({
	activate: function(event, tabindex) {
	    $(".codeide").each( function(i) {
		editor[i].refresh();
	    });
	}
    });

    // set each editor
    $(".codeide").each( function(i) {
	editor[i] = CodeMirror.fromTextArea(this, {
	    lineNumbers: true
	});
    });
    
    $("#runllStar").click(function(){
	$("#runllStarimg").attr("src", "../llstar_anim.gif");
    });
    
    $(window).bind("load", function() {
	$("#runllStarimg").attr("src", "../llstar0.png");
    });
});
