$(document).ready(function(){
	var tabN = -1;
	//alert($(".bsch [class^='row-']").html());
	$(".bsch [class^='row-']").click(function(){
		$(".bsch .rowbg").removeClass("hilite-bg").addClass("palebg");
		$(this).find(".rowbg").removeClass("palebg").addClass("hilite-bg");
		var i=this.className.match(/row-(\d+)/)[1];
		$(".bsch [class^='tab-']").hide();
		$(".bsch .sub-panel").show();
		$(".bsch .tab-"+i).show();
	});
});
