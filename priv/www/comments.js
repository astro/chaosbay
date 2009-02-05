function getName() {
  var d1 = document.URL.indexOf("://");
  var l = document.URL.substring(d1 + 3, document.URL.length);
  var d2 = l.indexOf("/");
  return l.substring(d2 + 1, l.length);
}

function reloadComments(text) {
  $("#spinner").show("fast");
  $("#comment-data").hide("normal");
  if (text)
    jQuery.post("/comments/" + getName(), {text: text}, updateComments, "html");
  else
    jQuery.get("/comments/" + getName(), {}, updateComments, "html");
}

function submitComment(text) {
  var text = $("#comment-text").val();
  reloadComments(text);
}

function updateComments(data, textStatus) {
  if (textStatus == "success") {
    $("#comment-data").hide("fast");
    $("#comment-data").empty();
    $("#comment-data").append(data);
    $("#comment-data").show("slow");
    $("#spinner").hide("fast");
  }
}

$(document).ready(function() {
		    if ($("#comments").length)
		    {
		      $("#comments").addClass("important");
		      $("#comments").empty();
		      $("#comments").append("<img id='spinner' src='/static/spinner.gif'/>");
		      $("#comments").append("<div id='comment-data'></div>");
		      reloadComments();
		    }
		  });