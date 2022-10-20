$(function() {

  "use strict";

  // key events

  $("#signed_in_new_color").keyup(function(event) {
    console.log('boom!');
    if (event.keyCode == 13) $("#signed_in_new_color_button").click();
  });

  $("#signed_in_new_password").keyup(function(event) {
    console.log('boom2!');
    if (event.keyCode == 13) $("#signed_in_new_password_button").click();
  });

  $("#signed_in_verify_new_password").keyup(function(event) {
    console.log('boom3!');
    if (event.keyCode == 13) $("#signed_in_new_password_button").click();
  });

});
