$(document).ready(function() {

  if ($('header').hasClass('fixed-header')) {
    $('.container-fluid').css("transition", "none");
    $('.container-fluid').css("padding-top", "50px");
  } else {
    $(window).scroll(function(event) {
    	var scroll = $(window).scrollTop();
    	if (scroll > 50) {
    		$('header').addClass('fixed-header');
    		$('.container-fluid').css("padding-top", "80px");
    	} else {
    		$('header').removeClass('fixed-header');
    		$('.container-fluid').css("padding-top", "122px");
    	}
    });
  }

  $(".toggle_fullscreen_menu").click(function() {
    var $menu_container = $('.slide_in_menu_container'),
      top_bar_height = $menu_container.find('.slide_menu_top').innerHeight();

    $menu_container.toggleClass('fullscreen_menu_opened');
    $('body').toggleClass('fullscreen_menu_active');

    // adjust the padding in fullscreen menu
    $menu_container.css({
      'padding-top': top_bar_height + 20
    });
  });

  $(".menu-item-has-children").children("a").click(function() {
    var $this_parent = $(this).closest('li'),
      $this_arrow = $this_parent.find('>a .mobile_menu_arrow'),
      $closest_submenu = $this_parent.find('>ul'),
      is_opened_submenu = $this_arrow.hasClass('submenu_opened'),
      sub_menu_max_height;

    $this_arrow.toggleClass('submenu_opened');
    if (is_opened_submenu) {
      $closest_submenu.removeClass('slide_dropdown_opened');
      $closest_submenu.slideToggle(700, 'easeInOutCubic');
    } else {
      $closest_submenu.slideToggle(700, 'easeInOutCubic');
      $closest_submenu.addClass('slide_dropdown_opened');
    }
    return false;
  });

  var f = document.getElementsByClassName("alwaysOnBotton");
  if (f !== null && f.length > 0) {
    $(".container-fluid").css("padding-bottom", f[0].clientHeight);
  }
});
