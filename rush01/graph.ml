let image_filename = "char-pikachu.png"
let dead_filename = "dead.png"
let progress_bar_filename = "progress_bar.png"
let font_filename  = "ProFont_For_Powerline.ttf"

let image () = Sdlloader.load_image image_filename
let dead_image () = Sdlloader.load_image dead_filename
let progress_bar () = Sdlloader.load_image progress_bar_filename

let font () = Sdlttf.open_font font_filename 24

let screen () = Sdlvideo.set_video_mode 1000 1000 []

type t = EAT | THUNDER | BATH | KILL | DEFAULT

type button = {
  group: t;
  rect: Sdlvideo.rect
}

type graph_elem = {
  image: Sdlvideo.surface;
  dead_image: Sdlvideo.surface;
  progress_bar: Sdlvideo.surface;
  screen: Sdlvideo.surface;
  font: Sdlttf.font
}

let check_button (button:Sdlvideo.rect) (mouse_event:Sdlevent.mousebutton_event) =
  mouse_event.mbe_x > button.r_x
  && mouse_event.mbe_x < (button.r_x + button.r_w)
  && mouse_event.mbe_y > button.r_y
  && mouse_event.mbe_y < (button.r_y + button.r_h)

let rec check_all_button (button_list: button list) mouse_event =
  match button_list with
  | [] -> DEFAULT
  | h::t -> if (check_button h.rect mouse_event) then h.group else check_all_button t mouse_event

let actions res tama = 
  match res with
  | DEFAULT -> tama
  | EAT -> tama#eat
  | THUNDER -> tama#thunder
  | BATH -> tama#bath
  | KILL -> tama#kill

let game_over g_elem =
  Sdlvideo.fill_rect g_elem.screen (Sdlvideo.map_RGB g_elem.screen (0, 0, 0));
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 50 197 478 606) ~src:g_elem.dead_image ~dst:g_elem.screen ();
  let f = Sdlttf.open_font font_filename 150 in
  let text_go = Sdlttf.render_text_blended f "GAME OVER !" ~fg:(165,42,42) in
  let text_ex = Sdlttf.render_text_blended g_elem.font "Press 'Escape' to exit" ~fg:Sdlvideo.white in
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 100 50 300 300) ~src:text_go ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 350 900 300 300) ~src:text_ex ~dst:g_elem.screen ();
  Sdlvideo.flip g_elem.screen;
  let rec game_over_loop () =
    match Sdlevent.wait_event () with 
    |  Sdlevent.KEYDOWN {Sdlevent.keysym = Sdlkey.KEY_ESCAPE} -> ()
    | QUIT -> ()
    | event -> game_over_loop () in
  game_over_loop ()

let save_tama tama g_elem = if tama#is_alive then tama#save else begin game_over g_elem; (new Pet.pet)#save end

let check_time tama save_time = 
  let new_time = Sdltimer.get_ticks () in
  if (new_time - save_time) >= 1000 then
    (new_time, tama#life)
  else
    (save_time, tama)

let draw tama g_elem =
  Sdlvideo.fill_rect g_elem.screen (Sdlvideo.map_RGB g_elem.screen (0, 0, 0));

  (* tama *)
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 261 197 478 606) ~src:g_elem.image ~dst:g_elem.screen ();

  (* Text of progress bar *)
  let text_health = Sdlttf.render_text_blended g_elem.font "HEALTH" ~fg:Sdlvideo.white in
  let text_ernergy = Sdlttf.render_text_blended g_elem.font "ENERGY" ~fg:Sdlvideo.white in
  let text_hygiene = Sdlttf.render_text_blended g_elem.font "HYGIENE" ~fg:Sdlvideo.white in
  let text_happy = Sdlttf.render_text_blended g_elem.font "HAPPY" ~fg:Sdlvideo.white in
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 190 25 300 300) ~src:text_health ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 390 25 300 300) ~src:text_ernergy ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 585 25 300 300) ~src:text_hygiene ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 800 25 300 300) ~src:text_happy ~dst:g_elem.screen ();

  (* Text in button*)
  let text_eat = Sdlttf.render_text_blended g_elem.font "EAT" ~fg:Sdlvideo.black in
  let text_thunder = Sdlttf.render_text_blended g_elem.font "THUNDER" ~fg:Sdlvideo.black in
  let text_bath = Sdlttf.render_text_blended g_elem.font "BATH" ~fg:Sdlvideo.black in
  let text_kill = Sdlttf.render_text_blended g_elem.font "KILL" ~fg:Sdlvideo.black in

  let eat = {group = EAT; rect = (Sdlvideo.rect 150 900 100 50)} in
  let thunder = {group = THUNDER; rect = (Sdlvideo.rect 350 900 100 50)} in
  let bath = {group = BATH; rect = (Sdlvideo.rect 550 900 100 50)} in
  let kill = {group = KILL; rect = (Sdlvideo.rect 750 900 100 50)} in
  let my_white = Sdlvideo.map_RGB g_elem.screen Sdlvideo.white in

  (* Progress bar *)
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 150 50 150 25) ~src:g_elem.progress_bar ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 350 50 150 25) ~src:g_elem.progress_bar ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 550 50 150 25) ~src:g_elem.progress_bar ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 750 50 150 25) ~src:g_elem.progress_bar ~dst:g_elem.screen ();

  let (h, e, hy, ha) = tama#get_values in
  let bar_health = Sdlvideo.rect 157 57 (int_of_float ((142.0 /. 100.0 ) *. (float_of_int h))) 19 in
  let bar_energy = Sdlvideo.rect 357 57 (int_of_float ((142.0 /. 100.0 ) *. (float_of_int e))) 19 in
  let bar_hygiene = Sdlvideo.rect 557 57 (int_of_float ((142.0 /. 100.0 ) *. (float_of_int hy))) 19 in
  let bar_happy = Sdlvideo.rect 757 57 (int_of_float ((142.0 /. 100.0 ) *. (float_of_int ha))) 19 in

  (* Print on screen *)
  (* Button *)
  Sdlvideo.fill_rect ~rect:eat.rect g_elem.screen my_white;
  Sdlvideo.fill_rect ~rect:thunder.rect g_elem.screen my_white;
  Sdlvideo.fill_rect ~rect:bath.rect g_elem.screen my_white;
  Sdlvideo.fill_rect ~rect:kill.rect g_elem.screen my_white;

  (* Text in button *)
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 185 915 300 300) ~src:text_eat ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 358 915 300 300) ~src:text_thunder ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 577 915 300 300) ~src:text_bath ~dst:g_elem.screen ();
  Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect 780 915 300 300) ~src:text_kill ~dst:g_elem.screen ();

  (* Progress bar*)
  let my_green = Sdlvideo.map_RGB g_elem.screen (0,100,0) in
  Sdlvideo.fill_rect ~rect:bar_health g_elem.screen my_green;
  Sdlvideo.fill_rect ~rect:bar_energy g_elem.screen my_green;
  Sdlvideo.fill_rect ~rect:bar_hygiene g_elem.screen my_green;
  Sdlvideo.fill_rect ~rect:bar_happy g_elem.screen my_green;

  Sdlvideo.flip g_elem.screen;
  [eat; thunder; bath; kill]


let rec wait_for_escape saved_time (button_list:button list) tama g_elem = 
  match Sdlevent.poll (), tama#is_alive with
  | _, false -> save_tama tama g_elem
  | None, _ -> Sdlvideo.flip g_elem.screen; 
    let (new_time, new_tama) = (check_time tama saved_time) in
    let nlst = if new_time <> saved_time then (draw new_tama g_elem) else button_list in
    wait_for_escape new_time nlst new_tama g_elem
  | Some k, _ -> (
      match k with
      | Sdlevent.KEYDOWN {Sdlevent.keysym = Sdlkey.KEY_ESCAPE}    -> save_tama tama g_elem
      | QUIT                                                      -> save_tama tama g_elem
      | Sdlevent.MOUSEBUTTONDOWN (mouse_event)
        when mouse_event.mbe_button = Sdlmouse.BUTTON_LEFT        ->
        let newtama = actions (check_all_button button_list mouse_event) tama in 
        let lst = draw newtama g_elem in  wait_for_escape saved_time lst newtama g_elem
      | event                                                     -> wait_for_escape saved_time button_list tama g_elem
    )

let run () =
  let tama = (new Pet.pet)#load in
  let g_elem = {image = (image ()); dead_image = (dead_image ()); progress_bar = (progress_bar ()); screen = (screen ()); font= (font ())} in
  wait_for_escape (Sdltimer.get_ticks ()) (draw tama g_elem) tama g_elem

let init () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  Sdlttf.init ();
  at_exit Sdlttf.quit
