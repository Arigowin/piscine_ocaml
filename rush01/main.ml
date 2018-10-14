
let main () = 
  if Sys.file_exists Graph.image_filename
  && Sys.file_exists Graph.dead_filename
  && Sys.file_exists Graph.progress_bar_filename
  && Sys.file_exists Graph.font_filename
  then
    begin
      Graph.init ();
      Graph.run ()
    end
  else
    print_endline "Unable to open image files or image files are missing"

let _ = main ()
