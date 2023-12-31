let time f_name f arg =
    let t0 = Unix.gettimeofday () in
    let result = f arg in
    let t1 = Unix.gettimeofday () in
    let open Format in
    let _ = printf "%.3f ms <- %s\n" ((t1 -. t0) *. 1000.) f_name in
    result

exception Unreachable of string

let bytes_for_bit a = (a + 7) / 8

(* read file with filename and return string *)
let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    String.to_bytes s

(* write whole string to the file with filename *)
let write_whole_file filename bytes =
    let ch = open_out_bin filename in
    output_bytes ch bytes;
    close_out ch


let lorem_ipsum = "
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ac ut consequat semper viverra nam libero justo laoreet sit. Ante in nibh mauris cursus. Quam viverra orci sagittis eu volutpat odio facilisis mauris sit. Dui vivamus arcu felis bibendum ut tristique. Vitae auctor eu augue ut lectus arcu bibendum. Duis at consectetur lorem donec massa sapien faucibus et molestie. Ac tincidunt vitae semper quis lectus nulla at volutpat. Tempus egestas sed sed risus pretium quam vulputate. Luctus venenatis lectus magna fringilla urna porttitor. Sollicitudin nibh sit amet commodo. Facilisis mauris sit amet massa vitae tortor condimentum lacinia quis. Dolor sit amet consectetur adipiscing. Libero id faucibus nisl tincidunt eget. Auctor urna nunc id cursus metus aliquam eleifend mi in. Massa massa ultricies mi quis hendrerit dolor magna eget. Sed egestas egestas fringilla phasellus faucibus scelerisque eleifend donec pretium. Risus in hendrerit gravida rutrum quisque. Sed vulputate mi sit amet mauris commodo quis imperdiet massa. Ut lectus arcu bibendum at varius vel pharetra vel.

Scelerisque in dictum non consectetur a erat. Commodo quis imperdiet massa tincidunt nunc pulvinar sapien et ligula. Ultricies tristique nulla aliquet enim tortor at auctor urna nunc. Arcu non odio euismod lacinia at quis risus sed vulputate. Fermentum et sollicitudin ac orci phasellus egestas. Eu sem integer vitae justo eget. Pharetra et ultrices neque ornare aenean euismod elementum. Egestas egestas fringilla phasellus faucibus. Scelerisque purus semper eget duis at tellus at urna condimentum. Ut etiam sit amet nisl. Consectetur a erat nam at. Lectus arcu bibendum at varius. At tempor commodo ullamcorper a lacus vestibulum. At imperdiet dui accumsan sit amet nulla facilisi. Sit amet massa vitae tortor condimentum lacinia quis vel.

Dictum fusce ut placerat orci nulla pellentesque dignissim enim. Massa tincidunt dui ut ornare lectus. Habitasse platea dictumst vestibulum rhoncus est pellentesque. Curabitur vitae nunc sed velit dignissim sodales ut. Vel risus commodo viverra maecenas accumsan. Pharetra et ultrices neque ornare aenean euismod. Varius sit amet mattis vulputate. Dui sapien eget mi proin sed libero enim. Tristique sollicitudin nibh sit amet commodo nulla. Eu consequat ac felis donec et odio pellentesque.

Sed faucibus turpis in eu mi bibendum. Neque laoreet suspendisse interdum consectetur libero. Eget nulla facilisi etiam dignissim diam quis enim lobortis scelerisque. Lacus sed turpis tincidunt id aliquet risus feugiat in. Nibh sit amet commodo nulla facilisi nullam vehicula. Donec ultrices tincidunt arcu non sodales. Et ultrices neque ornare aenean euismod. Orci eu lobortis elementum nibh tellus. Hac habitasse platea dictumst vestibulum rhoncus est pellentesque elit ullamcorper. Pellentesque nec nam aliquam sem et. Vitae aliquet nec ullamcorper sit. Ac felis donec et odio pellentesque. Ornare lectus sit amet est.

Volutpat consequat mauris nunc congue nisi. Netus et malesuada fames ac turpis. At urna condimentum mattis pellentesque id nibh tortor id aliquet. A lacus vestibulum sed arcu non odio euismod lacinia. Tellus cras adipiscing enim eu turpis egestas pretium aenean. Integer enim neque volutpat ac tincidunt vitae semper quis lectus. Nisl tincidunt eget nullam non nisi. Viverra suspendisse potenti nullam ac tortor vitae purus faucibus. Urna molestie at elementum eu. Nisi scelerisque eu ultrices vitae auctor eu augue. Neque egestas congue quisque egestas diam in arcu cursus euismod. Vitae congue eu consequat ac. Auctor eu augue ut lectus arcu bibendum. A lacus vestibulum sed arcu non. Egestas quis ipsum suspendisse ultrices gravida.

Tincidunt eget nullam non nisi est sit amet facilisis magna. Placerat orci nulla pellentesque dignissim enim sit. Integer quis auctor elit sed. Maecenas pharetra convallis posuere morbi leo urna molestie at. Eget duis at tellus at urna condimentum mattis. Diam sit amet nisl suscipit. Quis ipsum suspendisse ultrices gravida dictum. Suspendisse ultrices gravida dictum fusce ut placerat. Dignissim cras tincidunt lobortis feugiat vivamus at augue eget arcu. Rhoncus dolor purus non enim praesent elementum facilisis leo vel. Auctor augue mauris augue neque gravida in fermentum et sollicitudin. Ante in nibh mauris cursus mattis molestie a iaculis at. Ultricies integer quis auctor elit. Suspendisse potenti nullam ac tortor vitae purus faucibus ornare. Ultricies tristique nulla aliquet enim tortor at auctor. Orci phasellus egestas tellus rutrum tellus pellentesque. Volutpat blandit aliquam etiam erat velit. Habitant morbi tristique senectus et.

Sociis natoque penatibus et magnis dis parturient montes nascetur. Magna ac placerat vestibulum lectus mauris ultrices eros in cursus. Egestas congue quisque egestas diam in arcu. Urna neque viverra justo nec ultrices dui. Dictum sit amet justo donec enim diam vulputate ut. Justo donec enim diam vulputate ut pharetra. Pharetra pharetra massa massa ultricies mi quis hendrerit dolor magna. Vulputate dignissim suspendisse in est ante in nibh mauris. Vitae congue eu consequat ac felis donec et odio. Aliquam eleifend mi in nulla posuere sollicitudin. Amet facilisis magna etiam tempor orci eu lobortis.

Rhoncus mattis rhoncus urna neque viverra justo. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor. Nec nam aliquam sem et tortor consequat id. Eu scelerisque felis imperdiet proin. Bibendum ut tristique et egestas quis ipsum. Sed vulputate mi sit amet mauris commodo. Posuere urna nec tincidunt praesent semper feugiat nibh sed pulvinar. Pharetra massa massa ultricies mi quis hendrerit. Faucibus vitae aliquet nec ullamcorper sit amet risus nullam eget. Sit amet nisl purus in mollis nunc sed id. In eu mi bibendum neque egestas congue. In tellus integer feugiat scelerisque varius morbi enim nunc faucibus. Eros donec ac odio tempor orci dapibus ultrices in iaculis. Arcu dui vivamus arcu felis. Pellentesque eu tincidunt tortor aliquam. Duis tristique sollicitudin nibh sit. Sagittis aliquam malesuada bibendum arcu vitae elementum curabitur vitae. Mattis aliquam faucibus purus in massa tempor nec feugiat. Tellus in metus vulputate eu scelerisque felis. Aliquet eget sit amet tellus cras.

Lacinia quis vel eros donec ac odio tempor. Dolor sit amet consectetur adipiscing. Pellentesque adipiscing commodo elit at imperdiet dui accumsan sit amet. Sit amet facilisis magna etiam tempor orci. Sed sed risus pretium quam. Vel pretium lectus quam id. Pellentesque habitant morbi tristique senectus et netus et. Risus nullam eget felis eget nunc lobortis. Nisl nisi scelerisque eu ultrices vitae auctor eu. Placerat orci nulla pellentesque dignissim enim. Sagittis purus sit amet volutpat consequat mauris nunc. Integer eget aliquet nibh praesent. Ut morbi tincidunt augue interdum.

Vitae et leo duis ut diam quam. Rutrum quisque non tellus orci ac. Lectus sit amet est placerat in egestas erat. Velit egestas dui id ornare arcu odio ut sem nulla. Semper risus in hendrerit gravida rutrum quisque. Erat nam at lectus urna duis convallis convallis tellus id. Magna eget est lorem ipsum dolor sit amet consectetur. Lectus nulla at volutpat diam ut. Ornare lectus sit amet est placerat. Posuere ac ut consequat semper.

Pellentesque adipiscing commodo elit at imperdiet dui accumsan sit amet. Quis hendrerit dolor magna eget est lorem ipsum. At lectus urna duis convallis convallis. Integer eget aliquet nibh praesent tristique. Mattis molestie a iaculis at erat pellentesque adipiscing. Nunc vel risus commodo viverra maecenas accumsan. Dapibus ultrices in iaculis nunc sed augue lacus viverra. At elementum eu facilisis sed odio. Eget duis at tellus at. Dui faucibus in ornare quam viverra orci sagittis eu. Sed viverra tellus in hac habitasse.

Dui faucibus in ornare quam viverra orci sagittis eu volutpat. Volutpat lacus laoreet non curabitur gravida arcu ac tortor. Nunc mattis enim ut tellus. Sollicitudin ac orci phasellus egestas tellus rutrum tellus. In iaculis nunc sed augue lacus viverra vitae. Ut sem viverra aliquet eget sit. Ac tortor vitae purus faucibus. Ac orci phasellus egestas tellus rutrum tellus. Dictum varius duis at consectetur lorem donec massa. Eu consequat ac felis donec et. Phasellus faucibus scelerisque eleifend donec. Mauris pharetra et ultrices neque ornare aenean euismod. Leo duis ut diam quam nulla. Risus at ultrices mi tempus imperdiet. Vulputate eu scelerisque felis imperdiet."

