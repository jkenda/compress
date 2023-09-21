type seq = {
    dist : int;
    len : int;
    ch : char
}

type token =
    | Id of int
    | Char of char

type node = { id : int; ch : char option; parent : node option; mutable children : node list }

let make_node parent id ch = {
    id;
    ch = Some ch;
    parent = Some parent;
    children = []
}

let encode input =
    let root = { id = 0; ch = None; parent = None; children = [] }
    and size = ref 1 in

    (* find the longest substring already in the dict *)
    let find string i =
        let rec find' parent i len =
            let find_max ((_, max_len) as max) node =
                let (_, len) as curr =
                    if try Some string.[i] = node.ch with _ -> false then
                        find' node (i + 1) (len + 1)
                    else if len = 0 then
                        find' node i len
                    else max
                in
                if len > max_len then curr else max
            in
            List.fold_left find_max (parent, len) parent.children
        in
        find' root i 0
    (* insert the new node into the dict *)
    and insert_child node char =
        node.children <- make_node node !size char :: node.children;
        size := !size + 1
    in

    let rec encode' dict i =
        if i >= String.length input then List.rev dict
        else
            let (node, len) = find input i in
            insert_child node input.[i];
            let next_i = i + len in
            let dict =
                try Char input.[next_i] :: Id node.id :: dict
                with Invalid_argument _ -> Id node.id :: dict
            in
            encode' dict (next_i + 1)
    in
    if String.length input = 0 then [Id 0]
    else encode' [] 0


let test_encode input expected =
    let rec print = function
        | [] -> ()
        | Id id :: tl -> print_int id; print tl
        | Char ch :: tl -> print_char ch; print tl
    in
    let output = encode input in
    let pass = output = expected in
    if not pass then (print output; print_newline ());
    pass

let%test _ = test_encode ""      [Id 0]
let%test _ = test_encode "a"     [Id 0; Char 'a']
let%test _ = test_encode "aa"    [Id 0; Char 'a'; Id 1]
let%test _ = test_encode "aba"   [Id 0; Char 'a'; Id 0; Char 'b'; Id 1]
let%test _ = test_encode "abab"  [Id 0; Char 'a'; Id 0; Char 'b'; Id 1; Char 'b']
let%test _ = test_encode "abba"  [Id 0; Char 'a'; Id 0; Char 'b'; Id 2; Char 'a']
let%test _ = test_encode "AABBA" [Id 0; Char 'A'; Id 1; Char 'B'; Id 0; Char 'B'; Id 1]


let decode list =
    let dict = { id = 0; ch = None; parent = None; children = [] }
    and size = ref 1 in

    let insert id char =
        let rec insert' = function
            | [] -> None
            | node :: tl ->
                if node.id = id then (
                    node.children <- make_node node !size char :: node.children;
                    size := !size + 1;
                    Some node)
                else
                    match insert' node.children with
                    | None -> insert' tl
                    | node -> node
        in
        Option.get @@ insert' [dict]
    and build node =
        let rec build' node acc =
            if node.id = 0 then acc
            else
                build' (Option.get node.parent) (String.make 1 (Option.get node.ch) ^ acc)
        in
        build' node ""
    in

    let rec decode' acc = function
        | [] -> acc
        | [Id id] -> acc ^ (insert id '$' |> build)
        | Id id :: Char char :: tl ->
                let add_next_char str =
                     str ^ String.make 1 char
                in
                let acc = acc ^
                    (char
                        |> insert id
                        |> build
                        |> add_next_char)
                in
                decode' acc tl
        | _ -> raise (Failure "Unexpected token")
    in
    decode' "" list

let test_decode input expected =
    let output = decode input in
    let pass = output = expected in
    if not pass then print_endline output;
    pass

let%test _ = test_decode [Id 0]                                                 ""
let%test _ = test_decode [Id 0; Char 'a']                                       "a"
let%test _ = test_decode [Id 0; Char 'a'; Id 1]                                 "aa"
let%test _ = test_decode [Id 0; Char 'a'; Id 0; Char 'b'; Id 1]                 "aba"
let%test _ = test_decode [Id 0; Char 'a'; Id 0; Char 'b'; Id 1; Char 'b']       "abab"
let%test _ = test_decode [Id 0; Char 'a'; Id 0; Char 'b'; Id 2; Char 'a']       "abba"
let%test _ = test_decode [Id 0; Char 'A'; Id 1; Char 'B'; Id 0; Char 'B'; Id 1] "AABBA"

let%test _ = ""      |> encode |> decode = ""
let%test _ = "a"     |> encode |> decode = "a"
let%test _ = "aa"    |> encode |> decode = "aa"
let%test _ = "aba"   |> encode |> decode = "aba"
let%test _ = "abab"  |> encode |> decode = "abab"
let%test _ = "abba"  |> encode |> decode = "abba"
let%test _ = "AABBA" |> encode |> decode = "AABBA"
let%test _ = "AABBAABBA" |> encode |> decode = "AABBAABBA"

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

Dui faucibus in ornare quam viverra orci sagittis eu volutpat. Volutpat lacus laoreet non curabitur gravida arcu ac tortor. Nunc mattis enim ut tellus. Sollicitudin ac orci phasellus egestas tellus rutrum tellus. In iaculis nunc sed augue lacus viverra vitae. Ut sem viverra aliquet eget sit. Ac tortor vitae purus faucibus. Ac orci phasellus egestas tellus rutrum tellus. Dictum varius duis at consectetur lorem donec massa. Eu consequat ac felis donec et. Phasellus faucibus scelerisque eleifend donec. Mauris pharetra et ultrices neque ornare aenean euismod. Leo duis ut diam quam nulla. Risus at ultrices mi tempus imperdiet. Vulputate eu scelerisque felis imperdiet.
"

let lorem_ipsum_encoded = encode lorem_ipsum
let%test _ = decode lorem_ipsum_encoded = lorem_ipsum

let () =
    let _ = test_encode lorem_ipsum [] in
    (* print_endline @@ decode lorem_ipsum_encoded; *)
    Format.printf "compression ratio: %f\n"
    @@ ((String.length lorem_ipsum |> Float.of_int)
     /. (List.length lorem_ipsum_encoded |> Float.of_int))

