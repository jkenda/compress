open Tools

let bitv_of_string str =
    Bitv.init (String.length str) (fun i -> str.[i] = '1')

let string_of_bitv bitv =
    String.init (Bitv.length bitv) (fun i -> if Bitv.unsafe_get bitv i then '1' else '0')

let bitv_of_int num len =
    Bitv.init len (fun i -> (num lsr (len - 1 - i)) land 1 = 1)

type 'a node =
    | Leaf of int * 'a
    | Node of int * 'a node * 'a node

let freq = function
    | Leaf (freq, _)
    | Node (freq, _, _) -> freq

(* transform (char, freq) list to a (char, huffman code) list *)
let huffman freqs =
    (* build huffman tree *)
    let rec build_tree list =
        (* make node from first two nodes in the list *)
        let combine = function
            | a :: b :: tl -> Node (freq a + freq b, a, b), tl
            | _ -> raise (Unreachable "always at least 2 nodes")
        in
        (* insert node at the appropriate position *)
        let insert (node, list) =
            let rec aux = function
                | [] -> [node]
                | hd :: _ as ls when freq node < freq hd -> node :: ls
                | hd :: tl -> hd :: aux tl
            in aux list
        in

        (* keep building the tree until only one node is left (the root) *)
        match list with
        | [] -> None
        | [root] -> Some root
        | _ -> list
                |> combine
                |> insert
                |> build_tree
    in
    (* transform tree to list of huffman codes *)
    let to_huffman tree =
        let rec aux len code = function
            | Leaf (_, char) -> [char, bitv_of_int code len]
            | Node (_, lc, rc) -> aux (len + 1) (2 * code + 0) lc
                                @ aux (len + 1) (2 * code + 1) rc
        in
        match tree with
        | None -> []
        | Some Leaf (_, char) -> [char, bitv_of_int 0 1]
        | Some tree -> aux 0 0 tree
    in

    freqs
        |> List.sort (fun (_, f1) (_, f2) -> f1 - f2)
        |> List.map (fun (char, freq) -> Leaf (freq, char))
        |> build_tree
        |> to_huffman

let%test _ = huffman [] = []
let%test _ = huffman [('x', 1)] = [('x', bitv_of_string "0")]
let%test _ = huffman [('a', 45); ('b', 13); ('c', 12); ('d', 16); ('e', 9); ('f', 5)]
= [('a', bitv_of_string "0"); ('c', bitv_of_string "100"); ('b', bitv_of_string "101");
    ('f', bitv_of_string "1100"); ('e', bitv_of_string "1101"); ('d', bitv_of_string "111")]

(* Encode mesage with huffman *)
let encode message =
    (* calculate frequencies of characters *)
    let freqs string =
        let array = Array.make 256 0 in
        Bytes.iter (fun c -> array.(Char.code c) <- array.(Char.code c) + 1) string;
        let rec aux = function
            | 256 -> []
            | n -> if array.(n) > 0 then (Char.chr n, array.(n)) :: aux (n + 1) else aux (n + 1)
        in
        aux 0
    in
    (* buffer for building strings *)
    let out_buffer = Bitv.create (Bytes.length message * 8) false in
    let i = ref 0 in
    (* encode mesage with huffman *)
    let to_code dict =
        let add_code char =
            let rec find_code = function
                | (ch, code) :: _ when ch = char ->
                        let len = Bitv.length code in
                        Bitv.blit code 0 out_buffer !i len;
                        i := !i + len
                | _ :: tl -> find_code tl
                | [] -> raise (Failure "code for char always exists")
            in
            find_code dict
        in
        Bytes.iter add_code message;
        (Bitv.sub out_buffer 0 !i |> Bitv.to_bytes), !i
    in

    let dict = message
                |> time "encode.freqs" freqs
                |> time "encode.huffman" huffman
                |> time "encode.sort" List.sort (fun (_, c1) (_, c2) -> Bitv.length c1 - Bitv.length c2)
    in
    let (bytes, _) as encoded = time "encode.to_code" to_code dict in
    Format.printf "raw size: %d B, compressed size: %d B, compression ratio: %f\n"
        (Bytes.length message)
        (Bytes.length bytes)
        (Float.of_int (Bytes.length message) /. Float.of_int (Bytes.length bytes));
    (dict, encoded)

(* decode huffman encoded message *)
let decode (dict, (bytes, len)) =
    (* buffer for building strings *)
    let in_buffer  = Bitv.of_bytes bytes in
    let out_buffer = Buffer.create len in
    (* build string from code *)
    (* find a code that matches the front of the buffer, then
        - add the corresponding char to the out buffer,
        - clear the bit buffer *)
    let rec code_to_string start =
        (* check if the in_buffer[start..] starts with the code *)
        let rec starts_with code i = 
            let j = i - start in
            if j >= Bitv.length code then true
            else if i >= Bitv.length in_buffer then false
            else
                if Bitv.unsafe_get in_buffer i = Bitv.unsafe_get code j then
                    starts_with code (i + 1)
                else
                    false
        in
        (* find char that matches the code *)
        let rec find_char dict start =
            match dict with
            | (char, code) :: _ when starts_with code start -> char, Bitv.length code
            | _ :: tl -> find_char tl start
            | _ -> raise (Failure "unreachable: there is always a match")
        in

        if start < len then
            let char, len = find_char dict start in 
            Buffer.add_char out_buffer char;
            code_to_string (start + len)
    in

    time "decode.code_to_string" code_to_string 0;
    Buffer.to_bytes out_buffer


let%test _ = Bytes.of_string "" |> encode |> decode = Bytes.of_string ""
let%test _ = Bytes.of_string "a" |> encode |> decode = Bytes.of_string "a"
let%test _ = Bytes.of_string "aa" |> encode |> decode = Bytes.of_string "aa"

let lorem_ipsum = Bytes.of_string "
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

Commodo odio aenean sed adipiscing diam donec adipiscing tristique. A arcu cursus vitae congue mauris rhoncus aenean vel elit. Nullam non nisi est sit amet facilisis. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor. Aliquet nibh praesent tristique magna sit amet. Lobortis elementum nibh tellus molestie nunc non blandit. Porttitor eget dolor morbi non arcu risus quis varius. Id diam maecenas ultricies mi eget mauris pharetra et. Arcu cursus vitae congue mauris rhoncus. Convallis aenean et tortor at risus viverra adipiscing at in. Ac tincidunt vitae semper quis lectus nulla at volutpat diam. Convallis convallis tellus id interdum velit laoreet id.
";;

let%test _ = lorem_ipsum |> encode |> decode = lorem_ipsum

