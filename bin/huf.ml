open Huffman
open Tools

(*
   encoding: (
       <huffman dict>
       <encoded text>
   )

   <huffman dict>: (
       <num>
       [(char, n_bits, bits); num]
   ) where char = 1B, n_bits = 1B, bits = 1-32B
 *)


let freqs_eng_avg =
    let freqs_eng_avg = [
        9, 136; 23, 1; 32, 407934; 33, 170; 34, 5804; 35, 425; 36, 1333;
        37, 380; 38, 536; 39, 5816; 40, 5176; 41, 5307; 42, 1493; 43, 511;
        44, 17546; 45, 32638; 46, 35940; 47, 3681; 48, 13109; 49, 10916; 50, 7894;
        51, 4389; 52, 3204; 53, 3951; 54, 2739; 55, 2448; 56, 2505; 57, 2433;
        58, 10347; 59, 2884; 60, 2911; 61 , 540; 62 , 2952; 63 , 3503; 64 , 173;
        65, 7444; 66, 5140; 67 , 9283; 68 , 7489; 69 , 6351; 70 , 3365; 71 , 4459;
        72, 5515; 73, 7631; 74 , 4102; 75 , 1633; 76 , 4476; 77 , 8386; 78 , 4954;
        79, 4378; 80, 6211; 81 , 751; 82 , 5986; 83 , 9512; 84 , 7895; 85 , 1934;
        86, 2119; 87, 6005; 88 , 815; 89 , 722; 90 , 180; 91 , 205; 92 , 37;
        93, 210; 94, 8; 95 , 2755; 96 , 21; 97 , 123287; 98 , 24227; 99 , 50211;
        100, 59577; 101, 203824; 102, 32616; 103, 37064; 104, 65217; 105, 116488; 106, 2061;
        107, 16047; 108, 75450; 109, 39060; 110, 118108; 111, 137119; 112, 36791; 113, 1774;
        114, 101201; 115, 103814; 116, 151376; 117, 49901; 118, 20109; 119, 30974; 120, 4635;
        121, 26924; 122, 1417; 123, 62; 124, 16; 125, 61; 126, 8; 131, 1;
        149, 15233; 183, 23; 223, 1; 226, 1; 229, 1; 230, 1; 237, 1]
    |> List.map (fun (code, freq) -> Char.chr code, freq)
    in
    let add_letter_freqs list = function
        | 'a'..'z', _ | 'A'..'Z', _ -> list
        | b, _ -> (b, 0) :: list
    in
    Array.init 256 (fun b -> Char.chr b, 0)
    |> Array.fold_left add_letter_freqs freqs_eng_avg

let dict_eng_avg =
    huffman freqs_eng_avg

(* List.iter (fun (byte, code) -> Format.printf "%c: %d\n" byte @@ Bitv.length code) dict_eng_avg *)

let bytes_to_huffman_encoded bytes =
    (* convert huffman code bitv to bytes *)
    let code_to_bytes code =
        let nbytes = bytes_for_bit (Bitv.length code) in
        let bytes = Bitv.to_bytes code in
        Bytes.sub bytes (Bytes.length bytes - nbytes) nbytes
    in

    (* calculate frequencies of characters *)
    let freqs = freqs bytes in
    let dict = huffman freqs in
    let _, (bytes, len) = compress Bytes.length Bytes.iter 1 dict bytes in
    let buffer_with = Buffer.create (len / 4) in
    (* add length in bits *)
    Buffer.add_int32_be buffer_with (Int32.of_int len);
    (* add size of dict *)
    if len > 0 then
        Buffer.add_uint8 buffer_with @@ List.length dict - 1;
    (* add huffman dict *)
    List.iter (fun (ch, code) -> 
        Buffer.add_char buffer_with ch;
        Buffer.add_uint8 buffer_with (Bitv.length code);
        Buffer.add_bytes buffer_with (code_to_bytes code)) dict;
    (* add encoded text *)
    Buffer.add_bytes buffer_with bytes;

    (* use average frequencies in English *)
    let _, (bytes, len) = compress Bytes.length Bytes.iter 3 dict_eng_avg bytes in
    let buffer_without = Buffer.create (len / 4) in
    (* add length in bits *)
    Buffer.add_int32_be buffer_without @@ Int32.of_int (len - 1);
    (* add size of dict *)
    Buffer.add_uint8 buffer_without 0;
    (* add encoded text *)
    Buffer.add_bytes buffer_without bytes;

    Format.printf "w/ dict: %d, w/o dict: %d\n" (Buffer.length buffer_with) (Buffer.length buffer_without);

    if Buffer.length buffer_with < Buffer.length buffer_without then
        Buffer.to_bytes buffer_with
    else
        Buffer.to_bytes buffer_without

let huffman_encoded_to_bytes bytes =
    (* get size of dict *)
    let len = Bytes.get_int32_be bytes 0 |> Int32.to_int in
    let dict_size =
        if len = 0 then 0
        else Bytes.get_uint8 bytes 4 + 1
    in
    (* get code from sequence of bytes *)
    let bytes_to_code i len =
        let int_size = bytes_for_bit Sys.int_size in
        let buffer = Buffer.create (int_size + bytes_for_bit len) in
        (match int_size with
         | 8 -> Buffer.add_int64_ne buffer (Int64.of_int len)
         | 4 -> Buffer.add_int32_ne buffer (Int32.of_int len)
         | _ -> assert false);
        Buffer.add_subbytes buffer bytes i len; 
        i + bytes_for_bit len, Buffer.to_bytes buffer |> Bitv.of_bytes
    in
    (* build dict from sequence of bytes *)
    let build_dict i =
        let rec aux acc n i =
            if n = dict_size then i, acc
            else
                let char = Bytes.get bytes i in
                let len = Bytes.get_uint8 bytes (i + 1) in
                let i, code = bytes_to_code (i + 2) len in
                aux ((char, code) :: acc) (n + 1) i
        in
        let i, dict = aux [] 0 i in
        i, List.rev dict
    in

    let i, dict =
        if dict_size = 0 then 2, dict_eng_avg
        else time "decode.build_dict" build_dict 5
    in
    let bytes = Bytes.sub bytes i (Bytes.length bytes - i) in
    decompress (dict, (bytes, len))

type mode =
    | Compress
    | Decompress

let usage_err =
    Failure (Format.sprintf "Usage: %s <compress|decompress> <infile> <outfile>" Sys.argv.(0))

let () =
    let open Tools in
    if Array.length Sys.argv < 4 then
        raise usage_err
    else
        let mode =
            match Sys.argv.(1) with
            | "compress" -> Compress
            | "decompress" -> Decompress
            | _ -> raise usage_err
        in

        let infile = Sys.argv.(2) in
        let outfile = Sys.argv.(3) in
        let input = read_whole_file infile in

        let output =
            match mode with
            | Compress -> time "encode" bytes_to_huffman_encoded input
            | Decompress -> time "decode" huffman_encoded_to_bytes input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

