open Huffman;;

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s |> String.to_bytes

let write_whole_file filename bytes =
    let ch = open_out_bin filename in
    output_bytes ch bytes;
    close_out ch

let bytes_to_int bytes =
    String.fold_left (fun acc c -> acc * 2 + Char.code c - Char.code '0') 0 bytes
        |> Int32.of_int

(*
   encoding: [
       <huffman dict>
       <NULL char>
       <encoded text>
   ]

   <huffman dict>: [
       (char, n_bits, bits)
   ] where char = 8b, n_bits = 8b, bits = 32b
 *)

let bytes_to_huffman_encoded bytes =
    let (dict, (bytes, len)) = encode bytes in
    let buffer = Buffer.create len in
    (* add huffman dict *)
    List.iter (fun (ch, code) -> 
        Buffer.add_char buffer ch;
        Buffer.add_uint8 buffer (String.length code);
        Buffer.add_int32_ne buffer (bytes_to_int code)) dict;
    (* add a NULL char as a separator *)
    Buffer.add_char buffer (Char.chr 0);
    (* add length in bits *)
    Buffer.add_int32_ne buffer (Int32.of_int len);
    (* add encoded text *)
    Buffer.add_bytes buffer bytes;
    Buffer.to_bytes buffer

let huffman_encoded_to_bytes bytes =
    let get_int i =
        let rec aux acc = function
            | 4 -> i + 4, acc
            | n -> aux (Int.shift_left acc 8 lor Char.code (Bytes.get bytes (i + n))) (n + 1)
        in aux 0 0
    in
    let rec build_code dict i =
        print_int i; print_char '\n';

        let int_to_bit_str num len =
            let rec aux acc num len =
                match num, len with
                | 0, 0 -> acc
                | 0, _ -> aux ("0" ^ acc) 0 (len - 1)
                | _, _ ->
                        let digit = Char.escaped (Char.chr (num mod 2 + Char.code '0')) in
                        let _ = print_string digit in
                        aux (digit ^ acc) (num / 2) (len - 1)
            in
            aux "" num len
        in

        let byte = Bytes.get bytes i in
        match byte, Char.code byte with
        | _, 0 -> print_char '\n'; i + 1, dict
        | char, _ ->
                let len = Char.code (Bytes.get bytes (i + 1)) in
                let i, code = get_int (i + 2) in
                let code = int_to_bit_str code len in
                Format.printf "(%s, %s);\n" (Char.escaped char) code;
                build_code ((char, code) :: dict) i
    in

    let i, dict = build_code [] 0 in
    let i, len = get_int i in
    let dict = List.rev dict in

    print_int ((Bytes.length bytes - i) * 8); print_newline ();
    let bytes = Bytes.sub bytes i (Bytes.length bytes - i) in
    decode (dict, (bytes, len)) |> String.to_bytes

type mode =
    | Encode
    | Decode

let () =
    if Array.length Sys.argv < 4 then
        raise (Failure "Usage: ./main <encode|decode> <infile> <outfile>")
    else
        let mode =
            match Sys.argv.(1) with
            | "encode" -> Encode
            | "decode" -> Decode
            | _ -> raise (Failure "Usage: ./main <encode|decode> <infile> <outfile>")
        in

        let infile = Sys.argv.(2) in
        let outfile = Sys.argv.(3) in
        let input = read_whole_file infile in

        let output =
            match mode with
            | Encode -> bytes_to_huffman_encoded input
            | Decode -> huffman_encoded_to_bytes input
        in
        
        output
            |> write_whole_file outfile

