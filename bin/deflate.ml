
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


let deflate bytes =
    (* convert huffman code bitv to bytes *)
    let code_to_bytes code =
        let nbytes = bytes_for_bit (Bitv.length code) in
        let bytes = Bitv.to_bytes code in
        Bytes.sub bytes (Bytes.length bytes - nbytes) nbytes
    in
    (* compress with LZ78 *)
    let (compressed, size) =
        bytes
        |> Bytes.to_string
        |> Lempel_ziv.compress
    in

    (* calculate the size of entities *)
    let ent_size, f =
        if size <= 1 lsl 8 then 1, Buffer.add_uint8
        else if size <= 1 lsl 16 then 2, Buffer.add_uint16_be
        else if size <= 1 lsl 31 then 4, fun buf x -> x |> Int32.of_int |> Buffer.add_int32_be buf
        else 8, fun buf x -> x |> Int64.of_int |> Buffer.add_int64_be buf
    in

    let freqs = Hashtbl.create size in
    List.iter (fun id ->
        match Hashtbl.find_opt freqs id with
        | Some freq -> Hashtbl.replace freqs id @@ freq + 1
        | None -> Hashtbl.add freqs id 1
    ) compressed;
    let freqs = freqs
        |> Hashtbl.to_seq
        |> List.of_seq
    in

    let dict, (bytes, len) = Huffman.compress List.length List.iter ent_size freqs compressed in
    let buffer = Buffer.create (len * 2) in

    (* add size of dict *)
    Buffer.add_uint8 buffer (List.length dict - 1);
    Buffer.add_uint8 buffer ent_size;
    (* add huffman dict *)
    List.iter (fun (ch, code) -> 
        f buffer ch;
        Buffer.add_uint8 buffer (Bitv.length code);
        Buffer.add_bytes buffer (code_to_bytes code)) dict;
    (* add length in bits *)
    Buffer.add_int32_be buffer (Int32.of_int len);
    (* add encoded text *)
    Buffer.add_bytes buffer bytes;
    Buffer.to_bytes buffer
    

let inflate bytes =
    (* get size of entry *)
    let ent_size = Bytes.get_uint8 bytes 0 in
    let f =
        match ent_size with
        | 1 -> Bytes.get_uint8
        | 2 -> Bytes.get_uint16_be
        | 4 -> fun buf x -> Bytes.get_int32_be buf x |> Int32.to_int
        | 8 -> fun buf x -> Bytes.get_int64_be buf x |> Int64.to_int
        | _ -> raise (Failure "bad format")
    in
    (* build dict from sequence of bytes *)
    let rec build_list i acc =
        if i >= Bytes.length bytes then List.rev acc
        else
            build_list (i + ent_size)
            @@ f bytes i :: acc
    in

    build_list 1 []
    |> Lempel_ziv.decompress 
    |> Bytes.of_string


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
            | Compress -> time "lz78" deflate input
            | Decompress -> time "decode" inflate input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

