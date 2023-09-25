open Lempel_ziv
open Tools

let (<<) a b = a lsl b
let (>>) a b = a lsr b
let ($)  a b = a lor b
let (&)  a b = a land b

type nbytes =
    | N1
    | N2
    | N4
    | N8

let code_limit = function
    | N1 ->  8 - 1
    | N2 -> 16 - 2
    | N4 -> 32 - 3
    | N8 -> 64 - 3

let compress input =
    (* encode code length with first few bits *)
    let len_bits = function
        | N1 -> 0b0   << (code_limit N1)
        | N2 -> 0b10  << (code_limit N2)
        | N4 -> 0b110 << (code_limit N4)
        | N8 -> 0b111 << (code_limit N8)
    in

    (* compress the string *)
    let compressed, _ =
        input
        |> Bytes.to_string
        |> compress
    in
    (* create buffer *)
    let buffer = Buffer.create @@ Bytes.length input in

    (* add next code to buffer *)
    let add_to_buffer code =
        (* decide how many bytes should represent the code *)
        let nbytes, code =
            if code < (1 << code_limit N1) then
                N1, code
            else
                (*let code = code - code_limit N1 in*)
                if code < (1 << code_limit N2) then
                    N2, code
                else
                    (*let code = code - code_limit N2 in*)
                    if code < (1 << code_limit N4) then
                        N4, code
                    else
                        (*let code = code - code_limit N4 in*)
                        N8, code
        in
        (* add length info to code, choose add' function based on nbytes *)
        let code, add' =
            let open Buffer in
            match nbytes with
            | N1 -> code $ len_bits N1, add_uint8
            | N2 -> code $ len_bits N2, add_uint16_be
            | N4 -> code $ len_bits N4, fun buf x ->
                    add_uint16_be buf (x >> 16 & (1 << 16 - 1));
                    add_uint16_be buf (x >>  0 & (1 << 16 - 1));
            | N8 -> code $ len_bits N8, fun buf x ->
                    add_uint16_be buf (x >> 48 & (1 << 16 - 1));
                    add_uint16_be buf (x >> 32 & (1 << 16 - 1));
                    add_uint16_be buf (x >> 16 & (1 << 16 - 1));
                    add_uint16_be buf (x >>  0 & (1 << 16 - 1));
        in
        add' buffer code
    in

    (* add sequence *)
    List.iter add_to_buffer compressed;
    (* output bytes *)
    Buffer.to_bytes buffer

let decompress bytes =
    (* remove length encoding bits to get sequence code *)
    let mask = function
        | N1 -> lnot (0b1   << (code_limit N1))
        | N2 -> lnot (0b11  << (code_limit N2))
        | N4 -> lnot (0b111 << (code_limit N4))
        | N8 -> lnot (0b111 << (code_limit N8))
    in

    (* build dict from sequence of bytes *)
    let rec build_list i acc =
        if i >= Bytes.length bytes then List.rev acc
        else
            let nbytes, code =
                let byte = Bytes.get_uint8 bytes i in
                if byte >> (8 - 1) = 0b0 then 1, byte
                else
                    let nbytes, mask =
                        if byte >> (8 - 2) = 0b10 then 2, mask N2
                        else if byte >> (8 - 3) = 0b110 then 4, mask N4
                        else 8, mask N8
                    in
                    let get' =
                        match nbytes with
                        | 2 -> Bytes.get_uint16_be
                        | 4 -> fun buf x ->
                                Bytes.get_uint16_be buf x << 16 $
                                Bytes.get_uint16_be buf (x + 1)
                        | _ -> fun buf x ->
                                Bytes.get_uint16_be buf (x + 0) << 48 $
                                Bytes.get_uint16_be buf (x + 1) << 32 $
                                Bytes.get_uint16_be buf (x + 2) << 16 $
                                Bytes.get_uint16_be buf (x + 3)
                    in
                    let code = get' bytes i & mask in
                    nbytes, match nbytes with
                    | 2 -> code(* + code_limit N1*)
                    | 4 -> code(* + code_limit N1 + code_limit N2*)
                    | _ -> code(* + code_limit N1 + code_limit N2 + code_limit N4*)
            in
            build_list (i + nbytes)
            @@ code :: acc
    in

    build_list 0 []
    |> decompress 
    |> Bytes.of_string


type mode =
    | Compress
    | Decompress

let () =
    let usage_err =
        Failure (Format.sprintf "Usage: %s <compress|decompress> <infile> <outfile>" Sys.argv.(0))
    in
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
            | Compress -> time "lz78" compress input
            | Decompress -> time "decode" decompress input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

