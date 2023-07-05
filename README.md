# huffman.ml

This is a encoding-decoding program for losslessly compressing text files.

## Usage example:

This is an example of encoding and decoding an HTML file.

    /home/jakob/git/ocaml/huffman〉huffman encode jpbb.html jpbb.html.huf
    1.070 ms <- encode.freqs
    0.149 ms <- encode.huffman
    0.023 ms <- encode.sort
    57.591 ms <- encode.to_code
    /home/jakob/git/ocaml/huffman〉huffman decode jpbb.html.huf jpbb.html.huf.decoded
    /home/jakob/git/ocaml/huffman〉ls jpbb*                                                                   07/05/2023 10:36:46
    ╭───┬───────────────────────┬──────┬──────────┬────────────────╮
    │ # │         name          │ type │   size   │    modified    │
    ├───┼───────────────────────┼──────┼──────────┼────────────────┤
    │ 0 │ jpbb.html             │ file │ 310.6 KB │ 5 days ago     │
    │ 1 │ jpbb.html.huf         │ file │ 213.5 KB │ 15 seconds ago │
    │ 2 │ jpbb.html.huf.decoded │ file │ 310.6 KB │ now            │
    ╰───┴───────────────────────┴──────┴──────────┴────────────────╯
    /home/jakob/git/ocaml/huffman〉
