(* deflate *)
type blockType = Uncompressed | FixedHuffman | DynamicHuffman | Reserved
type blockFinal = Continues | Last
type deflatedContent = Payload of string
type deflateBlock = (blockFinal * blockType * deflatedContent)

(* Huffman *)
type distance = int
type length = int
type huffman_elements = Literal of char | Repeat of length * distance

type zlib_header = { cm : int; cinfo : int; flevel : int; fdict : bool;
        fcheck : int; checksum : int }

let parse_zlib_header bits =
  bitmatch bits with
  | {
      cinfo : 4;
      cm : 4;
      flevel : 2;
      fdict : 1;
      fcheck : 5
    } -> bitmatch bits with
          | {checksum : 16} ->
            {cm=cm; cinfo=cinfo; flevel=flevel; fdict=fdict;
            fcheck=fcheck; checksum=checksum}

let final_block = function
  | true -> Last
  | _ -> Continues

let decode_huffman bits =
  bitmatch bits with
  (* Literal 256, termination *)
  | { element : 7 } when element = 0 ->
      failwith "End of huffman stream"
  (* Literal 257 - 279, distance code *)
  | { element : 7 } when element > 0 && element <= 23 ->
      (* TODO *)
      Repeat (23, 42)
  (* Literal 0 - 143 *)
  | { element : 8 } when element >= 48 && element <= 191 ->
      Literal (Char.chr @@ element - 0x30)
  (* Literal 280 - 287 *)
  | { element : 8 } when element >= 192 && element <= 199 ->
      Literal (Char.chr @@ element - 0xC0)
  (* Literal 144 - 255 *)
  | { element : 9 } when element >= 400 && element <= 511 ->
      Literal (Char.chr @@ element - 0x190)
  | { _ } -> failwith "Invalid huffman segment"

let parse_segment bitstring =
  bitmatch bitstring with
  | { bfinal : 1;
      (* Non-compressed block, BTYPE=00 *)
      0 : 2 } ->
    (* ((final_block bfinal, Uncompressed, Payload content), rest) *)
    failwith "Uncompressed data, TODO"
  | { bfinal : 1;
      (* fixed Huffman, BTYPE=0b01, 0b10=2 in reversed *)
      2 : 2;
      rest : -1 : bitstring } ->
    let decoded = [decode_huffman rest] in
    ((final_block bfinal, FixedHuffman, decoded), rest)
  | { bfinal : 1;
      (* dynamic Huffman, BTYPE=10, 0b01=1 in reversed *)
      1 : 2;
      _ : -1 : bitstring } ->
    (* ((final_block bfinal, DynamicHuffman, []), rest) *)
    failwith "Dynamic Huffman, TODO"
  | { bfinal : 1;
      (* reserved, BTYPE=11, fail *)
      3 : 2} ->
    failwith "Reserved block, invalid bitstream"

let rec parse_payload bits =
  let seg, rest = parse_segment bits in
  match seg with
  | Last, _, _ -> [seg]
  | Continues, _, _ -> seg :: parse_payload rest

(* https://stackoverflow.com/questions/2602823/ *)
let tab = [|0x0; 0x8; 0x4; 0xc; 0x2; 0xa; 0x6; 0xe;
            0x1; 0x9; 0x5; 0xd; 0x3; 0xb; 0x7; 0xf|]

let reverse_bits byte =
  let lookup = Array.get tab in
  ((lookup (byte land 0xf)) lsl 4) lor lookup (byte lsr 4)

let reverse_string_bits s =
  String.map (fun x -> Char.chr @@ reverse_bits @@ Char.code x) s

let parse_zlib bytestring =
  let bits = Bitstring.bitstring_of_string bytestring in
  bitmatch bits with
  | { header: 16: bitstring; blocks: -1: string } ->
    (parse_zlib_header header, blocks
      |> reverse_string_bits
      |> Bitstring.bitstring_of_string
      |> parse_payload)

let adler32 data =
  let (a, b) = List.fold_left (fun (a, b) d ->
      (* these are guaranteed to be 16 bit which fits 31 bit OCaml ints *)
      let new_a = (a + d) mod 65521 in
      let new_b = (b + new_a) mod 65521 in
      (new_a, new_b))
    (1, 0) data in
  let a32 = Int32.of_int a
  and b32 = Int32.of_int b
  in Int32.logor a32 @@ Int32.shift_left b32 16

let inflate data =
  let bits = Bitstring.bitstring_of_string data in
  ignore @@ parse_payload bits;
  "Foo"
