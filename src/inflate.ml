(* deflate *)
type block_type = Uncompressed | FixedHuffman | DynamicHuffman | Reserved
type block_final = Continues | Last

(* Huffman *)
type distance = int
type length = int
type huffman_elements = Literal of char | Repeat of length * distance

type zlib_header = { cm : int; cinfo : int; flevel : int; fdict : bool;
        fcheck : int; checksum : int }

(* https://stackoverflow.com/questions/2602823/ *)
let tab = [|0x0; 0x8; 0x4; 0xc; 0x2; 0xa; 0x6; 0xe;
            0x1; 0x9; 0x5; 0xd; 0x3; 0xb; 0x7; 0xf|]

let reverse_bits byte =
  let lookup = Array.get tab in
  ((lookup (byte land 0xf)) lsl 4) lor lookup (byte lsr 4)

let reverse_string_bits s =
  String.map (fun x -> Char.chr @@ reverse_bits @@ Char.code x) s

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

let length_code = function
  | 257 -> (0, 3)
  | 258 -> (0, 4)
  | 259 -> (0, 5)
  | 260 -> (0, 6)
  | 261 -> (0, 7)
  | 262 -> (0, 8)
  | 263 -> (0, 9)
  | 264 -> (0, 10)
  | 265 -> (1, 11)
  | 266 -> (1, 13)
  | 267 -> (1, 15)
  | 268 -> (1, 17)
  | 269 -> (2, 19)
  | 270 -> (2, 23)
  | 271 -> (2, 27)
  | 272 -> (2, 31)
  | 273 -> (3, 35)
  | 274 -> (3, 43)
  | 275 -> (3, 51)
  | 276 -> (3, 59)
  | 277 -> (4, 67)
  | 278 -> (4, 83)
  | 279 -> (4, 99)
  | 280 -> (4, 115)
  | 281 -> (5, 131)
  | 282 -> (5, 163)
  | 283 -> (5, 195)
  | 284 -> (5, 227)
  | 285 -> (0, 258)
  | _ -> failwith "Invalid length code"

let distance_code = function
  | 0 -> (0, 1)
  | 1 -> (0, 2)
  | 2 -> (0, 3)
  | 3 -> (0, 4)
  | 4 -> (1, 5)
  | 5 -> (1, 7)
  | 6 -> (2, 9)
  | 7 -> (2, 13)
  | 8 -> (3, 17)
  | 9 -> (3, 25)
  | 10 -> (4, 33)
  | 11 -> (4, 49)
  | 12 -> (5, 65)
  | 13 -> (5, 97)
  | 14 -> (6, 129)
  | 15 -> (6, 193)
  | 16 -> (7, 257)
  | 17 -> (7, 385)
  | 18 -> (8, 513)
  | 19 -> (8, 769)
  | 20 -> (9, 1025)
  | 21 -> (9, 1537)
  | 22 -> (10, 2049)
  | 23 -> (10, 3073)
  | 24 -> (11, 4097)
  | 25 -> (11, 6145)
  | 26 -> (12, 8193)
  | 27 -> (12, 12289)
  | 28 -> (13, 16385)
  | 29 -> (13, 24577)
  | _ -> failwith "Invalid distance code"

let read_reversed width num =
  let rec reconstruct shifts num acc =
    match shifts with
    | 0 -> acc
    | n -> let bit = num land 0x1 in
      let acc = (bit lsl (n-1)) lor acc in
      (* Printf.printf "Bit %d Shift %d Res %d\n" bit n acc; *)
      reconstruct (n-1) (num lsr 1) acc
  in
  reconstruct width num 0

let rec decode_huffman bits =
  bitmatch bits with
  (* Literal 256, termination *)
  | { element : 7;
      rest : -1 : bitstring } when element = 0 ->
      ([], rest)
  (* Literal 257 - 279, distance code *)
  | { element : 7;
      rest : -1 : bitstring } when element > 0 && element <= 23 ->
      Printf.printf "Element %d\n" element;
      let extra_bits, length_start = length_code @@ element + 256 in
      Printf.printf "Extra bits %d, offset start %d\n" extra_bits length_start;

      let length, rest = bitmatch rest with
      | { length : extra_bits;
          rest : -1 : bitstring } ->
        let length = read_reversed extra_bits (Int64.to_int length) in
        (length + length_start, rest)
      in

      let distance, rest = bitmatch rest with
      | { distance : 5;
          rest : -1 : bitstring } ->
            Printf.printf "Distance code %d\n" distance;
            let extra_bits, distance_start = distance_code distance in
            bitmatch rest with
            | { distance : extra_bits;
                rest : -1 : bitstring } ->
              let distance = read_reversed extra_bits (Int64.to_int distance) in
              (distance + distance_start, rest)
      in
      Printf.printf "Distance %d\n" distance;

      let decoded, rest = decode_huffman rest in
      (Repeat (length, distance)::decoded, rest)
  (* Literal 0 - 143 *)
  | { element : 8;
      rest : -1 : bitstring } when element >= 48 && element <= 191 ->
      print_endline @@ Char.escaped @@ Char.chr @@ element - 0x30;
      let decoded, rest = decode_huffman rest in
      (Literal (Char.chr @@ element - 0x30)::decoded, rest)
  (* Literal 280 - 287 *)
  | { element : 8 } when element >= 192 && element <= 199 ->
      failwith "Length code"
  (* Literal 144 - 255 *)
  | { element : 9;
      rest : -1 : bitstring } when element >= 400 && element <= 511 ->
      let decoded, rest = decode_huffman rest in
      (Literal (Char.chr @@ element - 0x190)::decoded, rest)
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
