(**
 * decompress DEFLATE streams
 * Copyright (c) 2010, 2015 Marek Kubica <marek@xivilization.net>
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 *    1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 *
 *    2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 *    3. This notice may not be removed or altered from any source
 *    distribution.
 *)

(* Huffman *)
type distance = int
type length = int
type huffman_elements = Literal of char | Repeat of length * distance

(* DEFLATE *)
type blocks =
  | Uncompressed of string
  | FixedHuffman of huffman_elements list
  | DynamicHuffman
type block_final = Continues | Last

(* ZLIB *)
type compression_method = Deflate
type window_size = int
type dict_present = bool
type compression_level = Fastest | Fast | Default | Maximum

type zlib_header = {
  compression_method : compression_method;
  window_size : int;
  compression_level : compression_level;
  fdict : bool;
  fcheck : int;
  checksum : int
}

(* https://stackoverflow.com/questions/2602823/ *)
let tab = [|0x0; 0x8; 0x4; 0xc; 0x2; 0xa; 0x6; 0xe;
            0x1; 0x9; 0x5; 0xd; 0x3; 0xb; 0x7; 0xf|]

let reverse_bits byte =
  let lookup = Array.get tab in
  ((lookup (byte land 0xf)) lsl 4) lor lookup (byte lsr 4)

let reverse_string_bits s =
  String.map (fun x -> Char.chr @@ reverse_bits @@ Char.code x) s

(** Skips to next complete byte. Does nothing if it is aligned to a byte. *)
let align_to_next_byte bits =
  let _, offset, _ = bits in
  let byte_offset = offset mod 8 in
  if byte_offset = 0 then bits else
    BS.drop (8 - byte_offset) bits

let compression_level = function
  | 0 -> Fastest
  | 1 -> Fast
  | 2 -> Default
  | 3 -> Maximum
  | _ -> failwith "Invalid compression level"

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
            if cm <> 8 then failwith "Invalid compression method" else
            {
              compression_method = Deflate;
              window_size = int_of_float @@ 2. ** (float_of_int cinfo +. 8.);
              compression_level = compression_level flevel;
              fdict = fdict;
              fcheck = fcheck;
              checksum = checksum
            }

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
      (* Printf.printf "Element %d\n" element; *)
      let extra_bits, length_start = length_code @@ element + 256 in
      (* Printf.printf "Extra bits %d, offset start %d\n" extra_bits length_start; *)

      let length, rest = bitmatch rest with
      | { length : extra_bits;
          rest : -1 : bitstring } ->
        let length = read_reversed extra_bits (Int64.to_int length) in
        (length + length_start, rest)
      in

      let distance, rest = bitmatch rest with
      | { distance : 5;
          rest : -1 : bitstring } ->
            (* Printf.printf "Distance code %d\n" distance; *)
            let extra_bits, distance_start = distance_code distance in
            bitmatch rest with
            | { distance : extra_bits;
                rest : -1 : bitstring } ->
              let distance = read_reversed extra_bits (Int64.to_int distance) in
              (distance + distance_start, rest)
      in
      (* Printf.printf "Distance %d\n" distance; *)

      let decoded, rest = decode_huffman rest in
      (Repeat (length, distance)::decoded, rest)
  (* Literal 0 - 143 *)
  | { element : 8;
      rest : -1 : bitstring } when element >= 48 && element <= 191 ->
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
      0 : 2;
      rest : -1 : bitstring } ->
    let rest = rest
      |> align_to_next_byte
      |> BS.to_string
      |> reverse_string_bits
      |> BS.of_string
    in
    (bitmatch rest with
    | { len : 16 : littleendian;
        nlen : 16 : littleendian;
        rest : -1 : bitstring } ->
      bitmatch rest with
      | { bytes : 8*len : string;
          rest : -1 : string } ->
        let rest = rest |> reverse_string_bits |> BS.of_string in
        ((final_block bfinal, Uncompressed bytes), rest))
  | { bfinal : 1;
      (* fixed Huffman, BTYPE=0b01, 0b10=2 in reversed *)
      2 : 2;
      rest : -1 : bitstring } ->
    let decoded, rest = decode_huffman rest in
    ((final_block bfinal, FixedHuffman decoded), rest)
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

let parse_adler32 bits =
  let bits = bits
  |> align_to_next_byte
  |> BS.take 32
  |> BS.to_string
  |> reverse_string_bits
  |> BS.of_string
  in
  bitmatch bits with
  | { adler32 : 32 } -> adler32

let rec parse_payload bits =
  let seg, rest = parse_segment bits in
  match seg with
  | Last, _ -> ([seg], rest)
  | Continues, _ ->
      let (segs, rest) = parse_payload rest in
      (seg :: segs, rest)

let parse_zlib bytestring =
  let bits = BS.of_string bytestring in
  bitmatch bits with
  | { header: 16: bitstring; blocks: -1: string } ->
    let payload, rest = blocks
      |> reverse_string_bits
      |> BS.of_string
      |> parse_payload in
    (parse_zlib_header header, payload, parse_adler32 rest)

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
  let bits = BS.of_string data in
  ignore @@ parse_payload bits;
  "Foo"
