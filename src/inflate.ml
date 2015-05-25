(* deflate *)
type blockType = Uncompressed | FixedHuffman | DynamicHuffman | Reserved
type blockFinal = Continues | Last
type deflatedContent = Payload of string
type deflateBlock = (blockFinal * blockType * deflatedContent)

(* zlib *)
(*
type compressionMethod = Deflate | NotDeflate
type compressionInfo = WindowSize of int
type dictPresent = bool
type presetDictionary = Dictionary
type compressionLevel = FastestCompression | FastCompression | DefaultCompression | MaximalCompression
type checksumHeader = int
type compressedContent = list deflateBlock
type checksumData = Adler32 of int
type zlibBlock = ( compressionMethod * compressionInfo * checksumHeader *
dictPresent * compressionLevel * presetDictionary * compressedContent * checksumData )
*)

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

let parse_segment bitstring =
  bitmatch bitstring with
  | { bfinal : 1;
      (* Non-compressed block, BTYPE=00 *)
      0 : 2;
      len : 2 : offset(8);
      nlen : 2;
      content : len : string;
      rest : -1 : bitstring } ->
    ((final_block bfinal, Uncompressed, Payload content), rest)
  | { bfinal : 1;
      (* fixed Huffman, BTYPE=01 *)
      1 : 2;
      rest : -1 : bitstring } ->
    ((final_block bfinal, FixedHuffman, Payload "TODO"), rest)
  | { bfinal : 1;
      (* dynamic Huffman, BTYPE=10 *)
      2 : 2;
      rest : -1 : bitstring } ->
    ((final_block bfinal, DynamicHuffman, Payload "TODO"), rest)
  | { bfinal : 1;
      (* reserved, BTYPE=11, fail *)
      3 : 2} ->
    failwith "Reserved block, invalid bitstream"

let rec parse_payload bits =
  let seg, rest = parse_segment bits in
  match seg with
  | Last, _, _ -> [seg]
  (* | Continues, _, _ -> seg :: parse_payload rest *)
  | Continues, _, _ -> [seg]

let parse_zlib bytestring =
  let bits = Bitstring.bitstring_of_string bytestring in
  bitmatch bits with
  | { header: 16: bitstring; blocks: -1: bitstring } ->
    (parse_zlib_header header, parse_payload blocks)

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
