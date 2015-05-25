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

let bfinal_of_bit = function
  | true -> Last
  | _ -> Continues

let btype_of_bit = function
  | 0 -> Uncompressed
  | 1 -> FixedHuffman
  | 2 -> DynamicHuffman
  | _ -> Reserved

let parse_segment bitstring =
  bitmatch bitstring with
  | { bfinal : 1; btype : 2; _: -1: bitstring} ->
    (bfinal_of_bit bfinal, btype_of_bit btype, Payload "TODO")

let parse_payload bits =
  [parse_segment bits;]

let parse_zlib bytestring =
  let bits = Bitstring.bitstring_of_string bytestring in
  bitmatch bits with
  | { header: 32: bitstring; data: -1: bitstring } ->
    (parse_zlib_header header, parse_payload data)

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
