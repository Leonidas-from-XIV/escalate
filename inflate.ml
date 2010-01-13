(* deflate *)
type blockType = Uncompressed | FixedHuffman | DynamicHuffman | Reserved
type blockFinal = Continues | Last
type deflatedContent = Payload of string
type deflateBlock = ( blockFinal * blockType * deflatedContent )

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

type zlib_header = { cm : int; cinfo : int; flevel : int}

let parse_zlib bytestring =
        let bits = Bitstring.bitstring_of_string bytestring in
        bitmatch bits with
        (*| { cm : 4 : littleendian } -> cm *)
        | { _ } -> 42

let parse_zlib_header bytestring =
        let bits = Bitstring.bitstring_of_string bytestring in
        bitmatch bits with
        | {
            cinfo : 4 : littleendian;
            cm : 4 :  littleendian;
            flevel : 2 : littleendian
          } -> { cm=cm; cinfo=cinfo; flevel=flevel }

let parse bytestring = [ (Last, Uncompressed, Payload "foo") ]

let inflate = function
        | _ -> "Foo"
