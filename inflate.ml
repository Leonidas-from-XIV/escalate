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

let parse_zlib bytestring =
        let bits = Bitstring.bitstring_of_string bytestring in
        bitmatch bits with
        (*| { cm : 4 : littleendian } -> cm *)
        | { _ } -> 42

let parse bytestring = [ (Last, Uncompressed, Payload "foo") ]

let inflate = function
        | _ -> "Foo"
