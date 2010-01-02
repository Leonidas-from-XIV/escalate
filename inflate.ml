type blockType = Uncompressed | FixedHuffman | DynamicHuffman | Reserved
type blockFinal = Continues | Last
type compressedContent = Payload of string
type deflateBlock = ( blockFinal * blockType * compressedContent )

let parse bytestring = [ (Last, Uncompressed, Payload "foo") ]

let inflate = function
        | _ -> "Foo"
