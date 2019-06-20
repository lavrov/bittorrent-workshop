package com.github.lavrov.bencode
import scodec.bits.BitVector
import scodec.Attempt
import scodec.Err

object `package` {
    def encode(value: Bencode): BitVector = BencodeCodec.instance.encode(value).require
    def decode(bits: BitVector): Either[Err, Bencode] = BencodeCodec.instance.decodeValue(bits).toEither
}