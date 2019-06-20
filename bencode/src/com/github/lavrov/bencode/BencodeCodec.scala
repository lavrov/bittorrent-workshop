package com.github.lavrov.bencode

import scodec.Codec
import scodec.codecs._
import scodec.Attempt
import scodec.Err
import scodec.SizeBound
import scodec.DecodeResult
import scodec.bits.BitVector
import scodec.Encoder
import scodec.Decoder
import scodec.bits.ByteVector

object BencodeCodec {
  val instance: Codec[Bencode] = {
    lazily(???) // Implement codec!
  }

  /**
    * Codec that encodes/decodes a `List[A]` from a `Codec[A]`.
    *
    * When encoding, each `A` in the list is encoded and all of the resulting vectors are concatenated.
    *
    * When decoding, `codec.decode` is called repeatedly until first error or
    * there are no more remaining bits and the value result of each `decode` is returned in the list.
    *
    * @param codec codec to encode/decode a single element of the sequence
    */
  private def listSuccessful[A](codec: Codec[A]): Codec[List[A]] = new Codec[List[A]] {
    def sizeBound: SizeBound = SizeBound.unknown
    def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] =
      decodeCollectSuccessful[List, A](codec, None)(bits)
    def encode(value: List[A]): Attempt[BitVector] =
      Encoder.encodeSeq(codec)(value)
  }

  private def decodeCollectSuccessful[F[_], A](dec: Decoder[A], limit: Option[Int])(
      buffer: BitVector
  )(implicit cbf: collection.generic.CanBuildFrom[F[A], A, F[A]]): Attempt[DecodeResult[F[A]]] = {
    val bldr = cbf()
    var remaining = buffer
    var count = 0
    val maxCount = limit getOrElse Int.MaxValue
    var error = false
    while (count < maxCount && !error && remaining.nonEmpty) {
      dec.decode(remaining) match {
        case Attempt.Successful(DecodeResult(value, rest)) =>
          bldr += value
          count += 1
          remaining = rest
        case Attempt.Failure(_) =>
          error = true
      }
    }
    Attempt.successful(DecodeResult(bldr.result, remaining))
  }
}
