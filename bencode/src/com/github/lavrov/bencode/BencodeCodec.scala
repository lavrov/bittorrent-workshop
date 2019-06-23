package com.github.lavrov.bencode

import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

import scala.util.Try


object BencodeCodec {
  val instance: Codec[Bencode] = lazily {
    choice(bencodeString.upcast, bencodeInteger.upcast, bencodeList.upcast, bencodeDict.upcast)
  }
  
  object AsciiNumber {
    
    private val positiveAsciiDigit: Codec[Short] = {
      def decode(b: Byte): Attempt[Short] = {
        Try(Character.toString(b.toChar).toShort) match {
          case scala.util.Success(value) => Successful(value)
          case scala.util.Failure(_) => Failure(Err("Can not decode"))
        }
      }
      def encode(c: Short) =
        Successful(c.toByte)
      
      byte.exmap(decode, encode)
    }
    
    val positiveAsciiNumber: Codec[Long] = {
      def listToLong(ds: List[Short]): Long = ds.reverse.zipWithIndex.foldLeft(0l){
        case (acc, (d, i)) => (d * scala.math.pow(10, i)).toLong + acc
      }
      
      def longToList(d: Long): List[Short] = d.toString.toList.map(_.toShort)
      
      listSuccessful(positiveAsciiDigit).xmap[Long](listToLong, longToList)
    }
    
    val asciiNumber: Codec[Long] = {
      recover(constant('-')).consume{
        case true => positiveAsciiNumber.xmapc(d => -d)(d => -d)
        case false => positiveAsciiNumber
      }(d => d < 0)
    }
  }
  
  
  def bencodeInteger: Codec[Bencode.BInteger] = {
    val bci = AsciiNumber.asciiNumber.xmapc(d => Bencode.BInteger(d))(bd => bd.value)
    constant('i') ~> bci <~ constant('e')
  }
  
  def bencodeString: Codec[Bencode.BString] = {
    val stringLength = AsciiNumber.positiveAsciiNumber <~ constant(':')
    val str = stringLength.consume{ len =>
      bytes(len.toInt)
    }(s => s.length)
    str.xmapc(Bencode.BString(_))(_.value)
  }
  
  def bencodeList: Codec[Bencode.BList] = {
    val bcl = constant('l') ~> listSuccessful(instance) <~ constant('e')
    bcl.xmap(Bencode.BList, _.values)
  }
  
  def bencodeDict: Codec[Bencode.BDictionary] = {
    def bcTuple: Codec[(String, Bencode)] =
      (bencodeString ~ instance).xmapc{
        case (bs, v) => bs.value.decodeAscii match {
          case Right(value) => value -> v
          case Left(err) => throw err
        }
      }{
        case (s, v) => Bencode.BString(s) -> v
      }
    
    def bcDict: Codec[Bencode.BDictionary] = {
      val bcd = constant('d') ~> listSuccessful(bcTuple) <~ constant('e')
      bcd.xmapc(Bencode.BDictionary(_:_*))(_.values.toList)
    }
    
    bcDict
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
