import com.monovore.decline.Opts
import com.monovore.decline.Command
import java.nio.file.Files
import java.nio.file.Paths
import scodec.bits.BitVector
import com.monovore.decline.CommandApp
import com.github.lavrov.bencode.Bencode.BDictionary

object Main
    extends CommandApp(
      Command(
        name = "bencode",
        header = "Print bencode"
      )(
        Opts.option[String]("torrent", "Path to torrent file").map { path =>
          val bytes = Files.readAllBytes(Paths.get(path))
          com.github.lavrov.bencode.decode(BitVector(bytes)) match {
            case Left(err) =>
              println(s"Invalid bencode file $err")
            case Right(value) =>
              pprint.pprintln(value)
              value match {
                case BDictionary(values) =>
                  values.get("info") match {
                    case None =>
                    case Some(value) =>
                      val infoHash = com.github.lavrov.bencode.encode(value).digest("SHA-1").toHex
                      println(s"Info-hash: $infoHash")
                  }
                case _ =>
              }
          }
        }
      )
    )
