package debugging

import java.nio.charset.Charset
import scala.jdk.CollectionConverters.*

def findPairs(src: String, dst: String): Iterable[(Charset, Charset)] =
  for
    (name1, charset1) <- Charset.availableCharsets().asScala
    if charset1.canEncode()
    (name2, charset2) <- Charset.availableCharsets().asScala
    if charset2.decode(charset1.encode(src)).toString == dst
  yield (charset1, charset2)
