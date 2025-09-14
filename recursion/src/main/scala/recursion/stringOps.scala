package recursion

def stringLength(s: String): Int =
  if s.isEmpty then 0
  else 1 + stringLength(s.tail)

def capitalizeString(s: String): String =
  if s.isEmpty then ""
  else s.head.toUpper.toString + capitalizeString(s.tail)

def discardWord(s: String): String =
  if s.isEmpty then ""
  else if s.head.isWhitespace then s
  else discardWord(s.tail)

def wordCount(s: String): Int =
  if s.isEmpty then 0
  else if s.head.isWhitespace then wordCount(s.tail)
  else 1 + wordCount(discardWord(s))

def isBlank(s: String): Boolean =
  if s.isEmpty then true
  else if s.head.isWhitespace then isBlank(s.tail)
  else false

def caesarCipher(s: String, shift: Int): String =
  if s.isEmpty then ""
  else ((s.head.toInt + shift - 'a'.toInt) % 26 + 'a'.toInt).toChar.toString + caesarCipher(s.tail, shift)

def reverseString(s: String): String =
  if s.isEmpty then ""
  else reverseString(s.tail) + s.head.toString