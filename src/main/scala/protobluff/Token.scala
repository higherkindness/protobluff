package protobluff

final case class Token(str: String)

object Token {
  val Inf = Token("inf")
  val NaN = Token("nan")
  val True = Token("true")
  val False = Token("false")
  val Import = Token("import")
  val Weak = Token("weak")
  val Public = Token("public")
  val Package = Token("package")

  val Dot = Token(".")
  val Comma = Token(",")
  val Semicolon = Token(";")
  val Equals = Token("=")
  val SquareBracketOpen = Token("[")
  val SquareBracketClose = Token("]")
  val CurlyBracketOpen = Token("{")
  val CurlyBracketClose = Token("}")
  val ParenOpen = Token("(")
  val ParenClose = Token(")")

  val To = Token("to")
  val Max = Token("max")

  val Repeated = Token("repeated")
  val Reserved = Token("reserved")

  val Message = Token("message")
  val Enum = Token("enum")
  val Rpc = Token("rpc")
  val Service = Token("service")
}
