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

  val Double = Token("double")
  val Float = Token("float")
  val Int32 = Token("int32")
  val Int64 = Token("int64")
  val Uint32 = Token("uint32")
  val Uint64 = Token("uint64")
  val Sint32 = Token("sint32")
  val Sint64 = Token("sint64")
  val Fixed32 = Token("fixed32")
  val Fixed64 = Token("fixed64")
  val Sfixed32 = Token("sfixed32")
  val Sfixed64 = Token("sfixed64")
  val Bool = Token("bool")
  val String = Token("string")
  val Bytes = Token("bytes")

  val Repeated = Token("repeated")
  val Reserved = Token("reserved")

  val Oneof = Token("oneof")
  val Map = Token("map")

  val Message = Token("message")
  val Enum = Token("enum")
  val Rpc = Token("rpc")
  val Service = Token("service")    
}
