package protobluff

import atto._
import Atto._

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

  val SingleQuote = Token("'")
  val DoubleQuote = Token("\"")
  val Dot = Token(".")
  val Comma = Token(",")
  val Semicolon = Token(";")
  val Underscore = Token("_")
  val Equals = Token("=")
  val SquareBracketOpen = Token("[")
  val SquareBracketClose = Token("]")
  val CurlyBracketOpen = Token("{")
  val CurlyBracketClose = Token("}")
  val ParenOpen = Token("(")
  val ParenClose = Token(")")
  val AngleBracketOpen = Token("<")
  val AngleBracketClose = Token(">")

  val To = Token("to")
  val Max = Token("max")

  val Repeated = Token("repeated")
  val Reserved = Token("reserved")

  val Message = Token("message")
  val Enum = Token("enum")
  val Rpc = Token("rpc")
  val Service = Token("service")

  /**
    * parse a lexical token
    */
  def token(tok: Token): Parser[String] = string(tok.str)
  val inf = token(Inf)
  val nan = token(NaN)
  val `true` = token(True)
  val `false` = token(False)
  val `import` = token(Import)
  val weak = token(Weak)
  val public = token(Public)
  val `package` = token(Package)
  val singleQuote = token(SingleQuote)
  val doubleQuote = token(DoubleQuote)
  val dot = token(Dot)
  val comma = token(Comma)
  val semicolon = token(Semicolon)
  val underscore = token(Underscore)
  val equals = token(Equals)
  val squareBracketOpen = token(SquareBracketOpen)
  val squareBracketClose = token(SquareBracketClose)
  val curlyBracketOpen = token(CurlyBracketOpen)
  val curlyBracketClose = token(CurlyBracketClose)
  val parenOpen = token(ParenOpen)
  val parenClose = token(ParenClose)
  val to = token(To)
  val max = token(Max)
  val repeated = token(Repeated)
  val reserved = token(Reserved)
  val message = token(Message)
  val enum = token(Enum)
  val rpc = token(Rpc)
  val service = token(Service)
  val angleBracketOpen = token(AngleBracketOpen)
  val angleBracketClose = token(AngleBracketClose)

}
