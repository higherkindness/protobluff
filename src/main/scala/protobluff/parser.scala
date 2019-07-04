package protobluff

import atto._
import Atto._
import cats._
import cats.implicits._
import cats.data.NonEmptyList

/** parser package provides parsers for all parts of Protobuf grammar.
 *
 * `|   alternation` <br>
 * `()  grouping`  <br>
 * `[]  option (zero or one time)`  <br>
 * `{}  repetition (any number of times)`  <br>
 *
 * @groupname lexical Lexical elements
 * @groupdesc lexical basic lexical elements
 * @groupname syntax Syntax
 * @groupdesc syntax Syntax
 * @groupname importStatement Import Statement
 * @groupdesc importStatement Import Statement
 * @groupname package Package
 * @groupdesc package Package
 * @groupname option Option
 * @groupdesc option Option
 * @groupname fields Fields
 * @groupdesc fields Fields
 * @groupname reserved Reserved
 * @groupdesc reserved Reserved
 * @groupname tld Top Level Definitions
 * @groupdesc tld Top Level Definitions
 * @groupname protofile Proto file
 * @groupdesc protofile Proto file
 *
 * @see https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
 */
object parser {
  val dot = char('.')

  /**
   * letter = "A" … "Z" | "a" … "z"
   *
   * @group lexical
   */
  val letter: Parser[Char] = Atto.letter

  /**
   * decimalDigit = "0" … "9"
   *
   * @group lexical
   */
  val decimalDigit: Parser[Char] = Atto.digit

  /**
   *  octalDigit   = "0" … "7"
   *
   * @group lexical
   */
  val octalDigit: Parser[Char] = Atto.octalDigit

  /**
   *  hexDigit     = "0" … "9" | "A" … "F" | "a" … "f"
   *
   * @group lexical
   */
  val hexDigit: Parser[Char] = Atto.hexDigit

  /**
   *  ident = letter { letter | decimalDigit | "_" }
   *
   * @group lexical
   */
  val ident: Parser[String] = for {
    first <- letter
    rest  <- many(letter | decimalDigit | char('_'))
  } yield (first :: rest).mkString

  /**
   * fullIdent = ident { "." ident }
   *
   * @group lexical
   */
  val fullIdent: Parser[String] = for {
    first <- ident
    rest <- many(char('.') >>= { dot =>
      ident.map("." ++ _)
    })
  } yield (first :: rest).mkString

  /**
   *  messageName = ident
   *
   * @group lexical
   */
  val messageName: Parser[String] = ident

  /**
   *  enumName = ident
   *
   * @group lexical
   */
  val enumName: Parser[String] = ident

  /**
   *  fieldName = ident
   *
   * @group lexical
   */
  val fieldName: Parser[String] = ident

  /**
   *  oneofName = ident
   *
   * @group lexical
   */
  val oneofName: Parser[String] = ident

  /**
   *  mapName = ident
   *
   * @group lexical
   */
  val mapName: Parser[String] = ident

  /**
   * serviceName = ident
   *
   * @group lexical
   */
  val serviceName: Parser[String] = ident

  /**
   *  rpcName = ident
   *
   * @group lexical
   */
  val rpcName: Parser[String] = ident

  /**
   *  messageType = [ "." ] { ident "." } messageName
   *
   * @group lexical
   */
  val messageType: Parser[String] = for {
    initial <- opt(dot)
    middle  <- sepBy(ident, dot)
  } yield initial.fold("")(_.toString) ++ middle.mkString(".")

  /**
   *  enumType = [ "." ] { ident "." } enumName
   *
   * @group lexical
   */
  val enumType: Parser[String] = messageType

  /**
   *  decimalLit = ( "1" … "9" ) { decimalDigit }
   *
   * @group lexical
   */
  val decimalLit: Parser[String] = many1(digit).map(_.mkString_("", "", ""))

  /**
   *  octalLit   = "0" { octalDigit }
   *
   * @group lexical
   */
  val octalLit: Parser[String] = char('0') >>= { z =>
    many1(octalDigit).map(nel => "0" ++ nel.mkString_("", "", ""))
  }

  /**
   *  hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
   *
   * @group lexical
   */
  val hexLit: Parser[String] = (char('0') >> (char('x') | char('X'))) >>
    many1(hexDigit).map(nel => "0x" ++ nel.mkString_("", "", ""))

  /**
   *  intLit     = decimalLit | octalLit | hexLit
   *
   * @group lexical
   */
  val intLit: Parser[String] = decimalLit | octalLit | hexLit

  /**
   *  decimals  = decimalDigit { decimalDigit }
   *
   * {{{
   * scala> decimals.parse("1234321").done
   * res0: atto.ParseResult[String] = Done(,1234321)
   * }}}
   *
   * @group lexical
   */
  val decimals: Parser[String] = many(decimalDigit).map(_.mkString)

  /**
   *  exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
   *
   * @group lexical
   */
  val exponent: Parser[String] = (char('e') | char('E')) >> opt(char('+') | char('-')) >>= { sign =>
    decimals.map(dec => "e" |+| sign.map(_.toString).getOrElse("") |+| dec)
  }

  /**
   * floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "." decimals [ exponent ] ) | "inf" | "nan"
   *
   * @group lexical
   */
  val floatLit: Parser[String] = string("inf") |
    string("nan") |
    (char('.').map(_.toString) <+> decimals <+> exponent)

  /**
   * boolLit = "true" | "false"
   *
   * @group lexical
   */
  val boolLit: Parser[Boolean] = string("true").as(true) | string("false").as(false)

  /**
   * strLit = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )
   *
   * @group lexical
   */
  val strLit: Parser[String] = stringLiteral

  /**
   * quote = "'" | '"'
   *
   * @group lexical
   */
  val quote: Parser[Char] = char('"') | char(''')

  /**
   *  import = "import" [ "weak" | "public" ] strLit ";"
   *
   * @group import
   */
  val `import`: Parser[Import] = (string("import") <+>
    skipWhitespace.as("") <+>
    opt(string("weak") | string("public")).map(_.map()),
    strLit).mapN(Import.apply)

  /**
   *  package = "package" fullIdent ";"
   *
   * @group package
   */
  /**
   *  option = "option" optionName  "=" constant ";"
   *
   * @group option
   */
  /**
   *  optionName = ( ident | "(" fullIdent ")" ) { "." ident }
   *
   * @group option
   */
  /**
   *  type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string" | "bytes" | messageType | enumType
   *
   * @group fields
   */
  val `type`: Parser[Type] = string("double").as(Type.TDouble) |
    string("float").as(Type.TFloat) |
    string("int32").as(Type.TInt32) |
    string("int64").as(Type.TInt64) |
    string("uint32").as(Type.TUint32) |
    string("uint64").as(Type.TUint64) |
    string("sint32").as(Type.TSint32) |
    string("sint64").as(Type.TSint64) |
    string("fixed32").as(Type.TFixed32) |
    string("fixed64").as(Type.TFixed64) |
    string("sfixed32").as(Type.TSfixed32) |
    string("sfixed64").as(Type.TSfixed64) |
    string("bool").as(Type.TBool) |
    string("string").as(Type.TString) |
    string("bytes").as(Type.TBytes) |
    messageType |
    enumType
  /**
   *  fieldNumber = intLit;
   */
  /**
   *  field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
   */
  /**
   *  fieldOptions = fieldOption { ","  fieldOption }
   */
  /**
   *  fieldOption = optionName "=" constant
   */
  /**
   *  oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
   */
  /**
   *  oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
   */
  /**
   *  mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
   */
  /**
   *  keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
   */
  /**
   *  reserved = "reserved" ( ranges | fieldNames ) ";"
   */
  /**
   *  ranges = range { "," range }
   */
  /**
   *  range =  intLit [ "to" ( intLit | "max" ) ]
   */
  /**
   *  fieldNames = fieldName { "," fieldName }
   */
  /**
   *  enum = "enum" enumName enumBody
   */
  /**
   *  enumBody = "{" { option | enumField | emptyStatement } "}"
   */
  /**
   *  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
   */
  /**
   *  enumValueOption = optionName "=" constant
   */
  /**
   *  message = "message" messageName messageBody
   */
  /**
   *  messageBody = "{" { field | enum | message | option | oneof | mapField | reserved | emptyStatement } "}"
   */
  /**
   *  service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
   */
  /**
   *  rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ] messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
   */
  /**
   *  proto = syntax { import | package | option | topLevelDef | emptyStatement }
   */
  /**
 *  topLevelDef = message | enum | service
 */
}
