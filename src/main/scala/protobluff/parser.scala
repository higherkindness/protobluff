package protobluff

import Function.{const => κ}

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
    * octalDigit   = "0" … "7"
    *
    * @group lexical
    */
  val octalDigit: Parser[Char] = Atto.octalDigit

  /**
    * hexDigit     = "0" … "9" | "A" … "F" | "a" … "f"
    *
    * @group lexical
    */
  val hexDigit: Parser[Char] = Atto.hexDigit

  /**
    * ident = letter { letter | decimalDigit | "_" }
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
    * constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit 
    * 
    * @group lexical
    */
  val constant: Parser[String] = fullIdent | intLit | floatLit | strLit | boolLit.map(_.toString)

  /**
    * messageName = ident
    *
    * @group lexical
    */
  val messageName: Parser[String] = ident

  /**
    * enumName = ident
    *
    * @group lexical
    */
  val enumName: Parser[String] = ident

  /**
    * fieldName = ident
    *
    * @group lexical
    */
  val fieldName: Parser[String] = ident

  /**
    * oneofName = ident
    *
    * @group lexical
    */
  val oneofName: Parser[String] = ident

  /**
    * mapName = ident
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
    * rpcName = ident
    *
    * @group lexical
    */
  val rpcName: Parser[String] = ident

  /**
    * messageType = [ "." ] { ident "." } messageName
    *
    * @group lexical
    */
  val messageType: Parser[String] = for {
      initial <- opt(dot)
      middle  <- sepBy(ident, dot)
    } yield initial.fold("")(_.toString) ++ middle.mkString(".")

  /**
    * enumType = [ "." ] { ident "." } enumName
    *
    * @group lexical
    */
  val enumType: Parser[String] = messageType

  /**
    * decimalLit = ( "1" … "9" ) { decimalDigit }
    *
    * @group lexical
    */
  val decimalLit: Parser[String] = many1(digit).map(_.mkString_("", "", ""))

  /**
    * octalLit   = "0" { octalDigit }
    *
    * @group lexical
    */
  val octalLit: Parser[String] = char('0') >>= { z =>
      many(octalDigit).map(nel => "0" ++ nel.mkString_("", "", ""))
    }

  /**
    * hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
    *
    * @group lexical
    */
  val hexLit: Parser[String] = for {
    z <- string("0")
    x <- (string("x") | string("X"))
    n <- many1(hexDigit).map(_.mkString_("", "", ""))
  } yield z ++ x ++ n

  /**
    * intLit     = decimalLit | octalLit | hexLit
    *
    * @group lexical
    */
  val intLit: Parser[String] = decimalLit | octalLit | hexLit

  /**
    * decimals  = decimalDigit { decimalDigit }
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
    * exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
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
    * parse a lexical token
    * 
    * @group lexical
    */
  def token(tok: Token): Parser[String] = string(tok.str)

  /**
    * import = "import" [ "weak" | "public" ] strLit ";"
    *
    * @group import
    */
  val `import`: Parser[Import] = (string("import") >>
                                    skipWhitespace >>
                                    opt(string("weak").as(Import.Type.weak) | string("public").as(Import.Type.public)) << skipWhitespace
                                    ,
                                  strLit  << skipWhitespace << char(';') << skipWhitespace).mapN(Import.apply)

  /**
    * package = "package" fullIdent ";"
    *
    * @group package
    */
  val pkg: Parser[Package] = for {
    _ <- string("package") << skipWhitespace
    name <- fullIdent << skipWhitespace << string(";")
  } yield Package(name)

  /**
    * option = "option" optionName  "=" constant ";"
    *
    * @group option
    */


  /**
    * optionName = ( ident | "(" fullIdent ")" ) { "." ident }
    *
    * @group option
    */
  val optionName = ident | parens(fullIdent)


  /**
    * type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string" | "bytes" | messageType | enumType
    *
    * @group fields
    */
  val `type`: Parser[Type] = string("double").as(Type.TDouble.conceal) |
    string("float").as(Type.TFloat.conceal) |
    string("int32").as(Type.TInt32.conceal) |
    string("int64").as(Type.TInt64.conceal) |
    string("uint32").as(Type.TUint32.conceal) |
    string("uint64").as(Type.TUint64.conceal) |
    string("sint32").as(Type.TSint32.conceal) |
    string("sint64").as(Type.TSint64.conceal) |
    string("fixed32").as(Type.TFixed32.conceal) |
    string("fixed64").as(Type.TFixed64.conceal) |
    string("sfixed32").as(Type.TSfixed32.conceal) |
    string("sfixed64").as(Type.TSfixed64.conceal) |
    string("bool").as(Type.TBool.conceal) |
    string("string").as(Type.TString.conceal) |
    string("bytes").as(Type.TBytes.conceal) |
    messageType.map(n => Type.TNamedType(n).conceal) |
    enumType.map(n => Type.TNamedType(n).conceal)

  /**
    * fieldNumber = intLit;
    * @group fields
    */
  val fieldNumber: Parser[Int] = intLit.map(_.toInt)

  /**
    * fieldOption = optionName "=" constant
    *
    * @group fields
    */
  val fieldOption = for {
    name <- optionName << skipWhitespace
    _ <- string("=") << skipWhitespace
    value <- constant
    } yield OptionValue(name, value)

   val options: Parser[List[OptionValue]] = opt(squareBrackets(sepBy(fieldOption, skipWhitespace >> string(",") << skipWhitespace))).map(_.fold(List.empty[OptionValue])(identity))

  /**
    * oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    * @group fields
    */
  val oneofField: Parser[Field] = for {
    tpe <- `type` << skipWhitespace
    name <- fieldName << skipWhitespace
    _ <- string("=") << skipWhitespace
    position <- fieldNumber << skipWhitespace
    opts <- options
    _ <- string(";") << skipWhitespace
  } yield Field(name, tpe, position, opts, false, false)


  /**
    * field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    * @group fields
    */
  val field: Parser[Field] = for {
    isRepeated <- opt(string("repeated")).map(_.fold(false)(κ(true))) << skipWhitespace
    field <- oneofField
  } yield field.copy(isRepeated = isRepeated)

  /**
    * oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
    * 
    * @group fields
    */
  val oneof: Parser[Type.TOneOf] = for {
      _ <- string("oneof") << skipWhitespace
      name <- oneofName << skipWhitespace
      fields <- braces(sepBy(oneofField, skipWhitespace)) << skipWhitespace
    } yield Type.TOneOf(name, fields)

  /**
    * mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    * 
    * @group fields
    */
  val mapField = for {
    _ <- string("map") << skipWhitespace
    _ <- char('<') >> skipWhitespace
    key <- `type` << skipWhitespace
    _ <- char(',') >> skipWhitespace
    value <- `type` << skipWhitespace
    _ <- char('>') << skipWhitespace
    name <- mapName << skipWhitespace
    _ <- char('=') << skipWhitespace
    number <- fieldNumber << skipWhitespace
    opts <- options
  } yield Field(name, Type.TMap(key, value), number, opts, false, true)

  /**
    * keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
    */
  /**
    * reserved = "reserved" ( ranges | fieldNames ) ";"
    */
  /**
    * ranges = range { "," range }
    */
  /**
    * range =  intLit [ "to" ( intLit | "max" ) ]
    */
  /**
    * fieldNames = fieldName { "," fieldName }
    */
  /**
    * enum = "enum" enumName enumBody
    */
  /**
    * enumBody = "{" { option | enumField | emptyStatement } "}"
    */
  /**
    * enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
    */
  /**
    * enumValueOption = optionName "=" constant
    */
  /**
    * message = "message" messageName messageBody
    */
  /**
    * messageBody = "{" { field | enum | message | option | oneof | mapField | reserved | emptyStatement } "}"
    */
  /**
    * service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
    */
  /**
    * rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ] messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
    */
  /**
    * proto = syntax { import | package | option | topLevelDef | emptyStatement }
    */
  /**
    * topLevelDef = message | enum | service
    */
}
