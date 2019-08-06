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
      rest <- many(Token.dot >>= { dot =>
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
      initial <- opt(Token.dot)
      middle  <- sepBy(ident, Token.dot)
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
  val floatLit: Parser[String] = Token.inf |
    Token.nan |
    (Token.dot <+> decimals <+> exponent)

  /**
    * boolLit = "true" | "false"
    *
    * @group lexical
    */
  val boolLit: Parser[Boolean] = Token.`true`.as(true) | Token.`false`.as(false)

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
  val quote: Parser[String] = Token.singleQuote | Token.doubleQuote

  /**
    * import = "import" [ "weak" | "public" ] strLit ";"
    *
    * @group import
    */
  val `import`: Parser[Import] = (Token.`import` >>
                                    skipWhitespace >>
                                    opt(Token.weak.as(Import.Type.weak) | Token.public.as(Import.Type.public)) << skipWhitespace
                                    ,
                                  strLit  << skipWhitespace << Token.semicolon << skipWhitespace).mapN(Import.apply)

  /**
    * package = "package" fullIdent ";"
    *
    * @group package
    */
  val pkg: Parser[Package] = for {
    _ <- Token.`package` << skipWhitespace
    name <- fullIdent << skipWhitespace << Token.semicolon
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
  val primitiveType: Parser[Type] = string("double").as(Type.TDouble.widen) |
    string("float").as(Type.TFloat.widen) |
    string("int32").as(Type.TInt32.widen) |
    string("int64").as(Type.TInt64.widen) |
    string("uint32").as(Type.TUint32.widen) |
    string("uint64").as(Type.TUint64.widen) |
    string("sint32").as(Type.TSint32.widen) |
    string("sint64").as(Type.TSint64.widen) |
    string("fixed32").as(Type.TFixed32.widen) |
    string("fixed64").as(Type.TFixed64.widen) |
    string("sfixed32").as(Type.TSfixed32.widen) |
    string("sfixed64").as(Type.TSfixed64.widen) |
    string("bool").as(Type.TBool.widen) |
    string("string").as(Type.TString.widen) |
    string("bytes").as(Type.TBytes.widen)

  val tpe: Parser[Type] = primitiveType |
    messageType.map(n => Type.TNamedType(n).widen) |
    enumType.map(n => Type.TNamedType(n).widen)

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
    _ <- Token.equals << skipWhitespace
    value <- constant
    } yield OptionValue(name, value)

   val options: Parser[List[OptionValue]] = opt(squareBrackets(sepBy(fieldOption, skipWhitespace >> Token.comma << skipWhitespace))).map(_.fold(List.empty[OptionValue])(identity))

  /**
    * oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    * @group fields
    */
  val oneofField: Parser[Field.OneOf.OneOfField] = for {
    tpe <- tpe << skipWhitespace
    name <- fieldName << skipWhitespace
    _ <- Token.equals << skipWhitespace
    position <- fieldNumber << skipWhitespace
    opts <- options
    _ <- Token.semicolon << skipWhitespace
  } yield Field.OneOf.OneOfField(name, tpe, position, opts)


  /**
    * field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    * @group fields
    */
  val normalField: Parser[Field.Normal] = for {
    isRepeated <- opt(Token.repeated).map(_.fold(false)(κ(true))) << skipWhitespace
    Field.OneOf.OneOfField(name, tpe, fn, opts) <- oneofField
  } yield Field.Normal(name, tpe, fn, opts, isRepeated)

  /**
    * oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
    *
    * @group fields
    */
  val oneof: Parser[Field.OneOf] = for {
      _ <- string("oneof") << skipWhitespace
      name <- oneofName << skipWhitespace
      fields <- braces(sepBy(oneofField, skipWhitespace)) << skipWhitespace
    } yield Field.OneOf(name, fields)

  /**
    * mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    *
    * @group fields
    */
  val mapField: Parser[Field.Map] = for {
    _ <- string("map") << skipWhitespace
    _ <- Token.angleBracketOpen >> skipWhitespace
    key <- primitiveType << skipWhitespace
    _ <- Token.comma >> skipWhitespace
    value <- tpe << skipWhitespace
    _ <- Token.angleBracketClose << skipWhitespace
    name <- mapName << skipWhitespace
    _ <- Token.equals << skipWhitespace
    number <- fieldNumber << skipWhitespace
    opts <- options
  } yield Field.Map(name, Type.TMap(key, value), number, opts)


  type Ranges = NonEmptyList[Either[String, Range]]
  type FieldNames = NonEmptyList[String]

  /**
    * reserved = "reserved" ( ranges | fieldNames ) ";"
    */
  val reserved: Parser[Either[Ranges, FieldNames]] = Token.reserved >> skipWhitespace >> either(ranges, fieldNames) << skipWhitespace << Token.semicolon << skipWhitespace

  /**
    * ranges = range { "," range }
    */
  val ranges: Parser[Ranges] = sepBy1(either(intLit, range), skipWhitespace >> Token.comma << skipWhitespace)

  /**
    * range =  intLit [ "to" ( intLit | "max" ) ]
    */
  val range: Parser[Range] = for {
      start <- intLit << skipWhitespace
      _ <- Token.to << skipWhitespace
      end <- (Token.max.as(none[String]) | intLit.map(_.some)) << skipWhitespace
    } yield Range(start, end)

  /**
    * fieldNames = fieldName { "," fieldName }
    */
  val fieldNames: Parser[FieldNames] = sepBy1(fieldName, skipWhitespace >> Token.comma << skipWhitespace)

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
