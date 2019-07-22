package protobluff

import org.specs2._

class ParserSpec extends Specification {
  def is = s2"""
  This is a specification of the grammar of protobuf3, taken from
  https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

letter = "A" … "Z" | "a" … "z"        $letter
decimalDigit = "0" … "9"        $decimalDigit
octalDigit   = "0" … "7"        $octalDigit
hexDigit     = "0" … "9" | "A" … "F" | "a" … "f"        $hexDigit
ident = letter { letter | decimalDigit | "_" }        $ident
fullIdent = ident { "." ident }        $fullIdent
messageName = ident        $messageName
enumName = ident        $enumName
fieldName = ident        $fieldName
oneofName = ident        $oneofName
mapName = ident        $mapName
serviceName = ident        $serviceName
rpcName = ident        $rpcName
messageType = [ "." ] { ident "." } messageName        $messageType
enumType = [ "." ] { ident "." } enumName        $enumType
intLit     = decimalLit | octalLit | hexLit        $intLit
decimalLit = ( "1" … "9" ) { decimalDigit }        $decimalLit
octalLit   = "0" { octalDigit }        $octalLit
hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }        $hexLit
intLit     = decimalLit | octalLit | hexLit        $intLit
decimalLit = ( "1" … "9" ) { decimalDigit }        $decimalLit
octalLit   = "0" { octalDigit }        $octalLit
hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }        $hexLit
boolLit = "true" | "false"        $boolLit
strLit = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )        $strLit
charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/        $charValue
hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit        $hexEscape
octEscape = '\' octalDigit octalDigit octalDigit        $octEscape
charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )        $charEscape
quote = "'" | '"'        $quote
emptyStatement = ";"        $emptyStatement
constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit        $constant
syntax = "syntax" "=" quote "proto3" quote ";"        $syntax
import = "import" [ "weak" | "public" ] strLit ";"        $import
package = "package" fullIdent ";"        $package
option = "option" optionName  "=" constant ";"        $option
optionName = ( ident | "(" fullIdent ")" ) { "." ident }        $optionName
type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string" | "bytes" | messageType | enumType        $type
fieldNumber = intLit;        $fieldNumber
field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"        $field
fieldOptions = fieldOption { ","  fieldOption }        $fieldOptions
fieldOption = optionName "=" constant        $fieldOption
oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"        $oneof
oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"        $oneofField
mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"        $mapField
keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"        $keyType
reserved = "reserved" ( ranges | fieldNames ) ";"        $reserved
ranges = range { "," range }        $ranges
range =  intLit [ "to" ( intLit | "max" ) ]        $range
fieldNames = fieldName { "," fieldName }        $fieldNames
enum = "enum" enumName enumBody        $enum
enumBody = "{" { option | enumField | emptyStatement } "}"        $enumBody
enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"        $enumField
enumValueOption = optionName "=" constant        $enumValueOption
message = "message" messageName messageBody        $message
messageBody = "{" { field | enum | message | option | oneof | mapField |        $messageBody
reserved | emptyStatement } "}"        $reserved
service = "service" serviceName "{" { option | rpc | emptyStatement } "}"        $service
rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]        $rpc
messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")        $messageType
proto = syntax { import | package | option | topLevelDef | emptyStatement }        $proto
topLevelDef = message | enum | service        $topLevelDef
  """
}
