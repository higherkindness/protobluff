package protobluff

import atto._
import Atto._
import org.specs2._
import org.scalacheck._

class ParserSpec extends Specification with ScalaCheck with Generators {
  def is = s2"""
  This is a specification of the grammar of protobuf3, taken from
  https://developers.google.com/protocol-buffers/docs/reference/proto3-spec


  Lexical tokens:

  should parse letter $letter
  should parse decimalDigit $decimalDigit
  should parse octalDigit $octalDigit
  should parse hexDigit $hexDigit
  should parse ident $ident
  should parse fullIdent $fullIdent
  should parse messageName $messageName
  should parse enumName $enumName
  should parse fieldName $fieldName
  should parse oneofName $oneofName
  should parse mapName $mapName
  should parse serviceName $serviceName
  should parse rpcName $rpcName
  should parse messageType $messageType
  should parse enumType $enumType
  should parse intLit $intLit
  should parse decimalLit $decimalLit
  should parse octalLit $octalLit
  should parse hexLit $hexLit
  should parse boolLit $boolLit
  should parse strLit $strLit
  should parse charValue $charValue
  should parse hexEscape $hexEscape
  should parse octEscape $octEscape
  should parse charEscape $charEscape
  should parse quote $quote
  should parse emptyStatement $emptyStatement
  should parse constant $constant


  should parse syntax $syntax
  should parse import $imprt
  should parse package $pkg
  should parse option $option
  should parse optionName $optionName
  should parse type $tpe
  should parse fieldNumber $fieldNumber
  should parse field $field
  should parse fieldOptions $fieldOptions
  should parse fieldOption $fieldOption
  should parse oneof $oneof
  should parse oneofField $oneofField
  should parse mapField $mapField
  should parse keyType $keyType
  should parse reserved $reserved
  should parse ranges $ranges
  should parse range $range
  should parse fieldNames $fieldNames
  should parse enum $enum
  should parse enumBody $enumBody
  should parse enumField $enumField
  should parse enumValueOption $enumValueOption
  should parse message $message
  should parse messageBody $messageBody
  should parse service $service
  should parse rpc $rpc
  should parse proto $proto
  should parse topLevelDef $topLevelDef
  """

  def letter = Prop.forAll(Gen.alphaChar) { c =>
    parser.letter.parse(c.toString).done.either must beRight(c)
  }
  def decimalDigit = Prop.forAll(Gen.numChar) { c =>
    parser.decimalDigit.parse(c.toString).done.either must beRight(c)
  }
  def octalDigit = Prop.forAll(Gen.oneOf('0', '7')) { c =>
    parser.octalDigit.parse(c.toString).done.either must beRight(c)
  }
  def hexDigit = Prop.forAll(Gen.oneOf(
                               Gen.oneOf('0', '7'),
                               Gen.oneOf('A', 'F'),
                               Gen.oneOf('a', 'b'))) { c =>
    parser.hexDigit.parse(c.toString).done.either must beRight(c)
  }
  def ident = Prop.forAll(identGen) { i =>
    parser.ident.parse(i).done.either must beRight(i)
  }
  def fullIdent = Prop.forAll(fullIdentGen) { i =>
    parser.fullIdent.parse(i).done.either must beRight(i)
  }
  def messageName = 1 must_== 33
  def enumName = 1 must_== 33
  def fieldName = 1 must_== 33
  def oneofName = 1 must_== 33
  def mapName = 1 must_== 33
  def serviceName = 1 must_== 33
  def rpcName = 1 must_== 33
  def messageType = 1 must_== 33
  def enumType = 1 must_== 33
  def intLit = 1 must_== 33
  def decimalLit = 1 must_== 33
  def octalLit = 1 must_== 33
  def hexLit = 1 must_== 33
  def boolLit = 1 must_== 33
  def strLit = 1 must_== 33
  def charValue = 1 must_== 33
  def hexEscape = 1 must_== 33
  def octEscape = 1 must_== 33
  def charEscape = 1 must_== 33
  def quote = 1 must_== 33
  def emptyStatement = 1 must_== 33
  def constant = 1 must_== 33
  def syntax = 1 must_== 33
  def imprt = 1 must_== 33
  def pkg = 1 must_== 33
  def option = 1 must_== 33
  def optionName = 1 must_== 33
  def tpe = 1 must_== 33
  def fieldNumber = 1 must_== 33
  def field = 1 must_== 33
  def fieldOptions = 1 must_== 33
  def fieldOption = 1 must_== 33
  def oneof = 1 must_== 33
  def oneofField = 1 must_== 33
  def mapField = 1 must_== 33
  def keyType = 1 must_== 33
  def reserved = 1 must_== 33
  def ranges = 1 must_== 33
  def range = 1 must_== 33
  def fieldNames = 1 must_== 33
  def enum = 1 must_== 33
  def enumBody = 1 must_== 33
  def enumField = 1 must_== 33
  def enumValueOption = 1 must_== 33
  def message = 1 must_== 33
  def messageBody = 1 must_== 33
  def service = 1 must_== 33
  def rpc = 1 must_== 33
  def proto = 1 must_== 33
  def topLevelDef = 1 must_== 33
  
}
  
