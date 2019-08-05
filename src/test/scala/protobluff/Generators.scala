package protobluff

import cats._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators {

  val identGen: Gen[String] = Gen.alphaChar.map(_.toString) |+| Gen.listOf(
    Gen.oneOf(
      Gen.alphaChar,
      Gen.numChar
    )
  ).map(_.mkString)

  val fullIdentGen: Gen[String] = identGen |+| Gen.listOf(identGen).map(_.mkString("."))

  val messageTypeGen: Gen[String] = Gen.option(Gen.const(".")).map(_.getOrElse("")) |+| fullIdentGen

  val hexDigitGen: Gen[String] = Gen.oneOf(
    Gen.oneOf('0', '9'),
    Gen.oneOf('A', 'F'),
    Gen.oneOf('a', 'f')).map(_.toString)

  val decimalLitGen: Gen[String] = Gen.nonEmptyListOf(Gen.numChar).map(_.mkString)

  val octalLitGen: Gen[String] = Gen.const("0") |+| Gen.listOf(Gen.oneOf('0' to '7')).map(_.mkString)

  val hexLitGen: Gen[String] = Gen.const("0") |+|
    Gen.oneOf("x", "X") |+|
    Gen.nonEmptyListOf(hexDigitGen).map(_.mkString)

  val intLitGen: Gen[String] = Gen.oneOf(decimalLitGen, octalLitGen, hexLitGen)

}
