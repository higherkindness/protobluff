package protobluff

import cats._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.cats.implicits._

trait Generators {

  def identGen: Gen[String] = Gen.alphaChar.map(_.toString) |+| Gen.listOf(
      Gen.oneOf(
        Gen.alphaChar,
        Gen.numChar
      )
    ).map(_.mkString)

  def fullIdentGen: Gen[String] = identGen |+| Gen.listOf(identGen).map(_.mkString("."))

}
