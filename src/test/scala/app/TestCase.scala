package app

import app.model._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class TestCase extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  val digitGen: Gen[Digit] = Gen.oneOf(allDigits)
  implicit def digitAGen: Arbitrary[Digit] = Arbitrary(digitGen)
}
