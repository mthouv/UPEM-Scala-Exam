package com.fr.upem.partiel

import com.fr.upem.partiel.Part1._
import org.scalatest.{FlatSpec, Matchers}

class Part1Test extends FlatSpec with Matchers {

  "applyMul2WithPatternMatching" should "apply mul2 to given argument" in {
    applyMul2WithPatternMatching(Some(15)) should equal(Some(30))
    applyMul2WithPatternMatching(None) should equal(None)
  }

  "applyMul2WithoutPatternMatching" should "apply mul2 to given argument" in {
    applyMul2WithoutPatternMatching(Some(15)) should equal(Some(30))
    applyMul2WithoutPatternMatching(None) should equal(None)
  }

  "formatAnimal" should "serialize given animal" in {
    formatAnimal(Cat) should equal("It's a cat")
    formatAnimal(Dog(5)) should equal("It's a 5 year old dog")
    formatAnimal(Bird) should equal("It's a bird")
  }

  "indexOf" should "find index of given element in given list" in {
    indexOf(List("a", "b", "c"), "c") should equal(Some(2))
    indexOf(List("a", "b", "c"), "a") should equal(Some(0))
    indexOf(List("a", "b", "c"), "d") should equal(None)
  }

  "keepValid" should "ignore all errors" in {
    keepValid(List(Right(5))) should equal(List(5))
    keepValid(List(Left(Error("err")))) should equal(Nil)
    keepValid(List(Right(1), Left(Error("err")), Right(4))) should equal(List(1, 4))
  }

  "aggregate" should "combine elements" in {
    aggregate[Int](List(1, 2, 3), _ + _, 0) should equal(6)
    aggregate[String](List("Hello", " ", "world", "!"), _ + _, "") should equal("Hello world!")
    aggregate[String](Nil, _ + _, "") should equal("")
  }

  "aggregateValid" should "combine valid elements" in {
    aggregateValid[Int](List(Right(1), Left(Error("err")), Right(3)), _ + _, 0) should equal(4)
  }

  "computeTotalEarnings" should "compute earnings of given assets" in {
    val carSale = new CarSale(amount = 15000, horsePower = 5)
    val livretA = new LivretA(amount = 7500)

    computeTotalEarnings(List[FinancialAsset](carSale, livretA)) should equal(25625)
  }

}
