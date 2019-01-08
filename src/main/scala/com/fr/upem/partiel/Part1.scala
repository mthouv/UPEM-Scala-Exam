package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS

import scala.util.Try


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2
  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(x) => Some(x * 2)
    case None => None
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = i.map(x => x * 2)

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat extends Animal
  case object Bird extends Animal
  case class Dog(age: Int) extends Animal

  /*
  def formatAnimal(animal: Animal): String =
    if (animal == Cat)
      "It's a cat"
    else if (animal == Bird)
      "It's a bird"
    else if (animal.isInstanceOf[Dog])
      s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    else
      throw new RuntimeException("This should not happen but I'm a Java developer !")
  */

  def formatAnimal(animal : Animal): String = animal match {
    case Cat => "It's a cat"
    case Bird => "It's a bird"
    case Dog(x) => s"It's a ${x} year old dog"
    case _ => "This is not an animal"
  }


  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = l.indexOf(a) match {
    case -1 => None
    case x => Some(x)
  }


  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)
  def keepValid[A](l: List[Either[Error, A]]): List[A] = l.collect{case Right(x) => x}

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = l match {
    case Nil => empty
    case _ => l.reduce(combine)
  }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A =
    l.collect{case Right(x) => x}.reduce(combine)


  trait Monoid[A] {
    def empty : A
    def combine(x : A, y : A) : A
  }



  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  def aggregateValidM[A](l : List[Either[Error, A]], m : Monoid[A]): A =
    l.collect{case Right(x) => x}.reduce((x, y) => m.combine(x, y))

  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)

  implicit val monoidString = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x.concat(y)
  }

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)

  /*
  abstract class FinancialAsset {
    def computeEarnings: Double
  }

  abstract class FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }
  */

  sealed trait FinancialAsset {
    def computeEarnings: Double
  }


  sealed trait FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  case class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  case class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }

  case class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }


  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  trait Earnings[T] {
    def compute(t : T) : Double
  }

  implicit val earningsLivret = new Earnings[LivretA] {
    override def compute(t: LivretA): Double = t.amount + (t.amount * LivretA.Rate)
  }

  implicit val earningsPel = new Earnings[Pel ] {
    override def compute(t: Pel): Double =
      if (Instant.now().minus(4, YEARS).isAfter(t.creation))
        t.amount + (t.amount * Pel.Rate) + Pel.GovernmentGrant
      else
        t.amount + (t.amount * Pel.Rate)
  }

  implicit val earningsCarSale = new Earnings[CarSale] {
    override def compute(t: CarSale): Double = t.amount - (CarSale.StateHorsePowerTaxation * t.horsePower)
  }

  implicit val earningsFinancialAsset = new Earnings[FinancialAsset] {
    override def compute(t: FinancialAsset): Double = t match {
      case x : LivretA => earningsLivret.compute(x)
      case y : Pel => earningsPel.compute(y)
      case z : CarSale => earningsCarSale.compute(z)
      case _ => 0
    }
  }


  // 1.12 Rewrite the following function with your typeclass (.5pts)
  /*
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum
  */

  def computeTotalEarnings[A](assets: List[A])(implicit ev : Earnings[A]): Double =
    assets.map(x => ev.compute(x)).sum
  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)

  implicit class StringOps(val s : String) {
    def atoi : Option[Int] = Try(s.toInt).toOption
  }

}
