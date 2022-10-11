package byrd.riley.scalasimplemonadimpl

import org.scalatest.funspec.AnyFunSpec

import MonadInstances.given_Monad_Option

class MyTest extends AnyFunSpec:
  it("does the thing") {
//    val double1 = Some((3, "hello"))
//    val double2 = Some((1.5, List()))
//    val myProduct = optionMonad.product(double1, double2)
//    println(myProduct)
//
//    implicit val optionApplicative: Applicative[Option] = optionMonad
//    val doubleDouble: (Option[(Int, String)], Option[(Double, List[?])]) = (double1, double2)
//    implicit val ev: Tuple.Map[Tuple.InverseMap[doubleDouble.type, Option], Option] = doubleDouble
//
//
//    def loop(tuple: Tuple): Option[(Int, String, Double, List[?])] =
//      tuple match
//        case EmptyTuple => None
//        case (a, b) => optionApplicative.product(a, b)
//
//    println(loop(doubleDouble))
  }

  it("applicatives") {
    val func = (_: Int) + 3
    val productedOption = Applicative[Option].product(Some(3), Some(3))
    val mappedOption = Applicative[Option].map(Some(3))(func)
    val appedOption = Applicative[Option].ap(Some(func))(Some(3))

    println(productedOption)
    println(mappedOption)
    println(appedOption)

    import Applicative.{mapN, tupled, apWith}
    val applicativeTuple: (Option[Int], Option[Int]) = (Some(3), Some(4))
    val applicativeFunc: Option[Tuple.InverseMap[applicativeTuple.type, Option] => Int] = Some((item1: Int, item2: Int) => item1 + item2)
    val mapNedTuple = applicativeTuple.mapN((item1, item2) => item1 + item2)
    val tupledTuple = applicativeTuple.tupled
    val apWithedTuple = applicativeTuple.apWith(applicativeFunc)

    println(mapNedTuple)
    println(tupledTuple)
    println(apWithedTuple)
  }

