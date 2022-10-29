package byrd.riley.scalasimplemonadimpl

import org.scalatest.funspec.AnyFunSpec

import MonadInstances.given_Monad_Option

class MyTest extends AnyFunSpec:
  it("applicatives") {
    val func = (_: Int) + 3
    val productedOption = Applicative[Option].product(Some(3), Some(3))
    val mappedOption = Applicative[Option].map(Some(3))(func)
    val appedOption = Applicative[Option].ap(Some(func))(Some(3))

    println(productedOption)
    println(mappedOption)
    println(appedOption)

    import Apply.ap
    import Applicative.{mapN, tupled, apWith}
    val applicativeTuple: (Option[Int], Option[Int]) = (Some(3), Some(4))
    val applicativeFunc: Option[Tuple.InverseMap[applicativeTuple.type, Option] => Int] = Some((item1: Int, item2: Int) => item1 + item2)
    val mapNedTuple = applicativeTuple.mapN((item1, item2) => item1 + item2)
    val tupledTuple = applicativeTuple.tupled
    val apWithedTuple = applicativeTuple.apWith(applicativeFunc)
    val appedFunc = applicativeFunc.ap(tupledTuple)

    println(mapNedTuple)
    println(tupledTuple)
    println(apWithedTuple)
    println(appedFunc)
  }

  it("parallels") {
    import Parallel.{parProduct, parAp, parMapN, parTupled, parApWith}
    import ParallelInstances.{given_Parallel_Either, given_Parallel_List}
    import Semigroupal.product
    import MonadInstances.{given_Monad_Either, given_Monad_List}

    val list1 = List(1, 2)
    val list2 = List(3, 4)
    val listProduct = Monad[List].product(list1, list2)
    val listParProduct = list1.parProduct(list2)

    // Required when parProduct on an Either encounters more than one Left.
    given errorSemigroup: Semigroup[String] = (x: String, y: String) => s"$x, $y"

    val left1: Either[String, Int] = Left("Error1")
    val left2: Either[String, Int] = Left("Error2")
    val right1: Either[String, Int] = Right(1)
    val right2: Either[String, Int] = Right(2)
    val eitherLeftProduct = left1.product(left2)
    val eitherLeftParProduct = left1.parProduct(left2)
    val eitherMixedProduct = left1.product(right1)
    val eitherMixedParProduct = left1.parProduct(right1)
    val eitherRightProduct = right1.product(right2).product(right2)
    val eitherRightParProduct = right1.parProduct(right2).parProduct(right2)

    println(listProduct)
    println(listParProduct)
    println(eitherLeftProduct)
    println(eitherLeftParProduct)
    println(eitherMixedProduct)
    println(eitherMixedParProduct)
    println(eitherRightProduct)
    println(eitherRightParProduct)


    val listFunc = List((item: Int) => item + 5)
    val parAppedListFunc = listFunc.parAp(List(15))

    println(parAppedListFunc)


    val applicativeTuple: (List[Int], List[Int]) = (List(3), List(4))
    val applicativeFunc: List[Tuple.InverseMap[applicativeTuple.type, List] => Int] = List((item1: Int, item2: Int) => item1 + item2)
    val parMapNedTuple = applicativeTuple.parMapN((item1, item2) => item1 + item2)
    val parTupledTuple = applicativeTuple.parTupled
    val parApWithedTuple = applicativeTuple.parApWith(applicativeFunc)
    val parAppedFunc = applicativeFunc.parAp(parTupledTuple)

    println(parMapNedTuple)
    println(parTupledTuple)
    println(parApWithedTuple)
    println(parAppedFunc)
  }

