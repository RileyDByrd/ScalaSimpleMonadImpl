package byrd.riley.scalasimplemonadimpl

import org.scalatest.funspec.AnyFunSpec

class MyTest extends AnyFunSpec:
  it("applicatives") {
    import MonadInstances.Maybe.Attested
    import MonadInstances.Maybe
    import MonadInstances.given_Monad_Maybe

    val func = (_: Int) + 3
    val maybeProduct = Attested(3).product(Attested(3))
    val mappedMaybe = Attested(3).map(func)
    val appedMaybe = Attested(func).ap(Attested(3))

    println(maybeProduct)
    println(mappedMaybe)
    println(appedMaybe)

    val applicativeTuple: (Maybe[Int], Maybe[Int]) = (Attested(3), Attested(4))
    val applicativeFunc: Maybe[Tuple.InverseMap[applicativeTuple.type, Maybe] => Int] = Attested((item1: Int, item2: Int) => item1 + item2)
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
    import MonadInstances.{LinkedList, LinkedCons, LinkedNil, Disjunction}
    import MonadInstances.Disjunction.{Happy, Sad}
    import ParallelInstances.given_Parallel_LinkedList

    val list1 = LinkedCons(1, LinkedCons(2, LinkedNil))
    val list2 = LinkedCons(3, LinkedCons(4, LinkedNil))
    val listProduct = list1.product(list2)
    val listParProduct = list1.parProduct(list2)

    import ParallelInstances.given_Parallel_Disjunction

    // Required when parProduct on an Either encounters more than one Left.
    given errorSemigroup: Semigroup[String] = (x: String, y: String) => s"$x, $y"

    val sad1: Disjunction[String, Int] = Sad("Error1")
    val sad2: Disjunction[String, Int] = Sad("Error2")
    val happy1: Disjunction[String, Int] = Happy(1)
    val happy2: Disjunction[String, Int] = Happy(2)
    val sadProduct = sad1.product(sad2)
    val sadParProduct = sad1.parProduct(sad2)
    val mixedProduct = sad1.product(happy1)
    val mixedParProduct = sad1.parProduct(happy1)
    val happyProduct = happy1.product(happy2).product(happy2)
    val happyParProduct = happy1.parProduct(happy2).product(happy2)

    println(listProduct)
    println(listParProduct)
    println(sadProduct)
    println(sadParProduct)
    println(mixedProduct)
    println(mixedParProduct)
    println(happyProduct)
    println(happyParProduct)


    val linkedListFunc = LinkedCons((item: Int) => item + 5, LinkedNil)
    val parAppedListFunc = linkedListFunc.parAp(LinkedCons(15, LinkedNil))

    println(parAppedListFunc)


    val applicativeTuple: (LinkedList[Int], LinkedList[Int]) = (LinkedCons(3, LinkedNil), LinkedCons(4, LinkedNil))
    val applicativeFunc: LinkedList[Tuple.InverseMap[applicativeTuple.type, LinkedList] => Int] = LinkedCons((item1: Int, item2: Int) => item1 + item2, LinkedNil)
    val parMapNedTuple = applicativeTuple.parMapN((item1, item2) => item1 + item2)
    val parTupledTuple = applicativeTuple.parTupled
    val parApWithedTuple = applicativeTuple.parApWith(applicativeFunc)
    val parAppedFunc = applicativeFunc.parAp(parTupledTuple)

    println(parMapNedTuple)
    println(parTupledTuple)
    println(parApWithedTuple)
    println(parAppedFunc)
  }

