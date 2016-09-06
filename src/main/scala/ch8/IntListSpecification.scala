package ch8

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object IntListSpecification extends Properties("IntList") {
  val intList = Gen.listOf(Gen.choose(0, 100))

  property("reverse") = forAll(intList) {
    ns => ns.reverse.reverse == ns
  }

  property("headOption") = forAll(intList) {
    ns => ns.headOption == ns.reverse.lastOption
  }
  property("fail") = forAll(intList) {
    ns => ns == ns.reverse
  }
  property("sum intList") = forAll(intList) {
    ns => ns.sum == ns.reverse.sum
  }
  val index = 2
  val int2List = Gen.listOf(Gen.choose(index, index))
  property(" same intList") = forAll(int2List) {
    ns => ns.sum == ns.length * index
  }

  property("max") = forAll(intList) { ns =>
    if (ns.isEmpty) {
      true
    } else {
      val m = ns.max
      ns.forall { x => x <= m }
    }
  }
}