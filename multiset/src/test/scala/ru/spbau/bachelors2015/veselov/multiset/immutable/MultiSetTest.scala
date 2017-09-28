package ru.spbau.bachelors2015.veselov.multiset.immutable

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.bachelors2015.veselov.multiset.immutable

// TODO: nonEmpty sets
// TODO: intersection and union
// TODO: for-comprehension
// TODO: pattern-matching
// TODO: high-order functions
// TODO: elem searching (find)
// TODO: apply
class MultiSetTest extends FlatSpec with Matchers {
  // Empty multiset
  immutable.MultiSet.empty shouldBe immutable.MultiSet()

  immutable.MultiSet.empty.size shouldBe 0

  immutable.MultiSet.empty.isEmpty shouldBe true

  immutable.MultiSet.empty.nonEmpty shouldBe false

  immutable.MultiSet.empty.add(0) shouldBe immutable.MultiSet(0)

  immutable.MultiSet.empty.add(0) should not be theSameInstanceAs (immutable.MultiSet.empty)

  immutable.MultiSet.empty.count(0) shouldBe 0

  immutable.MultiSet.empty[Any].filter(_ => true) shouldBe immutable.MultiSet.empty

  immutable.MultiSet.empty[Any].map((_: Any) => 0) shouldBe immutable.MultiSet.empty

  immutable.MultiSet.empty[Any].flatMap((_: Any) => List(0)) shouldBe immutable.MultiSet.empty

  // ...
}
