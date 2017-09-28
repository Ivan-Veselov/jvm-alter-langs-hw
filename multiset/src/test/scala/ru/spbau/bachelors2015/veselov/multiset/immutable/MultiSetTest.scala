package ru.spbau.bachelors2015.veselov.multiset.immutable

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.bachelors2015.veselov.multiset.immutable

// TODO: for-comprehension
// TODO: pattern-matching
class MultiSetTest extends FlatSpec with Matchers {
  // Empty multiset

  immutable.MultiSet.empty shouldBe immutable.MultiSet()

  immutable.MultiSet.empty.size shouldBe 0

  immutable.MultiSet.empty.isEmpty shouldBe true

  immutable.MultiSet.empty.nonEmpty shouldBe false

  immutable.MultiSet.empty.add(0) shouldBe immutable.MultiSet(0)

  immutable.MultiSet.empty.add(0, 1) shouldBe immutable.MultiSet(0, 1)

  immutable.MultiSet.empty.add(0, 0) shouldBe immutable.MultiSet(0, 0)

  private val emptySet = immutable.MultiSet.empty
  emptySet.add(0) should not be theSameInstanceAs (emptySet)

  immutable.MultiSet.empty.find(0) shouldBe None

  immutable.MultiSet.empty.count(0) shouldBe 0

  immutable.MultiSet.empty(0) shouldBe 0

  immutable.MultiSet.empty[Any].filter(_ => true) shouldBe immutable.MultiSet.empty

  immutable.MultiSet.empty[Any].map((_: Any) => 0) shouldBe immutable.MultiSet.empty

  immutable.MultiSet.empty[Any].flatMap((_: Any) => List(0)) shouldBe immutable.MultiSet.empty

  // Multiset of ints

  immutable.MultiSet(0) shouldBe immutable.MultiSet(0)

  immutable.MultiSet(0, 1) shouldBe immutable.MultiSet(0, 1)

  immutable.MultiSet(0, 1) shouldBe immutable.MultiSet(1, 0)

  immutable.MultiSet(0, 0) shouldBe immutable.MultiSet(0, 0)

  immutable.MultiSet(0, 0) should not be immutable.MultiSet(0)

  immutable.MultiSet(0).size shouldBe 1

  immutable.MultiSet(0, 1).size shouldBe 2

  immutable.MultiSet(1, 0).size shouldBe 2

  immutable.MultiSet(0, 0).size shouldBe 2

  immutable.MultiSet(0).isEmpty shouldBe false

  immutable.MultiSet(0).nonEmpty shouldBe true

  immutable.MultiSet(0).add(1) shouldBe immutable.MultiSet(0, 1)

  immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(0, 1, 2)

  immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(1, 0, 2)

  immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(2, 0, 1)

  immutable.MultiSet(0).add(0) shouldBe immutable.MultiSet(0, 0)

  immutable.MultiSet(0).add(0, 1) shouldBe immutable.MultiSet(0, 0, 1)

  immutable.MultiSet(0).add(0, 0) shouldBe immutable.MultiSet(0, 0, 0)

  private val oneElementSet = immutable.MultiSet(0)
  oneElementSet.add(1) should not be theSameInstanceAs (oneElementSet)

  immutable.MultiSet(0).find(0) shouldBe Some(0)

  immutable.MultiSet(0, 1).find(0) shouldBe Some(0)

  immutable.MultiSet(0, 0).find(0) shouldBe Some(0)

  immutable.MultiSet(0, 1).find(2) shouldBe None

  immutable.MultiSet(0).count(1) shouldBe 0

  immutable.MultiSet(0).count(0) shouldBe 1

  immutable.MultiSet(0, 0).count(1) shouldBe 0

  immutable.MultiSet(0, 0).count(0) shouldBe 2

  immutable.MultiSet(0, 1, 1).count(0) shouldBe 1

  immutable.MultiSet(0, 1, 1).count(1) shouldBe 2

  immutable.MultiSet(0, 1, 1)(1) shouldBe 2

  immutable.MultiSet(0, 1).filter(n => n > 0) shouldBe immutable.MultiSet(1)

  immutable.MultiSet(0, 1, 1).filter(n => n > 0) shouldBe immutable.MultiSet(1, 1)

  immutable.MultiSet(0, 0).filter(n => n > 0) shouldBe immutable.MultiSet.empty

  immutable.MultiSet(0).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1)

  immutable.MultiSet(0, 1).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1, 2)

  immutable.MultiSet(0, 0).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1, 1)

  immutable.MultiSet(0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(0, 0)

  immutable.MultiSet(0, 0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(0, 0, 0, 0)

  immutable.MultiSet(0, 1, 0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(
                                                                                  0, 0, 0, 0, 1, 1)

  // intersection and union

  immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 1)) shouldBe immutable.MultiSet(0, 1)

  immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 2)) shouldBe immutable.MultiSet(0, 1, 2)

  immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 1, 1)) shouldBe immutable.MultiSet(0, 1, 1)

  immutable.MultiSet(0, 1).union(immutable.MultiSet(2, 3)) shouldBe immutable.MultiSet(0, 1, 2, 3)

  immutable.MultiSet(0, 1) | immutable.MultiSet(0, 1, 1) shouldBe immutable.MultiSet(0, 1, 1)

  immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 1)) shouldBe immutable.MultiSet(0, 1)

  immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 2)) shouldBe immutable.MultiSet(0)

  immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 1, 1)) shouldBe
                                                                          immutable.MultiSet(0, 1)

  immutable.MultiSet(0, 1).intersection(immutable.MultiSet(2, 3)) shouldBe immutable.MultiSet.empty

  immutable.MultiSet(0, 1) & immutable.MultiSet(0, 1, 1) shouldBe immutable.MultiSet(0, 1)
}
