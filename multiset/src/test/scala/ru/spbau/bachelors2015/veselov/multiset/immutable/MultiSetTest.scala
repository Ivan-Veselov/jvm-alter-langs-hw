package ru.spbau.bachelors2015.veselov.multiset.immutable

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.bachelors2015.veselov.multiset.immutable

class MultiSetTest extends FlatSpec with Matchers {
  behavior of "A Multiset"

  // Empty multiset

  "Multiset with no elements" should "be empty Multiset" in {
    immutable.MultiSet() shouldBe immutable.MultiSet.empty
  }

  "Empty Multiset size" should "be 0" in {
    immutable.MultiSet.empty.size shouldBe 0
  }

  "Empty Multiset" should "be empty" in {
    immutable.MultiSet.empty.isEmpty shouldBe true
  }

  "Empty Multiset" should "not be non empty" in {
    immutable.MultiSet.empty.nonEmpty shouldBe false
  }

  "Empty Multiset addition" should "work" in {
    immutable.MultiSet.empty.add(0) shouldBe immutable.MultiSet(0)

    immutable.MultiSet.empty.add(0, 1) shouldBe immutable.MultiSet(0, 1)

    immutable.MultiSet.empty.add(0, 0) shouldBe immutable.MultiSet(0, 0)
  }

  "Empty Multiset" should "be immutable" in {
    val emptySet = immutable.MultiSet.empty
    emptySet.add(0) should not be theSameInstanceAs(emptySet)
  }

  "Empty Multiset find" should "return None" in {
    immutable.MultiSet.empty.find(0) shouldBe None
  }

  "Empty Multiset count" should "be 0" in {
    immutable.MultiSet.empty.count(0) shouldBe 0
  }

  "Empty Multiset apply syntax" should "work" in {
    immutable.MultiSet.empty(0) shouldBe 0
  }

  "Empty Multiset filter" should "return empty Multiset" in {
    immutable.MultiSet.empty[Any].filter(_ => true) shouldBe immutable.MultiSet.empty
  }

  "Empty Multiset map" should "return empty Multiset" in {
    immutable.MultiSet.empty[Any].map((_: Any) => 0) shouldBe immutable.MultiSet.empty
  }

  "Empty Multiset flatMap" should "return empty Multiset" in {
    immutable.MultiSet.empty[Any].flatMap((_: Any) => List(0)) shouldBe immutable.MultiSet.empty
  }

  // Multiset of ints

  "Multiset ctor" should "always work the same way" in {
    immutable.MultiSet(0) shouldBe immutable.MultiSet(0)

    immutable.MultiSet(0, 1) shouldBe immutable.MultiSet(0, 1)

    immutable.MultiSet(0, 0) shouldBe immutable.MultiSet(0, 0)
  }

  "Multiset ctor" should "be independent of elements order" in {
    immutable.MultiSet(0, 1) shouldBe immutable.MultiSet(1, 0)
  }

  "Multiset ctor" should "depend on equal elements occurrences" in {
    immutable.MultiSet(0, 0) should not be immutable.MultiSet(0)
  }

  "Multiset size" should "work" in {
    immutable.MultiSet(0).size shouldBe 1

    immutable.MultiSet(0, 1).size shouldBe 2

    immutable.MultiSet(1, 0).size shouldBe 2

    immutable.MultiSet(0, 0).size shouldBe 2
  }

  "Non empty Multiset" should "not be empty" in {
    immutable.MultiSet(0).isEmpty shouldBe false
  }

  "Non empty Multiset" should "be non empty" in {
    immutable.MultiSet(0).nonEmpty shouldBe true
  }

  "Non empty Multiset addition" should "work" in {
    immutable.MultiSet(0).add(1) shouldBe immutable.MultiSet(0, 1)

    immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(0, 1, 2)

    immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(1, 0, 2)

    immutable.MultiSet(0).add(1, 2) shouldBe immutable.MultiSet(2, 0, 1)

    immutable.MultiSet(0).add(0) shouldBe immutable.MultiSet(0, 0)

    immutable.MultiSet(0).add(0, 1) shouldBe immutable.MultiSet(0, 0, 1)

    immutable.MultiSet(0).add(0, 0) shouldBe immutable.MultiSet(0, 0, 0)

    immutable.MultiSet(0, 0).add(0) shouldBe immutable.MultiSet(0, 0, 0)

    immutable.MultiSet(0, 0).add(1) shouldBe immutable.MultiSet(0, 0, 1)

    immutable.MultiSet(0, 0).add(1, 1) shouldBe immutable.MultiSet(0, 0, 1, 1)
  }

  "Non empty Multiset" should "be immutable" in {
    val oneElementSet = immutable.MultiSet(0)
    oneElementSet.add(1) should not be theSameInstanceAs(oneElementSet)
  }

  "Non empty Multiset find" should "work" in {
    immutable.MultiSet(0).find(0) shouldBe Some(0)

    immutable.MultiSet(0, 1).find(0) shouldBe Some(0)

    immutable.MultiSet(0, 0).find(0) shouldBe Some(0)

    immutable.MultiSet(0, 1).find(2) shouldBe None
  }

  "Non empty Multiset count" should "work" in {
    immutable.MultiSet(0).count(1) shouldBe 0

    immutable.MultiSet(0).count(0) shouldBe 1

    immutable.MultiSet(0, 0).count(1) shouldBe 0

    immutable.MultiSet(0, 0).count(0) shouldBe 2

    immutable.MultiSet(0, 1, 1).count(0) shouldBe 1

    immutable.MultiSet(0, 1, 1).count(1) shouldBe 2
  }

  "Non empty Multiset apply syntax" should "work" in {
    immutable.MultiSet(0, 1, 1)(1) shouldBe 2
  }

  "Non empty Multiset filter" should "work" in {
    immutable.MultiSet(0, 1).filter(n => n > 0) shouldBe immutable.MultiSet(1)

    immutable.MultiSet(0, 1, 1).filter(n => n > 0) shouldBe immutable.MultiSet(1, 1)

    immutable.MultiSet(0, 0).filter(n => n > 0) shouldBe immutable.MultiSet.empty
  }

  "Non empty Multiset map" should "work" in {
    immutable.MultiSet(0).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1)

    immutable.MultiSet(0, 1).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1, 2)

    immutable.MultiSet(0, 0).map((n: Int) => n + 1) shouldBe immutable.MultiSet(1, 1)
  }

  "Non empty Multiset flatMap" should "work" in {
    immutable.MultiSet(0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(0, 0)

    immutable.MultiSet(0, 0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(0, 0, 0, 0)

    immutable.MultiSet(0, 1, 0).flatMap((n: Int) => List(n, n)) shouldBe immutable.MultiSet(
                                                                                   0, 0, 0, 0, 1, 1)
  }

  // intersection and union

  "Multiset union" should "work" in {
    immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 1)) shouldBe immutable.MultiSet(0, 1)

    immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 2)) shouldBe immutable.MultiSet(0, 1, 2)

    immutable.MultiSet(0, 1).union(immutable.MultiSet(0, 1, 1)) shouldBe immutable.MultiSet(0, 1, 1)

    immutable.MultiSet(0, 1).union(immutable.MultiSet(2, 3)) shouldBe immutable.MultiSet(0, 1, 2, 3)
  }

  "Multiset union operator syntax" should "work" in {
    immutable.MultiSet(0, 1) | immutable.MultiSet(0, 1, 1) shouldBe immutable.MultiSet(0, 1, 1)
  }

  "Multiset intersection" should "work" in {
    immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 1)) shouldBe
                                                                            immutable.MultiSet(0, 1)

    immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 2)) shouldBe immutable.MultiSet(0)

    immutable.MultiSet(0, 1).intersection(immutable.MultiSet(0, 1, 1)) shouldBe
                                                                            immutable.MultiSet(0, 1)

    immutable.MultiSet(0, 1).intersection(immutable.MultiSet(2, 3)) shouldBe
                                                                          immutable.MultiSet.empty
  }

  "Multiset intersection operator syntax" should "work" in {
    immutable.MultiSet(0, 1) & immutable.MultiSet(0, 1, 1) shouldBe immutable.MultiSet(0, 1)
  }

  "Multiset" should "partly support for-comprehension syntax" in {
    (for (i: Int <- immutable.MultiSet(0, 0, 1, 1)) yield {
      i
    }) shouldBe immutable.MultiSet(0, 0, 1, 1)

    (for (i: Int <- immutable.MultiSet(0, 0, 1, 1)) yield {
      i * 2
    }) shouldBe immutable.MultiSet(0, 0, 2, 2)

    (for (i: Int <- immutable.MultiSet(0, 0, 1, 1) if i > 0) yield {
      i
    }) shouldBe immutable.MultiSet(1, 1)
  }

  "Multiset" should "partly support pattern-matching" in {
    (MultiSet(1, 1, 1) match {
      case MultiSet() => false
      case MultiSet(1) => false
      case MultiSet(1, 1) => false
      case MultiSet(1, 1, 1) => true
      case _ => false
    }) shouldBe true
  }
}
