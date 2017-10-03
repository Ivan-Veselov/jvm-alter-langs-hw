package ru.spbau.bachelors2015.veselov.multiset.immutable

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.bachelors2015.veselov.multiset.immutable

class MultiSetTest extends FlatSpec with Matchers {
  behavior of "A Multiset"

  // Empty multiset

  "Multiset with no elements" should "be empty Multiset" in {
    MultiSet(Seq()) shouldBe MultiSet.empty
  }

  "Empty Multiset size" should "be 0" in {
    MultiSet.empty.size shouldBe 0
  }

  "Empty Multiset" should "be empty" in {
    MultiSet.empty.isEmpty shouldBe true
  }

  "Empty Multiset" should "not be non empty" in {
    MultiSet.empty.nonEmpty shouldBe false
  }

  "Empty Multiset addition" should "work" in {
    MultiSet.empty + 0 shouldBe MultiSet(Seq(0))

    MultiSet.empty[Int] + 0 + 1 shouldBe MultiSet(Seq(0, 1))

    MultiSet.empty + 0 + 0 shouldBe MultiSet(Seq(0, 0))
  }

  "Empty Multiset" should "be immutable" in {
    val emptySet = MultiSet.empty
    emptySet + 0 should not be theSameInstanceAs(emptySet)
  }

  "Empty Multiset find" should "return None" in {
    MultiSet.empty.find(0) shouldBe None
  }

  "Empty Multiset count" should "be 0" in {
    MultiSet.empty.count(0) shouldBe 0
  }

  "Empty Multiset apply syntax" should "work" in {
    MultiSet.empty(0) shouldBe 0
  }

  "Empty Multiset filter" should "return empty Multiset" in {
    MultiSet.empty[Any].filter(_ => true) shouldBe MultiSet.empty
  }

  "Empty Multiset map" should "return empty Multiset" in {
    MultiSet.empty[Any].map((_: Any) => 0) shouldBe MultiSet.empty
  }

  "Empty Multiset flatMap" should "return empty Multiset" in {
    MultiSet.empty[Any].flatMap((_: Any) => List(0)) shouldBe MultiSet.empty
  }

  // Multiset of ints

  "Multiset ctor" should "always work the same way" in {
    MultiSet(Seq(0)) shouldBe MultiSet(Seq(0))

    MultiSet(Seq(0, 1)) shouldBe MultiSet(Seq(0, 1))

    MultiSet(Seq(0, 0)) shouldBe MultiSet(Seq(0, 0))
  }

  "Multiset ctor" should "be independent of elements order" in {
    MultiSet(Seq(0, 1)) shouldBe MultiSet(Seq(1, 0))
  }

  "Multiset ctor" should "depend on equal elements occurrences" in {
    MultiSet(Seq(0, 0)) should not be MultiSet(Seq(0))
  }

  "Multiset size" should "work" in {
    MultiSet(Seq(0)).size shouldBe 1

    MultiSet(Seq(0, 1)).size shouldBe 2

    MultiSet(Seq(1, 0)).size shouldBe 2

    MultiSet(Seq(0, 0)).size shouldBe 2
  }

  "Non empty Multiset" should "not be empty" in {
    MultiSet(Seq(0)).isEmpty shouldBe false
  }

  "Non empty Multiset" should "be non empty" in {
    MultiSet(Seq(0)).nonEmpty shouldBe true
  }

  "Non empty Multiset addition" should "work" in {
    MultiSet(Seq(0)) + 1 shouldBe MultiSet(Seq(0, 1))

    MultiSet(Seq(0)) + 1 + 2 shouldBe MultiSet(Seq(0, 1, 2))

    MultiSet(Seq(0)) + 1 + 2 shouldBe MultiSet(Seq(1, 0, 2))

    MultiSet(Seq(0)) + 1 + 2 shouldBe MultiSet(Seq(2, 0, 1))

    MultiSet(Seq(0)) + 0 shouldBe MultiSet(Seq(0, 0))

    MultiSet(Seq(0)) + 0 + 1 shouldBe MultiSet(Seq(0, 0, 1))

    MultiSet(Seq(0)) + 0 + 0 shouldBe MultiSet(Seq(0, 0, 0))

    MultiSet(Seq(0, 0)) + 0 shouldBe MultiSet(Seq(0, 0, 0))

    MultiSet(Seq(0, 0)) + 1 shouldBe MultiSet(Seq(0, 0, 1))

    MultiSet(Seq(0, 0)) + 1 + 1 shouldBe MultiSet(Seq(0, 0, 1, 1))
  }

  "Non empty Multiset" should "be immutable" in {
    val oneElementSet = MultiSet(Seq(0))
    oneElementSet + 1 should not be theSameInstanceAs(oneElementSet)
  }

  "Non empty Multiset find" should "work" in {
    MultiSet(Seq(0)).find(0) shouldBe Some(0)

    MultiSet(Seq(0, 1)).find(0) shouldBe Some(0)

    MultiSet(Seq(0, 0)).find(0) shouldBe Some(0)

    MultiSet(Seq(0, 1)).find(2) shouldBe None
  }

  "Non empty Multiset count" should "work" in {
    MultiSet(Seq(0)).count(1) shouldBe 0

    MultiSet(Seq(0)).count(0) shouldBe 1

    MultiSet(Seq(0, 0)).count(1) shouldBe 0

    MultiSet(Seq(0, 0)).count(0) shouldBe 2

    MultiSet(Seq(0, 1, 1)).count(0) shouldBe 1

    MultiSet(Seq(0, 1, 1)).count(1) shouldBe 2
  }

  "Non empty Multiset apply syntax" should "work" in {
    MultiSet(Seq(0, 1, 1))(1) shouldBe 2
  }

  "Non empty Multiset filter" should "work" in {
    MultiSet(Seq(0, 1)).filter(n => n > 0) shouldBe MultiSet(Seq(1))

    MultiSet(Seq(0, 1, 1)).filter(n => n > 0) shouldBe MultiSet(Seq(1, 1))

    MultiSet(Seq(0, 0)).filter(n => n > 0) shouldBe MultiSet.empty
  }

  "Non empty Multiset map" should "work" in {
    MultiSet(Seq(0)).map((n: Int) => n + 1) shouldBe MultiSet(Seq(1))

    MultiSet(Seq(0, 1)).map((n: Int) => n + 1) shouldBe MultiSet(Seq(1, 2))

    MultiSet(Seq(0, 0)).map((n: Int) => n + 1) shouldBe MultiSet(Seq(1, 1))
  }

  "Non empty Multiset flatMap" should "work" in {
    MultiSet(Seq(0)).flatMap((n: Int) => List(n, n)) shouldBe MultiSet(Seq(0, 0))

    MultiSet(Seq(0, 0)).flatMap((n: Int) => List(n, n)) shouldBe MultiSet(Seq(0, 0, 0, 0))

    MultiSet(Seq(0, 1, 0)).flatMap((n: Int) => List(n, n)) shouldBe MultiSet(Seq(
                                                                                   0, 0, 0, 0, 1, 1))
  }

  // intersection and union

  "Multiset union" should "work" in {
    MultiSet(Seq(0, 1)) | MultiSet(Seq(0, 1)) shouldBe MultiSet(Seq(0, 1))

    MultiSet(Seq(0, 1)) | MultiSet(Seq(0, 2)) shouldBe MultiSet(Seq(0, 1, 2))

    MultiSet(Seq(0, 1)) | MultiSet(Seq(0, 1, 1)) shouldBe MultiSet(Seq(0, 1, 1))

    MultiSet(Seq(0, 1)) | MultiSet(Seq(2, 3)) shouldBe MultiSet(Seq(0, 1, 2, 3))
  }

  "Multiset intersection" should "work" in {
    MultiSet(Seq(0, 1)) & MultiSet(Seq(0, 1)) shouldBe MultiSet(Seq(0, 1))

    MultiSet(Seq(0, 1)) & MultiSet(Seq(0, 2)) shouldBe MultiSet(Seq(0))

    MultiSet(Seq(0, 1)) & MultiSet(Seq(0, 1, 1)) shouldBe MultiSet(Seq(0, 1))

    MultiSet(Seq(0, 1)) & MultiSet(Seq(2, 3)) shouldBe MultiSet.empty
  }

  "Multiset" should "partly support for-comprehension syntax" in {
    (for (i: Int <- MultiSet(Seq(0, 0, 1, 1))) yield {
      i
    }) shouldBe MultiSet(Seq(0, 0, 1, 1))

    (for (i: Int <- MultiSet(Seq(0, 0, 1, 1))) yield {
      i * 2
    }) shouldBe MultiSet(Seq(0, 0, 2, 2))

    (for (i: Int <- MultiSet(Seq(0, 0, 1, 1)) if i > 0) yield {
      i
    }) shouldBe MultiSet(Seq(1, 1))
  }

  "Multiset" should "partly support pattern-matching" in {
    (MultiSet(Seq(1, 1, 1)) match {
      case MultiSet() => false
      case MultiSet(1) => false
      case MultiSet(1, 1) => false
      case MultiSet(1, 1, 1) => true
      case _ => false
    }) shouldBe true
  }
}
