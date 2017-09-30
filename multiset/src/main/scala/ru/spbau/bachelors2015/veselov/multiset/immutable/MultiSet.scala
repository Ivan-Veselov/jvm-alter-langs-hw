package ru.spbau.bachelors2015.veselov.multiset.immutable

import scala.collection.GenTraversableOnce
import scala.collection.immutable

sealed abstract class MultiSet[+T] {
  val size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = size > 0

  def add[A >: T](elems: A*): MultiSet[A]

  def find[A >: T](elem: A): Option[A]

  def count[A >: T](elem: A): Int

  def apply[A >: T](elem: A): Int = count(elem)

  def intersection[A >: T](other: MultiSet[A]): MultiSet[A]

  def &[A >: T](other: MultiSet[A]): MultiSet[A] = intersection(other)

  def union[A >: T](other: MultiSet[A]): MultiSet[A]

  def |[A >: T](other: MultiSet[A]): MultiSet[A] = union(other)

  def filter[A >: T](predicate: A => Boolean): MultiSet[A]

  def map[A >: T, B](mapper: A => B): MultiSet[B]

  def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B]
}

private class MultiSetImpl[+T](elems: T*) extends MultiSet[T] {
  private val hashTable: immutable.HashMap[Int, List[(T, Int)]] = {
    var fromHash: immutable.HashMap[Int, List[T]] = immutable.HashMap.empty

    elems.foreach(e => fromHash = fromHash.updated(e.hashCode(),
                                                   fromHash.getOrElse(e.hashCode(), Nil) :+ e))

    fromHash.map {
      case (hash, es) =>
        var pairs: List[(T, Int)] = Nil
        es.foreach(t => {
          val index = pairs.indexWhere { case (mainVal, _) => mainVal.equals(t) }
          if (index == -1) {
            pairs = pairs :+ (t, 1)
          } else {
            val amount = pairs(index)._2
            pairs = pairs.updated(index, (t, amount + 1))
          }
        })

        (hash, pairs)
    }
  }

  override val size: Int = elems.size

  override def add[A >: T](elems: A*): MultiSet[A] = MultiSet(asList() ++ elems:_*)

  override def find[A >: T](elem: A): Option[A] =
    hashTable.get(elem.hashCode()).flatMap(l => l.find { case (t, _) => elem.equals(t) }.map(_._1))

  override def count[A >: T](elem: A): Int =
    hashTable.get(elem.hashCode())
             .flatMap(l => l.find { case (t, _) => elem.equals(t)})
             .map { case (_, amount) => amount }
             .getOrElse(0)

  override def intersection[A >: T](other: MultiSet[A]): MultiSet[A] = ???

  override def union[A >: T](other: MultiSet[A]): MultiSet[A] = ???

  override def filter[A >: T](predicate: (A) => Boolean): MultiSet[A] =
    MultiSet(asList().filter(predicate): _*)

  override def map[A >: T, B](mapper: (A) => B): MultiSet[B] =
    MultiSet(asList().map(mapper): _*)

  override def flatMap[A >: T, B](mapper: (A) => GenTraversableOnce[B]): MultiSet[B] =
    MultiSet(asList().flatMap(mapper): _*)

  private def asList(): List[T] =
    hashTable.values.flatMap(l => l.flatMap { case (t, a) => List.fill(a)(t) }).toList

  private def isSubsetOf[A >: T](that: MultiSetImpl[A]): Boolean = {
    hashTable.flatMap { case (hash, ts) =>
      that.hashTable.get(hash).map { tts =>
        ts.map {
          case (t, amount) =>
            tts.find { case (tt, _) => t.equals(tt) }
               .exists { case (_, ttamount) => amount <= ttamount }
        }
      }.getOrElse(Seq(false))
    }.fold(false)((a: Boolean, b: Boolean) => a || b)
  }

  override def equals(other: Any): Boolean = other match {
    case that: MultiSetImpl[Any] => that.isSubsetOf(this) && this.isSubsetOf(that)
    case _ => false
  }
}

object MultiSet {
  def apply[T](elems: T*): MultiSet[T] = if (elems.isEmpty) EmptyMultiSet
                                         else new MultiSetImpl[T](elems: _*)

  // unapplySeq

  // (*) = unapplySeq

  def empty[T]: MultiSet[T] = EmptyMultiSet

  private object EmptyMultiSet extends MultiSet {
    override val size: Int = 0

    override def add[A](elems: A*): MultiSet[A] = new MultiSetImpl[A](elems: _*)

    override def find[A](elem: A): Option[A] = None

    override def count[A](elem: A): Int = 0

    override def intersection[A](other: MultiSet[A]): MultiSet[A] = this

    override def union[A](other: MultiSet[A]): MultiSet[A] = other

    override def filter[A](predicate: (A) => Boolean): MultiSet[A] = this

    override def map[A, B](mapper: (A) => B): MultiSet[B] = this

    override def flatMap[A, B](mapper: (A) => GenTraversableOnce[B]): MultiSet[B] = this
  }
}
