package ru.spbau.bachelors2015.veselov.multiset.immutable

import scala.collection.GenTraversableOnce

sealed abstract class MultiSet[+T] {
  val size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = size > 0

  def +[A >: T](elem: A): MultiSet[A]

  def find[A >: T](elem: A): Option[A]

  def count[A >: T](elem: A): Int

  def apply[A >: T](elem: A): Int = count(elem)

  def foreach[U](f: T => U): Unit

  def &[A >: T](other: MultiSet[A]): MultiSet[A]

  def |[A >: T](other: MultiSet[A]): MultiSet[A]

  def filter[A >: T](predicate: A => Boolean): MultiSet[A]

  def withFilter[A >: T](predicate: A => Boolean): MultiSet[A] = filter(predicate)

  def map[A >: T, B](mapper: A => B): MultiSet[B]

  def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B]

  def asSeq(): Seq[T]
}

object MultiSet {
  def apply[T](elems: GenTraversableOnce[T]): MultiSet[T] =
    if (elems.isEmpty) EmptyMultiSet
    else new MultiSetImpl[T](elems)

  def unapplySeq[T](set: MultiSet[T]): Option[Seq[T]] = Some(set.asSeq())

  def *[T](set: MultiSet[T]): Option[Seq[T]] = unapplySeq(set)

  def empty[T](): MultiSet[T] = EmptyMultiSet

  private object EmptyMultiSet extends MultiSet[Nothing] {
    override val size: Int = 0

    override def +[A](elem: A): MultiSet[A] = new MultiSetImpl[A](Seq(elem))

    override def find[A](elem: A): Option[A] = None

    override def count[A](elem: A): Int = 0

    override def foreach[U](f: Nothing => U): Unit = Unit

    override def &[A](other: MultiSet[A]): MultiSet[A] = this

    override def |[A](other: MultiSet[A]): MultiSet[A] = other

    override def filter[A](predicate: A => Boolean): MultiSet[A] = this

    override def map[A, B](mapper: A => B): MultiSet[B] = this

    override def flatMap[A, B](mapper: A => GenTraversableOnce[B]): MultiSet[B] = this

    override def asSeq(): Seq[Nothing] = Seq.empty
  }

  private class MultiSetImpl[+T](elems: GenTraversableOnce[T]) extends MultiSet[T] {
    private val hashTable: Map[Int, List[(T, Int)]] =
      elems.toList
           .groupBy(e => e.hashCode())
           .map { case (k, v) =>
             (k, v.map(t => (t, v.count(t == _)))
                  .distinct)
           }

    override val size: Int = elems.size

    override def +[A >: T](elem: A): MultiSet[A] = MultiSet(elem +: asSeq())

    override def find[A >: T](elem: A): Option[A] =
      for (list <- hashTable.get(elem.hashCode());
           (t, _) <- list.find(elem == _._1)) yield t

    override def count[A >: T](elem: A): Int =
      (for (list <- hashTable.get(elem.hashCode());
            (_, amount) <- list.find(elem == _._1)) yield amount).getOrElse(0)

    override def foreach[U](f: T => U): Unit =
      for (list <- hashTable.values; (t, a) <- list) {
        1 to a foreach(_ => f(t))
      }

    override def &[A >: T](other: MultiSet[A]): MultiSet[A] =
      MultiSet(
        (for ((_, list) <- hashTable; (t, amount) <- list) yield
          List.fill(math.min(amount, other(t)))(t)
        ).flatten
      )

    override def |[A >: T](other: MultiSet[A]): MultiSet[A] =
      MultiSet(
        (for ((_, list) <- hashTable; (t, amount) <- list) yield
          List.fill(math.max(amount, other(t)) - amount)(t)
        ).flatten ++ other.asSeq()
      )

    override def filter[A >: T](predicate: A => Boolean): MultiSet[A] =
      MultiSet(
        (for (list <- hashTable.values; (t, a) <- list if predicate(t)) yield {
          List.fill(a)(t)
        }).flatten
      )

    override def map[A >: T, B](mapper: A => B): MultiSet[B] =
      MultiSet(
        (for (list <- hashTable.values; (t, a) <- list) yield {
          val mt = mapper(t)
          List.fill(a)(mt)
        }).flatten
      )

    override def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B] =
      MultiSet(
        (for (list <- hashTable.values; (t, a) <- list) yield {
          val mt = mapper(t)
          List.fill(a)(mt)
        }).flatten.flatten
      )

    override def asSeq(): Seq[T] =
      (for (list <- hashTable.values; (t, a) <- list) yield {
        List.fill(a)(t)
      }).flatten.toList

    private def isSubsetOf[A >: T](that: MultiSetImpl[A]): Boolean =
      (for ((hash, ts) <- hashTable; tts <- that.hashTable.get(hash)) yield {
        for ((t, tamount) <- ts; (tt, ttamount) <- tts) yield {
          t == tt && tamount <= ttamount
        }
      }).flatten.fold(false)(_ || _)

    override def equals(other: Any): Boolean = other match {
      case that: MultiSetImpl[Any] => that.isSubsetOf(this) && this.isSubsetOf(that)
      case _ => false
    }
  }
}
