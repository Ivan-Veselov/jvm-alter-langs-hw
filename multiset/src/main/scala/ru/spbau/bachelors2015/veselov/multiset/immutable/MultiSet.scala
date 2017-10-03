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

  def foreach[U](f: T => U): Unit = asSeq().foreach(f)

  def &[A >: T](other: MultiSet[A]): MultiSet[A]

  def |[A >: T](other: MultiSet[A]): MultiSet[A]

  // TODO
  def filter[A >: T](predicate: A => Boolean): MultiSet[A] =
                                                          MultiSet(asSeq().filter(predicate): _*)

  // TODO
  def withFilter[A >: T](predicate: A => Boolean): MultiSet[A] = filter(predicate)

  // TODO
  def map[A >: T, B](mapper: A => B): MultiSet[B] = MultiSet(asSeq().map(mapper): _*)

  // TODO
  def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B] =
    MultiSet(asSeq().flatMap(mapper): _*)

  def asSeq(): Seq[T]
}

private class MultiSetImpl[+T](elems: T*) extends MultiSet[T] { // TODO
  private val hashTable: Map[Int, List[(T, Int)]] = // TODO
    elems.toList
         .groupBy(e => e.hashCode())
         .map { case (k, v) =>
           (k, v.map(t => (t, v.count(t == _)))
                .distinct)
         }

  override val size: Int = elems.size

  override def +[A >: T](elem: A): MultiSet[A] = MultiSet(elem +: asSeq():_*)

  override def find[A >: T](elem: A): Option[A] =
    for (list <- hashTable.get(elem.hashCode());
         (t, _) <- list.find(elem == _._1)) yield t

  override def count[A >: T](elem: A): Int =
    (for (list <- hashTable.get(elem.hashCode());
         (_, amount) <- list.find(elem == _._1)) yield amount).getOrElse(0)

  override def &[A >: T](other: MultiSet[A]): MultiSet[A] = // TODO
    MultiSet(
      hashTable.flatMap { case (_, l) =>
        l.flatMap { case (t, amount) =>
          List.fill(math.min(amount, other(t)))(t)
        }
      }.toList: _*
    )

  override def |[A >: T](other: MultiSet[A]): MultiSet[A] = // TODO
    MultiSet(
      hashTable.flatMap { case (_, l) =>
        l.flatMap { case (t, amount) =>
          List.fill(math.max(amount, other(t)) - amount)(t)
        }
      }.toList ++ other.asSeq(): _*
    )

  override def asSeq(): Seq[T] = // TODO
    hashTable.values
             .flatMap(l =>
               l.flatMap { case (t, a) =>
                 List.fill(a)(t)
               })
             .toList

  private def isSubsetOf[A >: T](that: MultiSetImpl[A]): Boolean = {
    val tmp =
      for ((hash, ts) <- hashTable; tts <- that.hashTable.get(hash)) yield {
        for ((t, tamount) <- ts; (tt, ttamount) <- tts) yield {
          t == tt && tamount <= ttamount
        }
      }

    tmp.flatten.fold(false)(_ || _) // TODO
  }

  override def equals(other: Any): Boolean = other match {
    case that: MultiSetImpl[Any] => that.isSubsetOf(this) && this.isSubsetOf(that)
    case _ => false
  }
}

object MultiSet {
  def apply[T](elems: T*): MultiSet[T] = if (elems.isEmpty) EmptyMultiSet
                                         else new MultiSetImpl[T](elems: _*)

  def unapplySeq[T](set: MultiSet[T]): Option[Seq[T]] = Some(set.asSeq())

  def *[T](set: MultiSet[T]): Option[Seq[T]] = unapplySeq(set)

  def empty[T]: MultiSet[T] = EmptyMultiSet

  private object EmptyMultiSet extends MultiSet[Nothing] {
    override val size: Int = 0

    override def +[A](elem: A): MultiSet[A] = new MultiSetImpl[A](Seq(elem): _*)

    override def find[A](elem: A): Option[A] = None

    override def count[A](elem: A): Int = 0

    override def &[A](other: MultiSet[A]): MultiSet[A] = this

    override def |[A](other: MultiSet[A]): MultiSet[A] = other

    override def asSeq(): Seq[Nothing] = Seq.empty
  }
}
