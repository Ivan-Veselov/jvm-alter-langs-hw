package ru.spbau.bachelors2015.veselov.multiset.immutable

import scala.collection.GenTraversableOnce

sealed abstract class MultiSet[+T] {
  def size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = size > 0

  // apply

  def add[A >: T](elems: A*): MultiSet[A]

  // def find[A >: T](elem: A): Option[A]

  def count[A >: T](elem: A): Int

  def intersection[A >: T](other: MultiSet[A]): MultiSet[A]

  def &[A >: T](other: MultiSet[A]): MultiSet[A] = intersection(other)

  def union[A >: T](other: MultiSet[A]): MultiSet[A]

  def |[A >: T](other: MultiSet[A]): MultiSet[A] = union(other)

  def filter[A >: T](predicate: A => Boolean): MultiSet[A]

  def map[A >: T, B](mapper: A => B): MultiSet[B]

  def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B]
}

private class MultiSetOnMap[+T](elems: T*) extends MultiSet[T] {
  override def size: Int = ???

  override def add[A >: T](elems: A*): MultiSet[A] = ???

  override def count[A >: T](elem: A): Int = ???

  override def intersection[A >: T](other: MultiSet[A]): MultiSet[A] = ???

  override def union[A >: T](other: MultiSet[A]): MultiSet[A] = ???

  override def filter[A >: T](predicate: (A) => Boolean): MultiSet[A] = ???

  override def map[A >: T, B](mapper: (A) => B): MultiSet[B] = ???

  override def flatMap[A >: T, B](mapper: (A) => GenTraversableOnce[B]): MultiSet[B] = ???
}

object MultiSet {
  def apply[T](elems: T*): MultiSet[T] = if (elems.isEmpty) EmptyMultiSet
                                         else new MultiSetOnMap[T](elems: _*)

  // unapplySeq

  // (*) = unapplySeq

  def empty[T]: MultiSet[T] = EmptyMultiSet

  private object EmptyMultiSet extends MultiSet {
    override def size: Int = 0

    override def add[A](elems: A*): MultiSet[A] = new MultiSetOnMap[A](elems: _*)

    override def count[A](elem: A): Int = 0

    override def intersection[A](other: MultiSet[A]): MultiSet[A] = this

    override def union[A](other: MultiSet[A]): MultiSet[A] = other

    override def filter[A](predicate: (A) => Boolean): MultiSet[A] = this

    override def map[A, B](mapper: (A) => B): MultiSet[B] = this

    override def flatMap[A, B](mapper: (A) => GenTraversableOnce[B]): MultiSet[B] = this
  }
}
