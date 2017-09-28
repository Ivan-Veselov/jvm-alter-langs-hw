package ru.spbau.bachelors2015.veselov.multiset.immutable

import scala.collection.GenTraversableOnce

sealed abstract class MultiSet[+T] {
  def size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = size > 0

  // apply

  def add[A >: T](elem: A): MultiSet[A]

  // def find[A >: T](elem: A): Option[A]

  def count[A >: T](elem: A): Int

  def intersection[A >: T](multiHashSet: MultiSet[A]): MultiSet[A]

  def &[A >: T](multiHashSet: MultiSet[A]): MultiSet[A] = intersection(multiHashSet)

  def union[A >: T](multiHashSet: MultiSet[A]): MultiSet[A]

  def |[A >: T](multiHashSet: MultiSet[A]): MultiSet[A] = union(multiHashSet)

  def filter[A >: T](predicate: A => Boolean): MultiSet[A]

  def map[A >: T, B](mapper: A => B): MultiSet[B]

  def flatMap[A >: T, B](mapper: A => GenTraversableOnce[B]): MultiSet[B]
}

object MultiSet {
  def apply[T](elements: T*): MultiSet[T] = ???

  // unapplySeq

  // (*) = unapplySeq

  def empty[T]: MultiSet[T] = ???
}
