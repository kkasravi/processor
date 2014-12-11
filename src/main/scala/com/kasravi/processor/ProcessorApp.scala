package com.kasravi.processor

import collection.TraversableLike
import scala.collection.generic._
import collection.mutable.{Builder, ListBuffer}

object ProcessorApp extends App {

  object Processor extends TraversableFactory[Processor] {
    def newBuilder[A] = new ProcessorBuilder[A]

    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Processor[A]] =
      new CanBuildFrom[Coll, A, Processor[A]] {
        def apply(): Builder[A, Processor[A]] = new ProcessorBuilder()

        def apply(from: Coll): Builder[A, Processor[A]] = apply()
      }
  }

  case class Processor[A](list: List[A]) extends Traversable[A]
  with TraversableLike[A, Processor[A]]
  with GenericTraversableTemplate[A, Processor] {
    override def companion: GenericCompanion[Processor] = Processor

    def foreach[U](f: A => U) {
      list foreach f
    }

    override def seq = list
  }

  class ProcessorBuilder[A] extends Builder[A, Processor[A]] {
    private val list = new ListBuffer[A]()

    def +=(elem: A): this.type = {
      list += elem
      this
    }

    def clear() {
      list.clear()
    }

    def result(): Processor[A] = Processor(list.result())
  }

  val processor = Processor(List(1, 2, 3))
  val f = processor filter { x => x == 1}
  val m = processor map { x => x + 1}

}
