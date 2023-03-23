/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import scala.collection.MapFactory
import scala.collection.immutable.Map
import scala.collection.mutable.Builder
import scala.collection.mutable.LinkedHashMap

/** A map keeping items in the order they were first appended, like mutable LinkedHashMap. */
@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.AsInstanceOf"))
class OrderedMap[A, B](private val underlying: LinkedHashMap[A, Any]) extends Map[A, B] with Serializable {

  override def empty                                                    = OrderedMap.empty[A, B]
  override def size: Int                                                = underlying.size
  override def isEmpty: Boolean                                         = underlying.isEmpty
  override def get(key: A): Option[B]                                   = underlying.get(key).map(_.asInstanceOf[B])
  override def iterator: Iterator[(A, B)]                               = underlying.iterator.asInstanceOf[Iterator[(A, B)]]
  override def +[B1 >: B](kv: (A, B1)): OrderedMap[A, B1]               = new OrderedMap(underlying.clone.+=(kv))
  override def removed(key: A): OrderedMap[A, B]                        = new OrderedMap(underlying.clone().-=(key))
  override def updated[V1 >: B](key: A, value: V1): OrderedMap[A, V1]   = new OrderedMap(
    underlying.clone.+=((key, value))
  )
  override def map[A2, B2](f: ((A, B)) => (A2, B2)): OrderedMap[A2, B2] =
    new OrderedMap(underlying.clone.asInstanceOf[LinkedHashMap[A, B]].map(f))

}

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.MutableDataStructures"))
object OrderedMap extends MapFactory[OrderedMap] {

  override def empty[A, B]: OrderedMap[A, B]                             = new OrderedMap(LinkedHashMap.empty[A, Any])
  override def apply[A, B](kv: (A, B)*): OrderedMap[A, B]                = new OrderedMap(LinkedHashMap.from(kv))
  def apply[A, B](other: Map[A, B]): OrderedMap[A, B]                    = new OrderedMap(LinkedHashMap.from(other))
  override def from[A, B](other: IterableOnce[(A, B)]): OrderedMap[A, B] = new OrderedMap(LinkedHashMap.from(other))

  override def newBuilder[A, B]: Builder[(A, B), OrderedMap[A, B]] =
    new OrderedMapBuilder

}

private class OrderedMapBuilder[A, B] extends Builder[(A, B), OrderedMap[A, B]] {

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def addOne(elem: (A, B)): this.type = {
    builder.addOne(elem)
    this
  }

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  val builder = LinkedHashMap.newBuilder[A, Any]

  override def clear(): Unit =
    builder.clear()

  override def result(): OrderedMap[A, B] =
    new OrderedMap(builder.result())

}
