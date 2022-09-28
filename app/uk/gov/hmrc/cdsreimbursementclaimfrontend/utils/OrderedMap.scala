/*
 * Copyright 2022 HM Revenue & Customs
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

import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.MapLike
import scala.collection.immutable.AbstractMap

/** A map keeping items in the order they were first appended, like mutable LinkedHashMap. */
@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.AsInstanceOf"))
class OrderedMap[A, B](private val underlying: LinkedHashMap[A, Any])
    extends AbstractMap[A, B]
    with Map[A, B]
    with MapLike[A, B, OrderedMap[A, B]]
    with Serializable {

  override def empty                                      = OrderedMap.empty[A, B]
  override def size: Int                                  = underlying.size
  override def isEmpty: Boolean                           = underlying.isEmpty
  override def get(key: A): Option[B]                     = underlying.get(key).map(_.asInstanceOf[B])
  override def iterator: Iterator[(A, B)]                 = underlying.iterator.asInstanceOf[Iterator[(A, B)]]
  override def -(key: A): OrderedMap[A, B]                = new OrderedMap(underlying.clone().-=(key))
  override def +[B1 >: B](kv: (A, B1)): OrderedMap[A, B1] = new OrderedMap(underlying.clone.+=(kv))
}

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.MutableDataStructures"))
object OrderedMap {

  def empty[A, B]: OrderedMap[A, B]                             = new OrderedMap(LinkedHashMap.empty[A, Any])
  def apply[A, B](kv: (A, B)*): OrderedMap[A, B]                = new OrderedMap(LinkedHashMap(kv: _*))
  def apply[A, B](other: Map[A, B]): OrderedMap[A, B]           = new OrderedMap(LinkedHashMap(other.iterator.toSeq: _*))
  def apply[A, B](other: Traversable[(A, B)]): OrderedMap[A, B] = new OrderedMap(LinkedHashMap(other.toSeq: _*))
}
