/*
 * Copyright 2021 HM Revenue & Customs
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

/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json._
import scala.reflect.ClassTag

/** Helper trait providing JSON formatter based on the set of subtype formatters.
  * Designed to be mixed in the companion object of the trait and as a typeclass.
  * @tparam A sealed trait type
  */
@SuppressWarnings(
  Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Throw", "org.wartremover.warts.Equals")
)
trait TraitFormat[A] {

  /** Formatters of the member classes. */
  val members: Seq[Member[_ <: A]]

  final def keyOf[T : ClassTag]: String =
    implicitly[ClassTag[T]].runtimeClass.getSimpleName

  final def keyOf(value: Any): String =
    value.getClass.getSimpleName

  final def get[T <: A](mapInstance: Map[String, A])(implicit cta: ClassTag[A], ctt: ClassTag[T]): Option[T] = {
    val key = keyOf[T]
    formatMap.get(key).flatMap { member =>
      mapInstance.get(key).flatMap(value => member.checkType(value).map(_.asInstanceOf[T]))
    }
  }

  final def set[T <: A : ClassTag](value: T, mapInstance: Map[String, A]): Map[String, A] = {
    val key = keyOf[T]
    mapInstance + ((key, value))
  }

  final def remove[T <: A : ClassTag](mapInstance: Map[String, A]): Map[String, A] = {
    val key = keyOf[T]
    mapInstance - key
  }

  private final lazy val formatMap: Map[String, Member[_ <: A]] =
    members.map(m => (m.key, m)).toMap

  protected case class Member[T <: A : ClassTag]()(implicit format: Format[T]) {
    val key = keyOf[T]

    def writes(value: A): JsValue =
      format.writes(value.asInstanceOf[T])

    def reads(json: JsValue): JsResult[A] =
      format.reads(json).map(_.asInstanceOf[A])

    def checkType[T1 : ClassTag](value: T1): Option[T1] =
      if (implicitly[ClassTag[T1]].runtimeClass eq implicitly[ClassTag[T]].runtimeClass)
        Some(value)
      else
        None
  }

  object CanSerialize {
    def unapply(entry: (String, A)): Option[(String, JsValue)] = {
      val (key, value) = entry
      formatMap
        .get(key)
        .map(_.writes(value))
        .map(jsvalue => (key, jsvalue))
    }
  }

  object CanDeserialize {
    def unapply(field: (String, JsValue)): Option[(String, A)] = {
      val (key, jsvalue) = field
      formatMap
        .get(key)
        .map(
          _.reads(jsvalue)
            .map(value => (key, value))
            .getOrElse(throw new Exception(s"Failure de-serializing ${Json.stringify(jsvalue)}"))
        )
    }
  }

  implicit final val mapFormat: Format[Map[String, A]] = Format(
    Reads {
      case o: JsObject =>
        JsSuccess(
          o.fields.collect { case CanDeserialize(key, value) => (key, value) }.toMap
        )

      case json => JsError(s"Expected json object but got ${json.getClass.getSimpleName}")
    },
    Writes.apply { mapInstance =>
      JsObject(
        mapInstance.toSeq
          .collect { case CanSerialize(key, jsvalue) =>
            key -> jsvalue
          }
      )
    }
  )

  implicit final val format: Format[A] = Format(
    Reads {
      case o: JsObject =>
        o.fields
          .collect { case CanDeserialize(_, value) => value }
          .headOption
          .map[JsResult[A]](JsSuccess.apply(_))
          .getOrElse(JsError(s"Could not deserialize $o"))

      case json => JsError(s"Expected json object but got ${json.getClass.getSimpleName}")
    },
    Writes.apply { value =>
      (keyOf(value), value) match {
        case CanSerialize(key, jsvalue) => Json.obj(key -> jsvalue)
        case _                          => JsNull
      }
    }
  )

  /** Instance of a typeclass declaration */
  implicit final val traitFormat: TraitFormat[A] = this

}
