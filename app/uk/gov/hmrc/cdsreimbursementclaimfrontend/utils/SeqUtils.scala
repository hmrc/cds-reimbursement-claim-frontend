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

import cats.data.NonEmptyList

trait SeqUtils {
  implicit class SeqOps[A](val seq: Seq[A]) {

    final def containsEachItemOf(other: Seq[A]): Boolean =
      other.forall(seq.contains(_))

    final def nextAfter(item: A): Option[A] = {
      val i = seq.indexOf(item)
      if (i < 0 || i >= seq.size - 1) None
      else Some(seq(i + 1))
    }

    final def noneIfEmpty: Option[Seq[A]] =
      if (seq.isEmpty) None else Some(seq)

    final def nonEmptyList: Option[NonEmptyList[A]] =
      NonEmptyList.fromList(seq.toList)
  }

  implicit class MapOps[K, V](val map: Map[K, V]) {

    final def noneIfEmpty: Option[Map[K, V]] =
      if (map.isEmpty) None else Some(map)
  }
}

object SeqUtils extends SeqUtils
