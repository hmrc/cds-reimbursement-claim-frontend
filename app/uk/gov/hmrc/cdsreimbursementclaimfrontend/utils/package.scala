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

package uk.gov.hmrc.cdsreimbursementclaimfrontend

import play.api.mvc.Result

import scala.concurrent.Future
import cats.data.NonEmptyList

package object util {

  implicit def toFuture: Result => Future[Result] = r => Future.successful(r)

  implicit class SeqOpts[A](seq: Seq[A]) {

    def someNelOrNone: Option[NonEmptyList[A]] =
      if (seq.isEmpty) None else Some(NonEmptyList.fromListUnsafe(seq.toList))
  }

}
