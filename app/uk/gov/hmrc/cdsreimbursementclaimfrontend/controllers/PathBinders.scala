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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutyType, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

import scala.util.Try

object PathBinders {

  lazy val stringBinder: PathBindable[String] = implicitly[PathBindable[String]]

  implicit val taxCodeBinder: PathBindable[TaxCode] =
    new PathBindable[TaxCode] {

      def bind(key: String, value: String): Either[String, TaxCode] =
        stringBinder.bind(key, value).map(TaxCode.apply)

      def unbind(key: String, dutyType: TaxCode): String =
        stringBinder.unbind(key, dutyType.value)
    }

  implicit val dutyTypeBinder: PathBindable[DutyType] =
    new PathBindable[DutyType] {

      override def bind(key: String, value: String): Either[String, DutyType] =
        stringBinder.bind(key, value).map(DutyType.apply)

      override def unbind(key: String, dutyType: DutyType): String =
        stringBinder.unbind(key, dutyType.repr)
    }

  implicit def associatedMrnIndexPathBinder(implicit intBinder: PathBindable[Int]): PathBindable[AssociatedMrnIndex] =
    new PathBindable[AssociatedMrnIndex] {
      def bind(key: String, value: String): Either[String, AssociatedMrnIndex] =
        for {
          index <- intBinder.bind(key, value)
          user  <- Either.cond(index >= 2, AssociatedMrnIndex.fromUrlIndex(index), "Invalid MRN index")
        } yield user

      def unbind(key: String, associatedMrnIndex: AssociatedMrnIndex): String =
        AssociatedMrnIndex.toUrlIndex(associatedMrnIndex).toString
    }

  implicit val journeyPathBindable: PathBindable[JourneyBindable] = new PathBindable[JourneyBindable] {

    def bind(key: String, value: String): Either[String, JourneyBindable] =
      Try(JourneyBindable.parse(value)).toEither.left.map(_.getMessage)

    def unbind(key: String, bindable: JourneyBindable): String =
      bindable.value
  }
}
