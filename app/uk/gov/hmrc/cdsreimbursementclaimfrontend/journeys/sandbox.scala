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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

object sandbox {

  trait Journey {
    type Type
  }

  trait HaveMrn {
    this: Journey =>

    def submitMrn(mrn: MRN): Either[String, this.Type]
  }

  final case class X() extends Journey with HaveMrn {

    type Type = X

    override def submitMrn(mrn: MRN): Either[String, X] =
      Right(new X)

  }

}
