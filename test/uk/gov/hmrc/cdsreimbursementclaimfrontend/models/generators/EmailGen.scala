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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email

import java.util.Locale

object EmailGen {

  lazy val genEmail: Gen[Email] =
    for
      name   <- genStringWithMaxSizeOfN(max = 15).map(_.toLowerCase(Locale.UK))
      at      = "@"
      domain <- genStringWithMaxSizeOfN(max = 10).map(_.toLowerCase(Locale.UK))
      dotCom  = ".com"
    yield Email(Seq(name, at, domain, dotCom).mkString)

  implicit lazy val arbitraryEmail: Arbitrary[Email] =
    Arbitrary(genEmail)
}
