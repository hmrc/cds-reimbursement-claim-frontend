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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.Locale

object EnrolmentConfig {

  object EoriEnrolment {

    val key = "HMRC-CUS-ORG"

    val eoriEnrolmentIdentifier = "EORINumber"

  }

  def getLimitedAccessEoriSet(config: Configuration): Set[Eori] =
    try
      config
        .getOptional[String]("limited-access-securities-eori-csv-base64")
        .map(s => Base64.getDecoder().decode(s.getBytes(StandardCharsets.UTF_8)))
        .map(a => new String(a, StandardCharsets.UTF_8))
        .map(
          _.split(',')
            .map(_.trim.toUpperCase(Locale.ENGLISH))
            .map(s => new Eori(s))
            .toSet
        )
        .getOrElse(Set.empty)
    catch {
      case e: Exception =>
        throw new Exception("Error while parsing 'limited-access-securities-eori-csv-base64' config property", e)
    }

}
