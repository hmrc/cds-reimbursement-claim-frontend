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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import play.api.Configuration
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait AuthActionSpec { this: MockFactory =>

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val (signInUrl, origin, selfBaseUrl) = ("sign-in", "origin", "self-base-url")

  val config: Configuration =
    Configuration(
      ConfigFactory.parseString(
        s"""
           |bas-gateway.signInUrl = "$signInUrl"
           |gg.origin             = "$origin"
           |self.url              = "$selfBaseUrl"
           |limited-access-eori-csv-base64 = "R0IwMDAwMDAwMDAwMDAwMDEsR0IwMDAwMDAwMDAwMDAwMDIK"
    """.stripMargin
      )
    )

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(
    result: Future[R]
  ): Any =
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[R])(
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(predicate, retrieval, *, *)
      .returning(result)

}
