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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.jsoup.nodes
import org.scalatest.OptionValues
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.HtmlParseSupport

abstract class CheckYourAnswersSummarySpec
    extends PropertyBasedControllerSpec
    with OptionValues
    with SessionSupport
    with AuthSupport {

  private val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

}

object CheckYourAnswersSummarySpec extends HtmlParseSupport {
  import scala.jdk.CollectionConverters._

  implicit class DOMDocOps(private val document: nodes.Document) {

    def extractHeaders(): Seq[String] =
      document
        .select("#main-content > div > div > h2")
        .content

    def extractSummaries(): Seq[(String, String)] =
      document
        .select("#main-content > div > div > dl > div")
        .asScala
        .flatMap { element =>
          val label      = element.select("dt").text()
          val value      = element.select("dd").not(".govuk-summary-list__actions")
          val paragraphs = value.select("p")

          if paragraphs.isEmpty then Seq((label, value.text()))
          else
            paragraphs.content.map { s =>
              (label, s.replace("<br>", " "))
            }
        }
        .toList
  }
}
