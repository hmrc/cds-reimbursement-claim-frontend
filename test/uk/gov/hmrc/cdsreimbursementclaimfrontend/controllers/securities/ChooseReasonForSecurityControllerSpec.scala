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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class ChooseReasonForSecurityControllerSpec extends ControllerSpec
  with AuthSupport
  with SessionSupport
  with BeforeAndAfterEach
  with ScalaCheckPropertyChecks  {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseReasonForSecurityController = instanceOf[ChooseReasonForSecurityController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "choose-reason-for-security.securities"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Securities)

  //def validateChooseFileTypePage(doc: Document, journey: RejectedGoodsSingleJourney) = {
  //    radioItems(doc) should contain theSameElementsAs Seq(
  //      ("Additional supporting documents", "AdditionalSupportingDocuments"),
  //      ("Calculation worksheet", "CalculationWorksheet"),
  //      ("Commercial invoice", "CommercialInvoice"),
  //      ("Correspondence between trader and agent", "CorrespondenceTrader"),
  //      ("Documentary proof that the goods are faulty or not what you ordered", "DocumentaryProofFaultyOrNotWhatOrdered"),
  //      ("Import and export declaration", "ImportAndExportDeclaration"),
  //      ("Letter of authority", "LetterOfAuthority"),
  //      ("Proof of export or destruction", "ProofOfExportOrDestruction"),
  //      (
  //        if (journey.answers.supportingEvidences.isEmpty)
  //          "I have no documents to upload"
  //        else
  //          "I have no more documents to upload",
  //        "none"
  //      )
  //    )
  //    hasContinueButton(doc)
  //  }

  

}
