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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.moneyGen

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  "Entry Claim Amount Validation" must {
    val form        = EnterClaimController.entryClaimAmountForm
    val paidAmount  = "enter-claim.paid-amount"
    val claimAmount = "enter-claim.claim-amount"

    val goodData = Map(
      paidAmount  -> "99999999999.99",
      claimAmount -> "0.01"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "paidAmount" should {
      "Accept shortest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject paidAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
      "Reject paidAmount too many decimals" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
      "Reject paidAmount too long" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
    }

    "claimAmount" should {
      "Accept shortest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject claimAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
      "Reject claimAmount too many decimals" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
      "Reject claimAmount too long" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
      }
    }

    "MRN Claim Amount Validation" must {
      val form        = EnterClaimController.mrnClaimAmountForm(BigDecimal("99999999999.99"))
      val claimAmount = "enter-claim"

      val goodData = Map(
        claimAmount -> "0.01"
      )

      "accept good declaration details" in {
        val errors = form.bind(goodData).errors
        errors shouldBe Nil
      }

      "claimAmount" should {
        "Accept shortest possible claimAmount" in {
          val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
          errors shouldBe Nil
        }
        "Accept longest possible claimAmount" in {
          val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
          errors shouldBe Nil
        }
        "Reject claimAmount decimals only too many" in {
          val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
          errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
        }
        "Reject claimAmount too many decimals" in {
          val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
          errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
        }
        "Reject claimAmount too long" in {
          val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
          errors.headOption.getOrElse(fail()).messages shouldBe List("error.real.precision")
        }
      }
    }
  }
}
