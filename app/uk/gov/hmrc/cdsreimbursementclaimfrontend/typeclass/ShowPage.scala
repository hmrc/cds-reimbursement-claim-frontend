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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim

trait ShowPage[A] {
  def showPage(journey: A): String
  def errorPage: String
}

object ShowPage {

  // tc instances
  implicit val showErrorPage: ShowPage[ErrorPage] =
    ShowPage((value: ErrorPage) => value.error)

  implicit val showSinglePageJourney: ShowPage[SingleJourney] =
    ShowPage((value: SingleJourney) => value.answer)

  implicit val showDraftClaimPageJourney: ShowPage[DraftClaim] =
    ShowPage((value: DraftClaim) => value.toString)

  def apply[A](func: A => String): ShowPage[A] = new ShowPage[A] {
    def showPage(value: A): String = func(value)
    def errorPage                  = "there is an error"
  }
}

object syntax {
  // polymorphic functions to be used by end user
  def getJourneyMeta[A : ShowPage](journey: Option[A]): String = journey match {
    case Some(value) => implicitly[ShowPage[A]].showPage(value)
    case None        => implicitly[ShowPage[ErrorPage]].errorPage
  }
}
