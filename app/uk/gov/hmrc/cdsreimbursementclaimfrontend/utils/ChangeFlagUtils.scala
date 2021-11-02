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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import play.api.mvc._

/** Change flag is a parameter we add to the URL to mark requests which have originated
  * from the CYA page and require routing back to CYA after submission.
  */
object ChangeFlagUtils {

  val changeFlag = "change"

  implicit class CallOps(val call: Call) {
    def maybeSetChangeFlag(implicit request: Request[_]): Call =
      if (isChangeRequest)
        call.copy(url = addChangeParameter(call.url))
      else
        call

    def setChangeFlag: Call =
      call.copy(url = addChangeParameter(call.url))
  }

  def isChangeRequest(implicit request: Request[_]): Boolean =
    request.getQueryString(changeFlag).isDefined

  private def addChangeParameter(url: String): String =
    if (url.indexOf("?") >= 0) s"$url&$changeFlag" else s"$url?$changeFlag"

}
