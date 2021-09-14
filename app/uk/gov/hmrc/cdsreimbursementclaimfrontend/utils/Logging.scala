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

import play.api.Logger
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._

trait Logging {

  val logger: Logger = Logger(this.getClass)

  def logAndDisplayError(
    description: String
  )(implicit errorHandler: ErrorHandler, request: Request[_]): Error => Result = {
    import errorHandler._
    error => {
      logger warn (description, error)
      errorResult()
    }
  }
}

object Logging {

  implicit class LoggerOps(private val l: Logger) extends AnyVal {
    def warn(msg: => String, error: => Error): Unit = {
      val idString = error.identifiers.map { case (k, v) => s"[$k: $v]" }.mkString(" ")
      error.throwable.fold(l.warn(s"$idString $msg ${error.message}"))(e =>
        l.warn(s"$idString $msg ${error.message}", e)
      )
    }
  }

}
