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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Results.NotFound
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeatureSwitchService @Inject() (configuration: Configuration) {

  def forName(name: String): FeatureName = // scalastyle:ignore cyclomatic.complexity
    name match {
      case CurrentMonthAdjustment.name => CurrentMonthAdjustment
      case BulkClaim.name              => BulkClaim
    }

  sealed trait FeatureName {

    val name: String
    val confPropertyName: String   = s"features.$name"
    val systemPropertyName: String = s"features.$name"

    def isEnabled(): Boolean =
      sys.props.get(systemPropertyName).map(_.toBoolean).getOrElse {
        configuration.getOptional[Boolean](confPropertyName).getOrElse(false)
      }

    def enable(): Unit =
      setProp(true)

    def disable(): Unit =
      setProp(false)

    def setProp(value: Boolean): Unit = {
      val _ = sys.props += (systemPropertyName -> value.toString)
    }

    def action(implicit
      errorHandler: ErrorHandler,
      cc: MessagesControllerComponents
    ): ActionBuilder[Request, AnyContent] with ActionFilter[Request] =
      new ActionBuilder[Request, AnyContent] with ActionFilter[Request] {

        def filter[A](input: Request[A]): Future[Option[Result]] = Future.successful {
          isEnabled() match {
            case true  => None
            case false => Some(NotFound(errorHandler.notFoundTemplate(input)))
          }
        }

        override def parser: BodyParser[AnyContent] = cc.parsers.defaultBodyParser

        override protected def executionContext: ExecutionContext = cc.executionContext
      }
  }

  case object CurrentMonthAdjustment extends { val name = "current-month-adjustment" } with FeatureName

  case object BulkClaim extends { val name = "bulk-claim" } with FeatureName

}
