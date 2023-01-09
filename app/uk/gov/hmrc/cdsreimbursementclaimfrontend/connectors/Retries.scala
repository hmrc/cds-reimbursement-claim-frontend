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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import akka.actor.ActorSystem
import akka.pattern.after
import play.api.Configuration
import play.api.Logger
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.http.logging.Mdc

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

trait Retries {

  protected def actorSystem: ActorSystem

  val shouldRetry: Try[HttpResponse] => Boolean =
    _.fold(NonFatal(_), _.status >= 499)

  val retryReason: HttpResponse => String =
    response => s"received $response"

  def retry[A](intervals: FiniteDuration*)(shouldRetry: Try[A] => Boolean, retryReason: A => String)(
    block: => Future[A]
  )(implicit ec: ExecutionContext): Future[A] = {

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def loop(remainingIntervals: Seq[FiniteDuration])(mdcData: Map[String, String])(block: => Future[A]): Future[A] =
      // scheduling will loose MDC data. Here we explicitly ensure it is available on block.
      Mdc
        .withMdc(block, mdcData)
        .flatMap(result =>
          if (remainingIntervals.nonEmpty && shouldRetry(Success(result))) {
            val delay = remainingIntervals.headOption.getOrElse(FiniteDuration.apply(1, "s"))
            Logger(getClass).warn(s"Will retry in $delay due to ${retryReason(result)}")
            after(delay, actorSystem.scheduler)(loop(remainingIntervals.drop(1))(mdcData)(block))
          } else
            Future.successful(result)
        )
        .recoverWith { case e: Throwable =>
          if (remainingIntervals.nonEmpty && shouldRetry(Failure(e))) {
            val delay = remainingIntervals.headOption.getOrElse(FiniteDuration.apply(1, "s"))
            Logger(getClass).warn(s"Will retry in $delay due to ${e.getClass.getName()}: ${e.getMessage()}")
            after(delay, actorSystem.scheduler)(loop(remainingIntervals.drop(1))(mdcData)(block))
          } else {
            Logger(getClass).error(s"Limit of ${intervals.size} has been reached, still failing ...")
            Future.failed(e)
          }
        }
    loop(intervals)(Mdc.mdcData)(block)
  }

}

object Retries {
  def getConfIntervals(serviceKey: String, configuration: Configuration): Seq[FiniteDuration] =
    configuration
      .getOptional[Seq[String]](s"microservice.services.$serviceKey.retryIntervals")
      .map(_.map(Duration.create).map(d => FiniteDuration(d.length, d.unit)))
      .getOrElse(Seq.empty)
}
