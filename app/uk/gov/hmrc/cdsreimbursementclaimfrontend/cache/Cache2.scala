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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.cache

import cats.data.{EitherT, OptionT}
import cats.syntax.either._
import play.api.libs.json.{Json, Reads, Writes}
import uk.gov.hmrc.cache.model.Id
import uk.gov.hmrc.cache.repository.CacheRepository
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.{ExecutionContext, Future}

trait Cache2 {

  val cacheRepository: CacheRepository

  val sessionKey: String

  protected def get[A : Reads](
    id: String
  )(implicit ec: ExecutionContext): OptionT[Future, Either[Error, A]] =
    for {
      cache   <- OptionT(cacheRepository.findById(id))
      jsValue <- OptionT.fromOption[Future](cache.data)
    } yield (jsValue \ sessionKey)
      .validate[A]
      .asEither
      .leftMap(e ⇒
        Error(
          s"Could not parse session data from mongo: ${e.mkString("; ")}"
        )
      )

  protected def store[A : Writes](id: String, a: A)(implicit
    ec: ExecutionContext
  ): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      cacheRepository
        .createOrUpdate(Id(id), sessionKey, Json.toJson(a))
        .map[Either[Error, Unit]] { dbUpdate ⇒
          if (dbUpdate.writeResult.inError)
            Left(
              Error(
                dbUpdate.writeResult.errmsg.getOrElse(
                  "unknown error during inserting session data in mongo"
                )
              )
            )
          else
            Right(())
        }
        .recover { case e ⇒ Left(Error(e)) }
    })

}
