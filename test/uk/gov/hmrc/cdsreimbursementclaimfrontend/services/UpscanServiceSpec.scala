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

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Call, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{FileUploadHelper, FileUploadHelperInstances}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ScheduledDocumentAnswer, SupportingEvidencesAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UploadRequest, UpscanUpload, UpscanUploadMeta}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UpscanServiceSpec extends AnyWordSpec with Matchers with MockFactory {

  val configuration: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        |  microservice {
        |    services {
        |     upscan-initiate {
        |       supporting-evidence {
        |         max-uploads = 10
        |       }
        |       schedule-of-mrn {
        |         max-uploads = 1
        |       }
              }
        |   }
        |}
        |""".stripMargin
    )
  )

  val mockUpscanConnector: UpscanConnector = mock[UpscanConnector]
  val mockUpscanService                    = new UpscanServiceImpl(mockUpscanConnector)
  val fileUploadConfig                     = new FileUploadConfig(configuration)
  val fileUploadInstances                  = new FileUploadHelperInstances(fileUploadConfig)

  val uploadReference: UploadReference = sample[UploadReference]
  val upscanUpload: UpscanUpload       = sample[UpscanUpload]

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  private val emptyJsonBody = "{}"

  "UpscanService" when {
    "receiving an upscan related request" must {
      import fileUploadInstances._

      "get the upscan upload reference data" when {
        "given a valid upload reference" in {
          val response = Right(HttpResponse(OK, Json.toJson(upscanUpload), Map[String, Seq[String]]().empty))
          (mockUpscanConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(uploadReference, *)
            .returning(EitherT.fromEither[Future](response))
          await(mockUpscanService.getUpscanUpload(uploadReference).value).isRight shouldBe true
        }

        "it receives an invalid response" in {
          val response = Right(HttpResponse(OK, JsString("Nope"), Map[String, Seq[String]]().empty))
          (mockUpscanConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(uploadReference, *)
            .returning(EitherT.fromEither[Future](response))
          await(mockUpscanService.getUpscanUpload(uploadReference).value).isLeft shouldBe true
        }

        "raise an error when the call fails" in {
          val response = Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody))
          (mockUpscanConnector
            .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
            .expects(uploadReference, *)
            .returning(EitherT.fromEither[Future](response))
          await(mockUpscanService.getUpscanUpload(uploadReference).value).isLeft shouldBe true
        }
      }

      "get the upscan file descriptors succeeds" when {
        "the upscan initiate service returns a successful response" in {
          val mockSuccess = Call("GET", "/mock-success")
          val mockFailure = Call("GET", "/mock-fail")
          val response    = Right(
            HttpResponse(
              OK,
              Json.toJson(UpscanUploadMeta("metadata", sample[UploadRequest])),
              Map[String, Seq[String]]().empty
            )
          )
          (mockUpscanConnector
            .initiate(_: Call, _: Call, _: UploadReference)(
              _: HeaderCarrier,
              _: FileUploadHelper[ScheduledDocumentAnswer]
            ))
            .expects(mockFailure, mockSuccess, *, *, *)
            .returning(EitherT.fromEither[Future](response))
          (mockUpscanConnector
            .saveUpscanUpload(_: UpscanUpload)(_: HeaderCarrier))
            .expects(*, *)
            .returning(EitherT.fromEither[Future](Right(HttpResponse(OK, emptyJsonBody))))
          await(
            mockUpscanService
              .initiate[ScheduledDocumentAnswer](mockFailure, (_: UploadReference) => mockSuccess)
              .value
          ).isRight shouldBe true
        }
      }

      "get the upscan file descriptors fails" when {
        "the upscan initiate service returns an unsuccessful response" in {
          val mockSuccess = Call("GET", "/mock-success")
          val mockFailure = Call("GET", "/mock-fail")
          (mockUpscanConnector
            .initiate(_: Call, _: Call, _: UploadReference)(
              _: HeaderCarrier,
              _: FileUploadHelper[SupportingEvidencesAnswer]
            ))
            .expects(mockFailure, mockSuccess, *, *, *)
            .returning(EitherT.fromEither[Future](Right(HttpResponse(BAD_REQUEST, emptyJsonBody))))
          await(
            mockUpscanService
              .initiate[SupportingEvidencesAnswer](mockFailure, (_: UploadReference) => mockSuccess)
              .value
          ).isLeft shouldBe true
        }

      }
    }
  }
}
