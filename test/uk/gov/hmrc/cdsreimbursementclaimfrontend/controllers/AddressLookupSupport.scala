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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.data.EitherT
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait AddressLookupSupport { this: MockFactory =>

  val addressLookupServiceMock: AddressLookupService = mock[AddressLookupService]

  def mockAddressLookupInitiation(
    eitherErrorOrUrl: Either[Error, URL]
  )(implicit
    ec: ExecutionContext
  ): CallHandler2[AddressLookupRequest, HeaderCarrier, EitherT[Future, Error, URL]] =
    (addressLookupServiceMock
      .initiate(_: AddressLookupRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](eitherErrorOrUrl))

  def mockAddressRetrieve(
    eitherErrorOrContactAddress: Either[Error, ContactAddress]
  )(implicit
    ec: ExecutionContext
  ): CallHandler2[UUID, HeaderCarrier, EitherT[Future, Error, ContactAddress]] =
    (addressLookupServiceMock
      .retrieveUserAddress(_: UUID)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](eitherErrorOrContactAddress))
}
