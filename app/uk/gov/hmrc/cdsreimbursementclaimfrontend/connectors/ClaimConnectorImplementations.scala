/*
 * Copyright 2026 HM Revenue & Customs
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

import com.google.inject.Inject
import org.apache.pekko.actor.ActorSystem
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.*
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.ExecutionContext

// ----------------------------------------------------
// Overpayments - Single
// ----------------------------------------------------

@Singleton
class OverpaymentsSingleClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      OverpaymentsSingleClaimConnector.Request,
      OverpaymentsSingleClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "overpayments-single"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        OverpaymentsSingleClaimConnector.Request,
        OverpaymentsSingleClaim.Output
      ](
        _.claim,
        OverpaymentsSingleClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = OverpaymentsSingleClaimConnector.Exception.apply
    )(
      ec,
      OverpaymentsSingleClaimConnector.requestFormat,
      OverpaymentsSingleClaimConnector.responseFormat
    )
    with OverpaymentsSingleClaimConnector

// ----------------------------------------------------
// Overpayments - Multiple
// ----------------------------------------------------

@Singleton
class OverpaymentsMultipleClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      OverpaymentsMultipleClaimConnector.Request,
      OverpaymentsMultipleClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "overpayments-multiple"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        OverpaymentsMultipleClaimConnector.Request,
        OverpaymentsMultipleClaim.Output
      ](
        _.claim,
        OverpaymentsMultipleClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = OverpaymentsMultipleClaimConnector.Exception.apply
    )(
      ec,
      OverpaymentsMultipleClaimConnector.requestFormat,
      OverpaymentsMultipleClaimConnector.responseFormat
    )
    with OverpaymentsMultipleClaimConnector

// ----------------------------------------------------
// Overpayments - Scheduled
// ----------------------------------------------------

@Singleton
class OverpaymentsScheduledClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      OverpaymentsScheduledClaimConnector.Request,
      OverpaymentsScheduledClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "overpayments-scheduled"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        OverpaymentsScheduledClaimConnector.Request,
        OverpaymentsScheduledClaim.Output
      ](
        _.claim,
        OverpaymentsScheduledClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = OverpaymentsScheduledClaimConnector.Exception.apply
    )(
      ec,
      OverpaymentsScheduledClaimConnector.requestFormat,
      OverpaymentsScheduledClaimConnector.responseFormat
    )
    with OverpaymentsScheduledClaimConnector

// ----------------------------------------------------
// Rejected Goods - Single
// ----------------------------------------------------

@Singleton
class RejectedGoodsSingleClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      RejectedGoodsSingleClaimConnector.Request,
      RejectedGoodsSingleClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "rejected-goods-single"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        RejectedGoodsSingleClaimConnector.Request,
        RejectedGoodsSingleClaim.Output
      ](
        _.claim,
        RejectedGoodsSingleClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = RejectedGoodsSingleClaimConnector.Exception.apply
    )(
      ec,
      RejectedGoodsSingleClaimConnector.requestFormat,
      RejectedGoodsSingleClaimConnector.responseFormat
    )
    with RejectedGoodsSingleClaimConnector

// ----------------------------------------------------
// Rejected Goods - Multiple
// ----------------------------------------------------

@Singleton
class RejectedGoodsMultipleClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      RejectedGoodsMultipleClaimConnector.Request,
      RejectedGoodsMultipleClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "rejected-goods-multiple"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        RejectedGoodsMultipleClaimConnector.Request,
        RejectedGoodsMultipleClaim.Output
      ](
        _.claim,
        RejectedGoodsMultipleClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = RejectedGoodsMultipleClaimConnector.Exception.apply
    )(
      ec,
      RejectedGoodsMultipleClaimConnector.requestFormat,
      RejectedGoodsMultipleClaimConnector.responseFormat
    )
    with RejectedGoodsMultipleClaimConnector

// ----------------------------------------------------
// Rejected Goods - Scheduled
// ----------------------------------------------------

@Singleton
class RejectedGoodsScheduledClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      RejectedGoodsScheduledClaimConnector.Request,
      RejectedGoodsScheduledClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "rejected-goods-scheduled"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        RejectedGoodsScheduledClaimConnector.Request,
        RejectedGoodsScheduledClaim.Output
      ](
        _.claim,
        RejectedGoodsScheduledClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = RejectedGoodsScheduledClaimConnector.Exception.apply
    )(
      ec,
      RejectedGoodsScheduledClaimConnector.requestFormat,
      RejectedGoodsScheduledClaimConnector.responseFormat
    )
    with RejectedGoodsScheduledClaimConnector

// ----------------------------------------------------
// Securities
// ----------------------------------------------------

@Singleton
class SecuritiesClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  actorSystem: ActorSystem,
  uploadDocumentsConnector: UploadDocumentsConnector
)(implicit ec: ExecutionContext)
    extends ClaimConnector[
      SecuritiesClaimConnector.Request,
      SecuritiesClaimConnector.Response
    ](
      config = ClaimConnectorConfig.forClaim(
        servicesConfig,
        "securities"
      ),
      http = http,
      configuration = configuration,
      actorSystem = actorSystem,
      uploadDocumentsConnector = uploadDocumentsConnector,
      mitigation = ClaimMitigation.forClaim[
        SecuritiesClaimConnector.Request,
        SecuritiesClaim.Output
      ](
        _.claim,
        SecuritiesClaimConnector.Request.apply,
        (claim, files) =>
          claim.copy(
            supportingEvidences = claim.supportingEvidences ++ files
          )
      ),
      exceptionFactory = SecuritiesClaimConnector.Exception.apply
    )(
      ec,
      SecuritiesClaimConnector.requestFormat,
      SecuritiesClaimConnector.responseFormat
    )
    with SecuritiesClaimConnector
