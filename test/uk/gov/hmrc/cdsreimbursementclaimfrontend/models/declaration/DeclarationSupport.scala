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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

trait DeclarationSupport {
  implicit class TestDisplayDeclaration(val declaration: DisplayDeclaration) {
    def withAllSubsidiesPaymentMethod(): DisplayDeclaration =
      declaration.copy(displayResponseDetail =
        declaration.displayResponseDetail.copy(ndrcDetails =
          declaration.displayResponseDetail.ndrcDetails.map(_.map(_.copy(paymentMethod = "006")))
        )
      )

    def withSomeSubsidiesPaymentMethod(): DisplayDeclaration =
      declaration.copy(displayResponseDetail =
        declaration.displayResponseDetail.copy(ndrcDetails =
          declaration.displayResponseDetail.ndrcDetails.map(_.zipWithIndex.map { case (ndrcDetails, index) =>
            if (index % 2 == 0)
              ndrcDetails.copy(paymentMethod = "006")
            else
              ndrcDetails
          })
        )
      )
  }
}
