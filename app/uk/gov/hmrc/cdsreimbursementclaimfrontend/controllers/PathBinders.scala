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

import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

object PathBinders {

  implicit def associatedMrnIndexPathBinder(implicit intBinder: PathBindable[Int]): PathBindable[AssociatedMrnIndex] =
    new PathBindable[AssociatedMrnIndex] {
      override def bind(key: String, value: String): Either[String, AssociatedMrnIndex] =
        for {
          index <- intBinder.bind(key, value)
          user  <- if (index >= 2) Right(AssociatedMrnIndex.fromUrlIndex(index))
                   else Left("Invalid MRN index")
        } yield user

      override def unbind(key: String, associatedMrnIndex: AssociatedMrnIndex): String =
        AssociatedMrnIndex.toUrlIndex(associatedMrnIndex).toString
    }

}
