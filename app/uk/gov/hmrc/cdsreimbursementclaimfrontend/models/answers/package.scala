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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{AssociatedMrnIndex, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument

package object answers {

  type LeadMrn       = MRN
  type AssociatedMrn = MRN

  type SupportingEvidencesAnswer = NonEmptyList[UploadDocument]
  type DutiesSelectedAnswer      = NonEmptyList[Duty]
  type ClaimsAnswer              = NonEmptyList[Claim]

  type AssociatedMRNsAnswer               = NonEmptyList[AssociatedMrn]
  type AssociatedMRNsDeclarationAnswer    = NonEmptyList[DisplayDeclaration]
  type AssociatedMRNsDutiesSelectedAnswer = NonEmptyList[DutiesSelectedAnswer]
  type AssociatedMRNsClaimsAnswer         = NonEmptyList[ClaimsAnswer]

  implicit final class AnswersOps[A](val answer: Option[NonEmptyList[A]]) extends AnyVal {

    def get(i: AssociatedMrnIndex): Option[A] =
      get(i.toListIndex)

    def get(index: Int): Option[A] =
      answer.flatMap { list =>
        if (index < 0 || index >= list.length) None
        else list.toList.drop(index).headOption
      }

    def canAppendAt(i: AssociatedMrnIndex): Boolean =
      canAppendAt(i.toListIndex)

    def canAppendAt(index: Int): Boolean =
      index >= 0 && (answer match {
        case None       => index === 0
        case Some(list) => index === list.length
      })

    def isDefinedAt(i: AssociatedMrnIndex): Boolean =
      isDefinedAt(i.toListIndex)

    def isDefinedAt(index: Int): Boolean =
      index >= 0 && answer.exists(_.length > index)

    def replaceOrAppend(i: AssociatedMrnIndex, item: A): Either[String, Option[NonEmptyList[A]]] =
      replaceOrAppend(i.toListIndex, item)

    def replaceOrAppend(index: Int, item: A): Either[String, Option[NonEmptyList[A]]] =
      if (index < 0) Left("Index must be greater or equal to zero")
      else
        answer match {
          case None if index === 0                => Right(Some(NonEmptyList(item, Nil)))
          case None                               => Left(s"Expected zero but was $index")
          case Some(list) if index <= list.length =>
            Right(NonEmptyList.fromList(list.toList.take(index) ::: item :: list.toList.drop(index + 1)))
          case Some(list)                         => Left(s"Expected index lower or equal to ${list.length} but was $index")
        }

    def remove(i: AssociatedMrnIndex): Option[NonEmptyList[A]] =
      remove(i.toListIndex)

    def remove(index: Int): Option[NonEmptyList[A]] =
      if (index < 0) answer
      else
        answer.flatMap { list =>
          NonEmptyList.fromList(list.toList.take(index) ::: list.toList.drop(index + 1))
        }

    def list: List[A] = answer.map(_.toList).getOrElse(Nil)

    def listAllElementsExceptAt(i: AssociatedMrnIndex): List[A] =
      listAllElementsExceptAt(i.toListIndex)

    def listAllElementsExceptAt(index: Int): List[A] =
      answer
        .map { list =>
          list.toList.take(index) ::: list.toList.drop(index + 1)
        }
        .getOrElse(Nil)

    def length: Int = answer.map(_.length).getOrElse(0)
  }

}
