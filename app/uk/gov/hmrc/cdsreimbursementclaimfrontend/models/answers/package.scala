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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

package object answers {

  type LeadMrn = MRN

  type AssociatedMrn = MRN

  type AssociatedMRNsAnswer            = NonEmptyList[AssociatedMrn]
  type AssociatedMRNsDeclarationAnswer = NonEmptyList[DisplayDeclaration]
  type SupportingEvidencesAnswer       = NonEmptyList[UploadDocument]
  type DutiesSelectedAnswer            = NonEmptyList[Duty]
  type ClaimsAnswer                    = NonEmptyList[Claim]

  object SupportingEvidencesAnswer {
    def apply(evidence: UploadDocument): NonEmptyList[UploadDocument] =
      NonEmptyList.one(evidence)
  }

  object DutiesSelectedAnswer {
    def apply(head: Duty, tail: Duty*): NonEmptyList[Duty] = NonEmptyList.of(head, tail: _*)
    def apply(l: List[Duty]): Option[NonEmptyList[Duty]]   = NonEmptyList.fromList(l)
  }

  object DutyTypesSelectedAnswer {
    def apply(head: DutyType, tail: DutyType*): NonEmptyList[DutyType] = NonEmptyList.of(head, tail: _*)
    def apply(l: List[DutyType]): Option[NonEmptyList[DutyType]]       = NonEmptyList.fromList(l)
  }

  object ClaimsAnswer {
    def apply(head: Claim, tail: Claim*): NonEmptyList[Claim] = NonEmptyList.of(head, tail: _*)
    def apply(l: List[Claim]): Option[NonEmptyList[Claim]]    = NonEmptyList.fromList(l)
  }

  object AssociatedMRNsAnswer {

    def apply(mrn: AssociatedMrn): AssociatedMRNsAnswer =
      NonEmptyList.one(mrn)

    def apply(associatedMRNs: List[AssociatedMrn]): Option[AssociatedMRNsAnswer] =
      NonEmptyList.fromList(associatedMRNs)
  }

  object AssociatedMRNsDeclarationAnswer {
    def apply(declaration: DisplayDeclaration): AssociatedMRNsDeclarationAnswer =
      NonEmptyList.one(declaration)
  }

  implicit final class AnswersOps[A](val answer: Option[NonEmptyList[A]]) extends AnyVal {

    final def get(i: AssociatedMrnIndex): Option[A] =
      get(i.toListIndex)

    final def get(index: Int): Option[A] =
      answer.flatMap { list =>
        if (index < 0 || index >= list.length) None
        else list.toList.drop(index).headOption
      }

    final def canAppendAt(i: AssociatedMrnIndex): Boolean =
      canAppendAt(i.toListIndex)

    final def canAppendAt(index: Int): Boolean =
      index >= 0 && (answer match {
        case None       => index === 0
        case Some(list) => index === list.length
      })

    final def isDefinedAt(i: AssociatedMrnIndex): Boolean =
      isDefinedAt(i.toListIndex)

    final def isDefinedAt(index: Int): Boolean =
      index >= 0 && answer.map(_.length > index).getOrElse(false)

    final def replaceOrAppend(i: AssociatedMrnIndex, item: A): Either[String, Option[NonEmptyList[A]]] =
      replaceOrAppend(i.toListIndex, item)

    final def replaceOrAppend(index: Int, item: A): Either[String, Option[NonEmptyList[A]]] =
      if (index < 0) Left("Index must be greater or equal to zero")
      else
        answer match {
          case None if index === 0                => Right(Some(NonEmptyList(item, Nil)))
          case None                               => Left(s"Expected zero but was $index")
          case Some(list) if index <= list.length =>
            Right(NonEmptyList.fromList(list.toList.take(index) ::: item :: list.toList.drop(index + 1)))
          case Some(list)                         => Left(s"Expected index lower or equal to ${list.length} but was $index")
        }

    final def remove(i: AssociatedMrnIndex): Option[NonEmptyList[A]] =
      remove(i.toListIndex)

    final def remove(index: Int): Option[NonEmptyList[A]] =
      if (index < 0) answer
      else
        answer.flatMap { list =>
          NonEmptyList.fromList(list.toList.take(index) ::: list.toList.drop(index + 1))
        }

    final def list: List[A] = answer.map(_.toList).getOrElse(Nil)

    final def listAllElementsExceptAt(i: AssociatedMrnIndex): List[A] =
      listAllElementsExceptAt(i.toListIndex)

    final def listAllElementsExceptAt(index: Int): List[A] =
      answer
        .map { list =>
          list.toList.take(index) ::: list.toList.drop(index + 1)
        }
        .getOrElse(Nil)

    final def length: Int = answer.map(_.length).getOrElse(0)
  }

}
