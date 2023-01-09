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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

sealed abstract class TaxCode(val value: String) extends Product with Serializable {
  override def toString: String = value
}

object TaxCode {

  def apply(value: String): TaxCode =
    TaxCodes.findUnsafe(value)

  def unapply(taxCode: TaxCode): Option[String] =
    Some(taxCode.value)

  case object A00 extends TaxCode("A00")
  case object A20 extends TaxCode("A20")
  case object A30 extends TaxCode("A30")
  case object A35 extends TaxCode("A35")
  case object A40 extends TaxCode("A40")
  case object A45 extends TaxCode("A45")
  case object B00 extends TaxCode("B00")
  case object A50 extends TaxCode("A50")
  case object A70 extends TaxCode("A70")
  case object A80 extends TaxCode("A80")
  case object A85 extends TaxCode("A85")
  case object A90 extends TaxCode("A90")
  case object A95 extends TaxCode("A95")
  case object B05 extends TaxCode("B05")

  case object NI407 extends TaxCode("407")
  case object NI411 extends TaxCode("411")
  case object NI412 extends TaxCode("412")
  case object NI413 extends TaxCode("413")
  case object NI415 extends TaxCode("415")
  case object NI419 extends TaxCode("419")
  case object NI421 extends TaxCode("421")
  case object NI422 extends TaxCode("422")
  case object NI423 extends TaxCode("423")
  case object NI425 extends TaxCode("425")
  case object NI429 extends TaxCode("429")
  case object NI431 extends TaxCode("431")
  case object NI433 extends TaxCode("433")
  case object NI435 extends TaxCode("435")
  case object NI438 extends TaxCode("438")
  case object NI440 extends TaxCode("440")
  case object NI441 extends TaxCode("441")
  case object NI442 extends TaxCode("442")
  case object NI443 extends TaxCode("443")
  case object NI444 extends TaxCode("444")
  case object NI445 extends TaxCode("445")
  case object NI446 extends TaxCode("446")
  case object NI447 extends TaxCode("447")
  case object NI451 extends TaxCode("451")
  case object NI461 extends TaxCode("461")
  case object NI462 extends TaxCode("462")
  case object NI463 extends TaxCode("463")
  case object NI473 extends TaxCode("473")
  case object NI481 extends TaxCode("481")
  case object NI483 extends TaxCode("483")
  case object NI485 extends TaxCode("485")
  case object NI487 extends TaxCode("487")
  case object NI511 extends TaxCode("511")
  case object NI520 extends TaxCode("520")
  case object NI521 extends TaxCode("521")
  case object NI522 extends TaxCode("522")
  case object NI540 extends TaxCode("540")
  case object NI541 extends TaxCode("541")
  case object NI542 extends TaxCode("542")
  case object NI546 extends TaxCode("546")
  case object NI551 extends TaxCode("551")
  case object NI556 extends TaxCode("556")
  case object NI561 extends TaxCode("561")
  case object NI570 extends TaxCode("570")
  case object NI571 extends TaxCode("571")
  case object NI572 extends TaxCode("572")
  case object NI589 extends TaxCode("589")
  case object NI591 extends TaxCode("591")
  case object NI592 extends TaxCode("592")
  case object NI595 extends TaxCode("595")
  case object NI597 extends TaxCode("597")
  case object NI611 extends TaxCode("611")
  case object NI615 extends TaxCode("615")
  case object NI619 extends TaxCode("619")
  case object NI623 extends TaxCode("623")
  case object NI627 extends TaxCode("627")
  case object NI633 extends TaxCode("633")
  case object NI99A extends TaxCode("99A")
  case object NI99B extends TaxCode("99B")
  case object NI99C extends TaxCode("99C")
  case object NI99D extends TaxCode("99D")

  implicit val taxCodeEq: Eq[TaxCode]         = Eq.fromUniversalEquals[TaxCode]
  implicit val taxCodeFormat: Format[TaxCode] = SimpleStringFormat(TaxCode(_), _.value)

  implicit val ordering: Ordering[TaxCode] =
    Ordering.fromLessThan { (t1: TaxCode, t2: TaxCode) =>
      val a = if (t1.value.head.isLetter) "0" + t1.value else t1.value
      val b = if (t2.value.head.isLetter) "0" + t2.value else t2.value
      implicitly[Ordering[String]].lt(a, b)
    }
}
