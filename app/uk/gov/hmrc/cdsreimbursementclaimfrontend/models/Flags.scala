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

import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

/** Simple container for bit flags */
@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.Equals"))
final case class Flags private (flags: Long) {

  /** Check if flag is set on the given position */
  def check(pos: Int): Boolean =
    ((flags >>> pos) & 1L) == 1L

  def set(pos: Int): Flags =
    if (pos < 0 || pos >= 64) throw new IndexOutOfBoundsException("Flags index must be in range [0,63]")
    else new Flags(flags | (1L << pos))

  def unset(pos: Int): Flags =
    if (pos < 0 || pos >= 64) throw new IndexOutOfBoundsException("Flags index must be in range [0,63]")
    else new Flags(flags & ~(1L << pos))

  /** Check whether all flags are set up to the given position */
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While"))
  def checkAllSet(maxPos: Int): Boolean =
    if (maxPos < 0 || maxPos >= 64) throw new IndexOutOfBoundsException("Flags index must be in range [0,63]")
    else {
      var i = 0
      var c = true
      while (c && (i <= maxPos)) {
        c = c & ((flags >>> i) & 1L) == 1L
        i = i + 1
      }
      c
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While"))
  def firstSet: Option[Int] =
    if (flags == 0L) None
    else if (flags == -1L) { Some(0) }
    else {
      var i = -1
      var c = true
      var f = flags
      while (c) {
        i = i + 1
        c = (f & 1L) != 1L
        f = f >>> 1
      }
      if (c) None else Some(i)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While"))
  def firstNotSet: Option[Int] =
    if (flags == 0L) Some(0)
    else if (flags == -1L) { None }
    else {
      var i = -1
      var c = true
      var f = flags
      while (c) {
        i = i + 1
        c = (f & 1L) == 1L
        f = f >>> 1
      }
      if (c) None else Some(i)
    }

  override def toString: String =
    java.lang.Long.toBinaryString(flags)

}

object Flags {

  val empty: Flags = new Flags(+0L)
  val full: Flags  = new Flags(-1L)

  def parse(s: String): Flags =
    Flags(java.lang.Long.parseUnsignedLong(s, 2))

  implicit val format: Format[Flags] =
    SimpleStringFormat[Flags](Flags.parse, _.toString)
}
