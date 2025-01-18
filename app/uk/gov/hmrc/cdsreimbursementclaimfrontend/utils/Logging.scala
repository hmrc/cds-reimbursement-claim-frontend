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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import play.api.Logger
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging.*

trait Logging {

  val logger: Logger = Logger(this.getClass)

  def logAndDisplayError(
    description: String
  )(implicit errorHandler: ErrorHandler, request: Request[?]): Error => Result = {
    import errorHandler._
    error => {
      logger.warn(description, error)
      errorResult()
    }
  }

  def logAndDisplayError[T](
    description: String,
    error: T
  )(implicit errorHandler: ErrorHandler, request: Request[?], cdsError: CdsError[T]): Result = {
    import errorHandler._
    logger.warn(s"$description: ${cdsError.message(error)}")
    errorResult()
  }
}

object Logging {

  implicit class LoggerOps(private val l: Logger) {
    def warn(msg: => String, error: => Error): Unit = {
      val idString = error.identifiers.map { case (k, v) => s"[$k: $v]" }.mkString(" ")
      error.throwable.fold(l.warn(s"$idString $msg ${error.message}"))(e =>
        l.warn(s"$idString $msg ${error.message}", e)
      )
    }
  }

}

object PrettyPrint {

  /** Pretty prints a Scala value similar to its source represention.
    *
    * Credits: https://gist.github.com/carymrobbins/7b8ed52cd6ea186dbdf8
    *
    * @param a
    *   \- The value to pretty print.
    * @param indentSize
    *   \- Number of spaces for each indent.
    * @param maxElementWidth
    *   \- Largest element size before wrapping.
    * @param depth
    *   \- Initial depth to pretty print indents.
    * @return
    */

  def apply(a: Any, indentSize: Int = 2, maxElementWidth: Int = 30, depth: Int = 0): String = {
    val indent      = " " * depth * indentSize
    val fieldIndent = indent + (" " * indentSize)
    val thisDepth   = apply(_: Any, indentSize, maxElementWidth, depth)
    val nextDepth   = apply(_: Any, indentSize, maxElementWidth, depth + 1)
    a match {
      // Make Strings look similar to their literal form.
      case s: String                =>
        val replaceMap = Seq(
          "\n" -> "\\n",
          "\r" -> "\\r",
          "\t" -> "\\t",
          "\"" -> "\\\""
        )
        '"' + replaceMap.foldLeft(s) { case (acc, (c, r)) => acc.replace(c, r) } + '"'
      // For an empty Seq just use its normal String representation.
      case xs: Seq[_] if xs.isEmpty => xs.toString()
      case xs: Seq[_]               =>
        // If the Seq is not too long, pretty print on one line.
        val resultOneLine = xs.map(nextDepth).toString()
        if resultOneLine.length <= maxElementWidth then return resultOneLine
        // Otherwise, build it with newlines and proper field indents.
        val result        = xs.map(x => s"\n$fieldIndent${nextDepth(x)}").toString()
        result.substring(0, result.length - 1) + "\n" + indent + ")"
      // Product should cover case classes.
      case p: Product               =>
        val prefix = p.productPrefix
        // We'll use reflection to get the constructor arg names and values.
        val cls    = p.getClass
        val fields = cls.getDeclaredFields.filterNot(_.isSynthetic).map(_.getName)
        val values = p.productIterator.toSeq
        // If we weren't able to match up fields/values, fall back to toString.
        if fields.length != values.length then return p.toString
        fields.zip(values).toList match {
          // If there are no fields, just use the normal String representation.
          case Nil               => p.toString
          // If there is just one field, let's just print it as a wrapper.
          case (_, value) :: Nil => s"$prefix(${thisDepth(value)})"
          // If there is more than one field, build up the field names and values.
          case kvps              =>
            val prettyFields  = kvps.map { case (k, v) => s"$fieldIndent$k = ${nextDepth(v)}" }
            // If the result is not too long, pretty print on one line.
            val resultOneLine = s"$prefix(${prettyFields.mkString(", ")})"
            if resultOneLine.length <= maxElementWidth then return resultOneLine
            // Otherwise, build it with newlines and proper field indents.
            s"$prefix(\n${prettyFields.mkString(",\n")}\n$indent)"
        }
      // If we haven't specialized this type, just use its toString.
      case _                        => a.toString
    }
  }

}
