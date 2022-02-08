/*
 * Copyright 2022 HM Revenue & Customs
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

import akka.stream.Materializer
import com.google.inject.Inject
import com.google.inject.Singleton
import com.typesafe.config.ConfigFactory
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api._
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.metrics.Metrics
import uk.gov.hmrc.cdsreimbursementclaimfrontend.metrics.MockMetrics

import scala.collection.JavaConverters._
import java.net.URLEncoder
import scala.concurrent.Future
import scala.reflect.ClassTag
import org.scalactic.source.Position
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

@Singleton
class TestMessagesApi(
  messages: Map[String, Map[String, String]],
  langs: Langs,
  langCookieName: String,
  langCookieSecure: Boolean,
  langCookieHttpOnly: Boolean,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApi(
      messages,
      langs,
      langCookieName,
      langCookieSecure,
      langCookieHttpOnly,
      None,
      httpConfiguration
    ) {

  val logger: Logger = Logger(this.getClass)

  override protected def noMatch(key: String, args: Seq[Any])(implicit lang: Lang): String = {
    logger.error(s"Could not find message for key: $key ${args.mkString("-")}")
    s"""not_found_message("$key")"""
  }

}

@Singleton
class TestDefaultMessagesApiProvider @Inject() (
  environment: Environment,
  config: Configuration,
  langs: Langs,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApiProvider(environment, config, langs, httpConfiguration) {

  override lazy val get: MessagesApi =
    new TestMessagesApi(
      loadAllMessages,
      langs,
      langCookieName = langCookieName,
      langCookieSecure = langCookieSecure,
      langCookieHttpOnly = langCookieHttpOnly,
      httpConfiguration = httpConfiguration
    )
}

trait ControllerSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll with MockFactory {

  implicit val lang: Lang = Lang("en")

  def overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").text()

  private lazy val additionalConfig = Configuration()

  def buildFakeApplication(): Application = {
    val metricsBinding: GuiceableModule =
      bind[Metrics].toInstance(MockMetrics.metrics)

    new GuiceApplicationBuilder()
      .configure(
        Configuration(
          ConfigFactory.parseString(
            """
              | metrics.jvm = false
              | metrics.enabled = false
              | metrics.logback = false
              | auditing.enabled = false
              | microservice.upscan-initiate.upscan-store.expiry-time = 1
          """.stripMargin
          )
        ).withFallback(additionalConfig)
      )
      .disable[uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache]
      .overrides(metricsBinding :: overrideBindings: _*)
      .overrides(bind[MessagesApi].toProvider[TestDefaultMessagesApiProvider])
      .build()
  }

  lazy val fakeApplication: Application = buildFakeApplication()
  lazy val theMessagesApi               = fakeApplication.injector.instanceOf[MessagesApi]

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

  lazy implicit val materializer: Materializer = fakeApplication.materializer
  lazy implicit val viewConfig                 = instanceOf[ViewConfig]

  abstract override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }

  def messageFromMessageKey(messageKey: String, args: Any*): String = {
    val m = theMessagesApi(messageKey, args: _*)
    if (m === messageKey) sys.error(s"Message key `$messageKey` is missing a message")
    m
  }

  def checkIsTechnicalErrorPage(
    result: Future[Result]
  )(implicit pos: Position): Any = {
    import cats.instances.int._
    import cats.syntax.eq._
    if (status(result) =!= INTERNAL_SERVER_ERROR) println(contentAsString(result))

    (status(result), redirectLocation(result)) shouldBe (INTERNAL_SERVER_ERROR -> None)
    contentAsString(result)                      should include(
      messageFromMessageKey("global.error.InternalServerError500.title")
    )
  }

  def checkIsRedirect(
    result: Future[Result],
    expectedRedirectUrl: String
  )(implicit pos: Position): Any = {
    import cats.instances.int._
    import cats.syntax.eq._
    if (status(result) =!= SEE_OTHER)
      status(result)         shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some(expectedRedirectUrl)
  }

  def checkIsRedirect(
    result: Future[Result],
    expectedRedirectCall: Call
  )(implicit pos: Position): Any =
    checkIsRedirect(result, expectedRedirectCall.url)

  def checkPageIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    contentChecks: Document => Any = _ => (),
    expectedStatus: Int = OK
  )(implicit pos: Position): Any = {
    (status(result), redirectLocation(result)) shouldBe (expectedStatus -> None)
    status(result)                             shouldBe expectedStatus

    val doc = Jsoup.parse(contentAsString(result))

    doc.select("h1").text should include(expectedTitle)

    val bodyText = doc.select("body").text

    val regex = """not_found_message\((.*?)\)""".r

    val regexResult = regex.findAllMatchIn(bodyText).toList
    if (regexResult.nonEmpty) fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")
    else contentChecks(doc)
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  def toTypeOfClaim(journeyBindable: JourneyBindable): TypeOfClaimAnswer = journeyBindable match {
    case JourneyBindable.Single    => TypeOfClaimAnswer.Individual
    case JourneyBindable.Multiple  => TypeOfClaimAnswer.Multiple
    case JourneyBindable.Scheduled => TypeOfClaimAnswer.Scheduled
  }

  def isCheckboxChecked(document: Document, fieldValue: String): Boolean =
    document.select(s"""input[value="$fieldValue"] """).hasAttr("checked")

  import cats.instances.int._
  import cats.syntax.eq._

  def selectedRadioValue(doc: Document): Option[String] = {
    val radioItems = doc.select("div.govuk-radios input[checked]")
    if (radioItems.size() =!= 0) Some(radioItems.`val`())
    else None
  }

  def selectedTextArea(doc: Document): Option[String] = {
    val textArea = doc.select("textarea.govuk-textarea")
    if (textArea.size() =!= 0) Some(textArea.`val`()) else None
  }

  def selectedCheckBox(doc: Document): Seq[String] = {
    val checkBoxes: Elements = doc.select("div.govuk-checkboxes input[checked]")
    checkBoxes.eachAttr("value").asScala.toSeq
  }

  def selectedInputBox(doc: Document, inputName: String): Option[String] = {
    val inputString: String = s"input.govuk-input[name='$inputName']"
    val input               = doc.select(inputString)
    if (input.size() =!= 0) Some(input.`val`()) else None
  }

  def selectedInput(doc: Document): Option[String] = {
    val input = doc.select(s"input.govuk-input[checked]")
    if (input.size() =!= 0) Some(input.`val`()) else None
  }

  def summaryKeyValueMap(doc: Document): Map[String, String] = {
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    summaryKeys.asScala.zip(summaryValues.asScala).toMap
  }
}

trait PropertyBasedControllerSpec extends ControllerSpec with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
}
