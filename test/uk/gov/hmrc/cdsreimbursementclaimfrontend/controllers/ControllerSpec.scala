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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.google.inject.Inject
import com.google.inject.Singleton
import com.typesafe.config.ConfigFactory
import org.apache.pekko.stream.Materializer
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalacheck.ShrinkLowPriority
import org.scalactic.source.Position
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.*
import play.api.http.HttpConfiguration
import play.api.i18n.*
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.FeatureSet
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.PageAssertions
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId
import uk.gov.hmrc.mongo.play.PlayMongoModule

import java.net.URLEncoder
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.collection.immutable.SortedMap

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

trait ControllerSpec
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with MockFactory
    with SeqUtils
    with SummaryMatchers
    with PageAssertions {

  implicit val lang: Lang = Lang("en")

  val sessionCacheBinding: GuiceableModule =
    bind[SessionCache].to[TestSessionCache]

  val featuresCacheBinding: GuiceableModule =
    bind[FeaturesCache].to[TestFeaturesCache]

  def overrideBindings: List[GuiceableModule] =
    List(sessionCacheBinding)

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").html()

  private lazy val additionalConfig = Configuration()

  def buildFakeApplication(): Application =
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
      .disable[PlayMongoModule]
      .overrides(featuresCacheBinding :: overrideBindings*)
      .overrides(bind[MessagesApi].toProvider[TestDefaultMessagesApiProvider])
      .build()

  lazy val fakeApplication: Application = buildFakeApplication()
  lazy val theMessagesApi               = fakeApplication.injector.instanceOf[MessagesApi]

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

  lazy implicit val materializer: Materializer = fakeApplication.materializer
  lazy implicit val viewConfig: ViewConfig     = instanceOf[ViewConfig]

  abstract override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }

  final def messageFromMessageKey(messageKey: String, args: Any*): String = {
    val m = theMessagesApi(messageKey, args*)
    if m === messageKey then sys.error(s"Message key `$messageKey` is missing a message")
    m
  }

  /** Resolves key to message, reports an error if message is missing. */
  final def m(messageKey: String, args: Any*): String =
    messageFromMessageKey(messageKey, args*)

  final def checkIsTechnicalErrorPage(
    result: Future[Result]
  )(implicit pos: Position): Any = {
    import cats.instances.int._
    import cats.syntax.eq._
    if status(result) =!= INTERNAL_SERVER_ERROR then println(contentAsString(result))

    (status(result), redirectLocation(result)) shouldBe (INTERNAL_SERVER_ERROR -> None)
    contentAsString(result)                      should include(
      messageFromMessageKey("global.error.InternalServerError500.title")
    )
  }

  final def checkIsRedirect(
    result: Future[Result],
    expectedRedirectUrl: String
  )(implicit pos: Position): Any = {
    status(result)           shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some(expectedRedirectUrl)
  }

  final def checkIsRedirect(
    result: Future[Result],
    expectedRedirectCall: Call
  )(implicit pos: Position): Any =
    checkIsRedirect(result, expectedRedirectCall.url)

  final def checkPageIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    contentChecks: Document => Any = _ => (),
    expectedStatus: Int = OK
  )(implicit pos: Position): Any = {
    (status(result), redirectLocation(result)) shouldBe (expectedStatus -> None)

    val doc = Jsoup.parse(contentAsString(result))

    doc.select("h1").text should include(expectedTitle)

    val bodyText = doc.select("body").text

    val regex = """not_found_message\((.*?)\)""".r

    val regexResult = regex.findAllMatchIn(bodyText).toList
    if regexResult.nonEmpty then fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")
    else contentChecks(doc)
  }

  final def checkPageWithErrorIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    expectedErrorMessage: String
  )(implicit pos: Position): Any = {
    status(result) shouldBe BAD_REQUEST

    val doc = Jsoup.parse(contentAsString(result))

    doc.select("h1").text should include(expectedTitle)

    val bodyText = doc.select("body").text

    val regex = """not_found_message\((.*?)\)""".r

    val regexResult = regex.findAllMatchIn(bodyText).toList
    if regexResult.nonEmpty then fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")
    else {
      doc.select(".govuk-list.govuk-error-summary__list").text() shouldBe expectedErrorMessage
    }
  }

  final def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  extension [A, B, C](m: SortedMap[A, SortedMap[B, Option[C]]]) {
    def clearFirstOption: SortedMap[A, SortedMap[B, Option[C]]] =
      m.updated(m.head._1, m.head._2.updated(m.head._2.head._1, None))
  }

  extension [A, B](m: Map[A, Option[B]]) {
    def clearFirstOption: Map[A, Option[B]] =
      m.updated(m.head._1, None)
  }

}

trait PropertyBasedControllerSpec extends ControllerSpec with ScalaCheckPropertyChecks with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
}

@Singleton
class TestSessionCache extends SessionCache {

  val sessions: scala.collection.concurrent.Map[String, SessionData] =
    (new ConcurrentHashMap[String, SessionData]()).asScala

  override def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[cdsreimbursementclaimfrontend.models.Error, Option[SessionData]]] =
    Future.successful(hc.sessionId match {
      case Some(SessionId(sessionId)) =>
        Right(sessions.get(sessionId))
      case None                       =>
        Left(uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error("no session found"))
    })

  override def store(sessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[cdsreimbursementclaimfrontend.models.Error, Unit]] =
    Future.successful(hc.sessionId match {
      case Some(SessionId(sessionId)) =>
        sessions.put(sessionId, sessionData)
        Right(())
      case None                       =>
        Left(uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error("no session found"))
    })

}
@Singleton
class TestFeaturesCache extends FeaturesCache {

  val sessions: scala.collection.concurrent.Map[String, FeatureSet] =
    (new ConcurrentHashMap[String, FeatureSet]()).asScala

  override def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error, FeatureSet]] =
    Future.successful(hc.sessionId match {
      case Some(SessionId(sessionId)) =>
        Right(
          sessions
            .getOrElse(sessionId, FeatureSet.empty)
        )
      case None                       =>
        Right(FeatureSet.empty)
    })

  override def store(featureSet: FeatureSet)(implicit
    hc: HeaderCarrier
  ): Future[Either[uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error, Unit]] =
    Future.successful(hc.sessionId match {
      case Some(SessionId(sessionId)) =>
        sessions.put(sessionId, featureSet)
        Right(())
      case None                       =>
        Right(())
    })

}
