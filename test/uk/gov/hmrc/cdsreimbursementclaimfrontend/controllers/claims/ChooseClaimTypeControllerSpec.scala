package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import scala.concurrent.Future
import collection.JavaConverters._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class ChooseClaimTypeControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseClaimTypeController = instanceOf[ChooseClaimTypeController]
  val featureSwitch: FeatureSwitchService   = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val formKey = "choose-claim-type"

  "ChooseClaimTypeController" must {

    featureSwitch.CAndE1179.enable()

    def performAction(): Future[Result] = controller.show()(FakeRequest())

    "display the page" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val buttons      = radioButtons(doc)
          val c285Button   = extractButton(buttons, "C285")
          val ce1179Button = extractButton(buttons, "CE1179")
          extractLabel(c285Button)   shouldBe messageFromMessageKey(s"$formKey.c285.title")
          extractHint(c285Button)    shouldBe ""
          extractLabel(ce1179Button) shouldBe messageFromMessageKey(s"$formKey.ce1179.title")
          extractHint(ce1179Button)  shouldBe messageFromMessageKey(s"$formKey.ce1179.hint")
        }
      )
    }
  }

  private def radioButtons(doc: Document): Elements =
    doc.select(s"div.govuk-radios div.govuk-radios__item")

  private def extractButton(buttons: Elements, requiredValue: String): Element =
    buttons.asScala.filterNot(button => button.select(s"""input[value="$requiredValue"]""").isEmpty).head

  private def extractLabel(button: Element): String =
    button.select("label").text()

  private def extractHint(button: Element): String =
    button.select("div.govuk-hint").text()
}
