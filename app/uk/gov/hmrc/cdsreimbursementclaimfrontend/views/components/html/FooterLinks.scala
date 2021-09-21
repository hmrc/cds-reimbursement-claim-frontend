package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.govukfrontend.views.viewmodels.footer.FooterItem

object FooterLinks {

  def apply()(implicit messages: Messages, viewConfig: ViewConfig): Seq[FooterItem] =
    viewConfig.footerLinkItems.flatMap { item =>
      val keyPrefix = s"footer.$item"
      val textKey   = s"$keyPrefix.text"
      val urlKey    = s"$keyPrefix.url"

      if (messages.isDefinedAt(textKey) && messages.isDefinedAt(urlKey))
        Seq(
          FooterItem(
            text = Some(messages(textKey)),
            href = Some(messages(urlKey))
          )
        )
      else Seq.empty
    }
}
