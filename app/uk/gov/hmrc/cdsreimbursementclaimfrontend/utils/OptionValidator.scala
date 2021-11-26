package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

/** Methods to validate existence of [[Option]]s. */
@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Equals"))
object OptionsValidator {

  def all(options: Option[Any]*): Boolean =
    options.forall(_.isDefined)

  def any(options: Option[Any]*): Boolean =
    options.exists(_.isDefined)

  def none(options: Option[Any]*): Boolean =
    options.forall(_.isEmpty)

  def allOrNone(options: Option[Any]*): Boolean =
    options.forall(_.isDefined) || options.forall(_.isEmpty)

  def onlyOne(options: Option[Any]*): Boolean =
    options.count(_.isDefined) == 1

  def requiredWhen(cond: Boolean)(option: Option[Any]): Boolean =
    !cond || option.isDefined

  def nonEmptyMap(option: Option[Map[_, _]]*): Boolean =
    option.exists(_.nonEmpty)

  implicit def asOptionAny(bool: Boolean): Option[Any] =
    if (bool) Some(()) else None

}
