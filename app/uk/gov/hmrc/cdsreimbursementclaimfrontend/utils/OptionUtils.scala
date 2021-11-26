package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object OptionUtils {

  def all(options: Option[Any]*): Option[Unit] =
    if (options.forall(_.isDefined)) Some(())
    else None

  def none(options: Option[Any]*): Option[Unit] =
    if (options.forall(_.isEmpty)) Some(())
    else None

  def allOrNone(options: Option[Any]*): Option[Unit] =
    if (options.forall(_.isDefined) || options.forall(_.isEmpty)) Some(())
    else None

  def requiredWhen(cond: Boolean)(option: Option[Any]): Option[Unit] =
    if (!cond || option.isDefined) Some(()) else None

  def nonEmptyMap(option: Option[Map[_, _]]): Option[Unit] =
    if (option.exists(_.nonEmpty)) Some(()) else None

}
