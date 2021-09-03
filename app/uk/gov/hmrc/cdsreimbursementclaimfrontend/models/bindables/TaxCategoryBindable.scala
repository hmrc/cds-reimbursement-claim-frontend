package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bindables

import play.api.mvc.PathBindable
import cats.syntax.eq._

sealed abstract class TaxCategoryBindable(val value: String) extends Product with Serializable

object TaxCategoryBindable {

  case object UkDuty extends TaxCategoryBindable("uk-duty")
  case object EuDuty extends TaxCategoryBindable("eu-duty")
  case object Beer extends TaxCategoryBindable("beer")
  case object Wine extends TaxCategoryBindable("wine")
  case object MadeWine extends TaxCategoryBindable("made-wine")
  case object LowAlcoholBeverages extends TaxCategoryBindable("low-alcohol-beverages")
  case object Spirits extends TaxCategoryBindable("spirits")
  case object CiderPerry extends TaxCategoryBindable("cider-perry")
  case object HydrocarbonOils extends TaxCategoryBindable("hydrocarbon-oils")
  case object Biofuels extends TaxCategoryBindable("biofuels")
  case object MiscellaneousRoadFuels extends TaxCategoryBindable("miscellaneous-road-fuels")
  case object Tobacco extends TaxCategoryBindable("tobacco")
  case object ClimateChangeLevy extends TaxCategoryBindable("climate-change-levy")

  val dutyCategoriesList: List[TaxCategoryBindable] = List(
    UkDuty,
    EuDuty,
    Beer,
    Wine,
    MadeWine,
    LowAlcoholBeverages,
    Spirits,
    CiderPerry,
    HydrocarbonOils,
    Biofuels,
    MiscellaneousRoadFuels,
    Tobacco,
    ClimateChangeLevy
  )

  def parse(str: String): Either[String, TaxCategoryBindable] =
    dutyCategoriesList.find(a => a.value === str).toRight("No such category")

  implicit lazy val taxCategoryBindable: PathBindable[TaxCategoryBindable] = new PathBindable[TaxCategoryBindable] {

    override def bind(key: String, value: String): Either[String, TaxCategoryBindable] =
      parse(value)

    override def unbind(key: String, bindable: TaxCategoryBindable): String =
      bindable.value
  }

}
