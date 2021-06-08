package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.mvc.{Call, Result, Results}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import cats.syntax.all._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType.Individual

import scala.concurrent.Future

//sealed abstract class Ref(value : String) extends Product with Serializable
//
//final case class MRN(value : String) extends Ref(value)
//final case class ERN(value : String) extends Ref(value)
//
//
//sealed trait Journey
//
//case object Single extends Journey
//case object Bulk extends Journey
//case object Scheduled extends Journey
//case object CMA extends Journey

trait ReimbursementRoutes extends Product with Serializable {
  //All routes are listed here which are common, or maybe all the routes with defaults
  def nextPageForselectBasisForClaim:Call = routes.CheckYourAnswersAndSubmitController.checkAllAnswers
  def nextPageForselectDuties:Call = routes.EnterClaimController.startClaim()
}

case object DefaultRoutes extends ReimbursementRoutes

case object MRNSingleRoutes extends ReimbursementRoutes{ //Any overrides for the specific journeys
  override def nextPageForselectBasisForClaim:Call = routes.EnterCommoditiesDetailsController.enterCommoditiesDetails
}
case object ERNSingleRoutes extends ReimbursementRoutes {
  override def nextPageForselectBasisForClaim:Call = routes.EnterCommoditiesDetailsController.changeCommoditiesDetails()
}
case object MRNBulkRoutes extends ReimbursementRoutes
case object ERNBulkRoutes extends ReimbursementRoutes
case object MRNScheduledRoutes extends ReimbursementRoutes
case object ERNScheduledRoutes extends ReimbursementRoutes
case object MRNCmaRoutes extends ReimbursementRoutes
case object ERNCmaRoutes extends ReimbursementRoutes



trait SessionAndJourneyExtractor extends Results {
  def withAnswersAndRoutes[T](
                      f: (FillingOutClaim, Option[T], ReimbursementRoutes) => Future[Result]
                    )(implicit extractor: DraftC285Claim => Option[T], request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
        val draftC285Claim = draftClaim.fold(identity)
        val numOfClaims = draftC285Claim.selectNumberOfClaimsAnswer.flatMap(_.fold(_.selectNumberOfClaimsChoice,a => Option(a.selectNumberOfClaimsChoice)))
        val refType = draftC285Claim.movementReferenceNumber.map(_.value)
        val router = (refType, numOfClaims).mapN{
          case (Right(_),SelectNumberOfClaimsType.Individual) => MRNSingleRoutes
          case (Left(_), SelectNumberOfClaimsType.Individual ) => ERNSingleRoutes
          case (Right(_),SelectNumberOfClaimsType.Bulk) => MRNBulkRoutes
          case (Left(_), SelectNumberOfClaimsType.Bulk ) => ERNBulkRoutes
          case (Right(_),SelectNumberOfClaimsType.Scheduled) => MRNScheduledRoutes
          case (Left(_), SelectNumberOfClaimsType.Scheduled ) => ERNScheduledRoutes
          case (Right(_),SelectNumberOfClaimsType.Individual) => MRNCmaRoutes
          case (Left(_), SelectNumberOfClaimsType.Individual ) => ERNCmaRoutes
        }.getOrElse(DefaultRoutes)

        draftClaim.fold(extractor(_)).fold[Future[Result]](f(fillingOutClaim, None, router))(data => f(fillingOutClaim, Option(data), router))
      case _                                                                     =>
        Future.successful(Redirect(baseRoutes.StartController.start()))
    }

}
