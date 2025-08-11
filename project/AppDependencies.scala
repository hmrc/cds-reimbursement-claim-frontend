import play.core.PlayVersion.current
import sbt._
import sbt.librarymanagement.InclExclRule

object AppDependencies {

  val jsoupVersion         = "1.21.1"
  val hmrcMongoPlayVersion = "2.7.0"

  val compile = Seq(
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-30" % "10.1.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc-play-30" % "12.8.0",
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-30"         % hmrcMongoPlayVersion,
    "org.typelevel"     %% "cats-core"                  % "2.13.0"
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"               % "3.2.19"             % Test,
    "org.jsoup"               % "jsoup"                   % jsoupVersion         % Test,
    "org.playframework"      %% "play-test"               % current              % Test,
    "org.scalamock"          %% "scalamock"               % "7.3.2"              % Test,
    "org.scalatestplus"      %% "scalacheck-1-18"         % "3.2.19.0"           % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "7.0.2"              % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.8"             % Test,
    "org.scalameta"          %% "munit-diff"              % "1.0.4"              % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30" % hmrcMongoPlayVersion % Test
  )

}
