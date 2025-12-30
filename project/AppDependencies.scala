import play.core.PlayVersion.current
import sbt.*

object AppDependencies {

  val jsoupVersion         = "1.21.2"
  val hmrcMongoPlayVersion = "2.11.0"
  val bootstrapVersion     = "10.5.0"
  val playSuffix           = "-play-30"

  val compile = Seq(
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"       %% "play-frontend-hmrc-play-30" % "12.25.0",
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-30"         % hmrcMongoPlayVersion,
    "org.typelevel"     %% "cats-core"                  % "2.13.0"
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"                   % "3.2.19"             % Test,
    "org.jsoup"               % "jsoup"                       % jsoupVersion         % Test,
    "org.playframework"      %% "play-test"                   % current              % Test,
    "org.scalamock"          %% "scalamock"                   % "7.5.0"              % Test,
    "org.scalatestplus"      %% "scalacheck-1-18"             % "3.2.19.0"           % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"          % "7.0.2"              % Test,
    "com.vladsch.flexmark"    % "flexmark-all"                % "0.64.8"             % Test,
    "org.scalameta"          %% "munit-diff"                  % "1.0.4"              % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30"     % hmrcMongoPlayVersion % Test,
    "uk.gov.hmrc"            %% s"bootstrap-test$playSuffix"  % bootstrapVersion     % Test
  )

}
