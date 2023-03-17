import play.core.PlayVersion.current
import sbt._
import sbt.librarymanagement.InclExclRule

object AppDependencies {

  val monocleVersion       = "2.1.0"
  val jsoupVersion         = "1.15.3"
  val hmrcMongoPlayVersion = "0.74.0"

  val compile = Seq(
    "uk.gov.hmrc"           %% "bootstrap-frontend-play-28" % "7.14.0",
    "uk.gov.hmrc"           %% "play-frontend-hmrc"         % "6.4.0-play-28",
    "uk.gov.hmrc.mongo"     %% "hmrc-mongo-play-28"         % hmrcMongoPlayVersion,
    "org.typelevel"         %% "cats-core"                  % "2.8.0",
    "com.github.kxbmap"     %% "configs"                    % "0.5.0",
    "org.julienrf"          %% "play-json-derived-codecs"   % "7.0.0",
    "com.github.arturopala" %% "validator"                  % "0.20.0"
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"               % "3.2.15"             % Test,
    "org.jsoup"               % "jsoup"                   % jsoupVersion         % Test,
    "com.typesafe.play"      %% "play-test"               % current              % Test,
    "org.scalamock"          %% "scalamock"               % "5.1.0"              % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"            % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "5.1.0"              % Test,
    "com.github.chocpanda"   %% "scalacheck-magnolia"     % "0.5.1"              % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.0"             % Test,
    "org.pegdown"             % "pegdown"                 % "1.6.0"              % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28" % hmrcMongoPlayVersion % Test
  )

}
