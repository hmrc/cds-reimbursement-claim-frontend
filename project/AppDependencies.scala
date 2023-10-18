import play.core.PlayVersion.current
import sbt._
import sbt.librarymanagement.InclExclRule

object AppDependencies {

  val monocleVersion       = "2.1.0"
  val jsoupVersion         = "1.16.1"
  val hmrcMongoPlayVersion = "1.3.0"

  val compile = Seq(
    "uk.gov.hmrc"           %% "bootstrap-frontend-play-28" % "7.22.0",
    "uk.gov.hmrc"           %% "play-frontend-hmrc"         % "7.23.0-play-28",
    "uk.gov.hmrc.mongo"     %% "hmrc-mongo-play-28"         % hmrcMongoPlayVersion,
    "org.typelevel"         %% "cats-core"                  % "2.10.0",
    "com.github.kxbmap"     %% "configs"                    % "0.6.1",
    "org.julienrf"          %% "play-json-derived-codecs"   % "10.1.0",
    "com.github.arturopala" %% "validator"                  % "0.22.0"
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"               % "3.2.17"             % Test,
    "org.jsoup"               % "jsoup"                   % jsoupVersion         % Test,
    "com.typesafe.play"      %% "play-test"               % current              % Test,
    "org.scalamock"          %% "scalamock"               % "5.2.0"              % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"            % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "5.1.0"              % Test,
    "com.github.chocpanda"   %% "scalacheck-magnolia"     % "0.5.1"              % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.8"             % Test,
    "org.pegdown"             % "pegdown"                 % "1.6.0"              % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28" % hmrcMongoPlayVersion % Test
  )

}
