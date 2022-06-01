import play.core.PlayVersion.current
import sbt._
import sbt.librarymanagement.InclExclRule

object AppDependencies {

  val monocleVersion = "2.1.0"
  val jsoupVersion   = "1.13.1"

  val compile = Seq(
    "uk.gov.hmrc"           %% "bootstrap-frontend-play-28" % "5.3.0",
    "uk.gov.hmrc"           %% "play-frontend-hmrc"         % "3.20.0-play-28",
    "uk.gov.hmrc"           %% "mongo-caching"              % "7.0.0-play-28",
    "uk.gov.hmrc"           %% "play-language"              % "4.13.0-play-28",
    "org.typelevel"         %% "cats-core"                  % "2.7.0",
    "com.github.kxbmap"     %% "configs"                    % "0.5.0",
    "org.julienrf"          %% "play-json-derived-codecs"   % "7.0.0",
    "com.github.arturopala" %% "validator"                  % "0.9.0"
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"           % "3.2.5"         % Test,
    "org.jsoup"               % "jsoup"               % jsoupVersion    % Test,
    "com.typesafe.play"      %% "play-test"           % current         % Test,
    "org.scalamock"          %% "scalamock"           % "5.1.0"         % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"     % "3.2.0.0"       % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"  % "5.1.0"         % Test,
    "uk.gov.hmrc"            %% "reactivemongo-test"  % "5.0.0-play-28" % Test,
    "com.github.chocpanda"   %% "scalacheck-magnolia" % "0.5.1"         % Test,
    "com.vladsch.flexmark"    % "flexmark-all"        % "0.36.8"        % "test, it",
    "org.pegdown"             % "pegdown"             % "1.6.0"         % "test, it",
    "com.typesafe.akka"      %% "akka-testkit"        % "2.6.14"        % Test
  )

}
