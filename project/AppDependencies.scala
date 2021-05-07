import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val silencerVersion = "1.7.0"

  val compile = Seq(
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.2.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "0.60.0-play-28",
    "uk.gov.hmrc"       %% "mongo-caching"              % "7.0.0-play-28",
    "uk.gov.hmrc"       %% "play-language"              % "4.12.0-play-28",
    "org.typelevel"     %% "cats-core"                  % "2.3.1",
    "com.github.kxbmap" %% "configs"                    % "0.5.0",
    "org.julienrf"      %% "play-json-derived-codecs"   % "7.0.0",
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    "com.github.ghik"    % "silencer-lib"               % silencerVersion % Provided cross CrossVersion.full
  )

  val test = Seq(
    "org.scalatest"              %% "scalatest"                 % "3.2.3"         % Test,
    "org.jsoup"                   % "jsoup"                     % "1.13.1"        % Test,
    "com.typesafe.play"          %% "play-test"                 % current         % Test,
    "org.scalamock"              %% "scalamock"                 % "5.1.0"         % Test,
    "org.scalatestplus"          %% "scalacheck-1-14"           % "3.2.0.0"       % Test,
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "5.1.0"          % Test,
    "uk.gov.hmrc"                %% "reactivemongo-test"        % "5.0.0-play-28" % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"         % Test,
    "com.vladsch.flexmark"        % "flexmark-all"              % "0.36.8"        % "test, it",
    "org.pegdown"                 % "pegdown"                   % "1.6.0"         % "test, it",
    "com.typesafe.akka"          %% "akka-testkit"              % "2.6.10"        % Test
  )

}
