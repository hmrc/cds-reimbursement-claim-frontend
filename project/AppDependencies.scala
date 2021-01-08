import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val silencerVersion = "1.7.0"

  val compile = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-27" % "3.0.0",
    "uk.gov.hmrc"                %% "play-frontend-hmrc"         % "0.27.0-play-27",
    "uk.gov.hmrc"                %% "play-frontend-govuk"        % "0.54.0-play-27",
    "uk.gov.hmrc"                %% "mongo-caching"              % "6.16.0-play-27",
    "uk.gov.hmrc"                %% "domain"                     % "5.10.0-play-27",
    "uk.gov.hmrc"                %% "play-language"              % "4.5.0-play-27",
    "org.typelevel"              %% "cats-core"                  % "2.3.1",
    "com.github.kxbmap"          %% "configs"                    % "0.5.0",
    "org.julienrf"               %% "play-json-derived-codecs"   % "7.0.0",
    "com.github.julien-truffaut" %% "monocle-core"               % "2.1.0",
    "com.github.julien-truffaut" %% "monocle-macro"              % "2.1.0",
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    "com.github.ghik"             % "silencer-lib"               % silencerVersion % Provided cross CrossVersion.full
  )

  val test = Seq(
    "uk.gov.hmrc"                %% "bootstrap-test-play-27"    % "3.0.0"          % Test,
    "org.scalatest"              %% "scalatest"                 % "3.2.3"          % Test,
    "org.jsoup"                   % "jsoup"                     % "1.13.1"         % Test,
    "com.typesafe.play"          %% "play-test"                 % current          % Test,
    "org.scalamock"              %% "scalamock"                 % "4.4.0"          % Test,
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "4.0.3"          % "test, it",
    "uk.gov.hmrc"                %% "reactivemongo-test"        % "4.22.0-play-27" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"          % "test",
    "com.vladsch.flexmark"        % "flexmark-all"              % "0.36.8"         % "test, it",
    "org.pegdown"                 % "pegdown"                   % "1.6.0"          % "test, it"
  )
}
