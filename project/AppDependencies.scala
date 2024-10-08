import play.core.PlayVersion.current
import sbt._
import sbt.librarymanagement.InclExclRule

object AppDependencies {

  val monocleVersion       = "2.1.0"
  val jsoupVersion         = "1.17.2"
  val hmrcMongoPlayVersion = "2.2.0"

  val compile = Seq(
    "uk.gov.hmrc"           %% "bootstrap-frontend-play-30" % "9.4.0",
    "uk.gov.hmrc"           %% "play-frontend-hmrc-play-30" % "10.11.0",
    "uk.gov.hmrc.mongo"     %% "hmrc-mongo-play-30"         % hmrcMongoPlayVersion,
    "org.typelevel"         %% "cats-core"                  % "2.10.0",
    "com.github.kxbmap"     %% "configs"                    % "0.6.1",
    "com.github.arturopala" %% "validator"                  % "0.23.0",
    "io.github.arturopala"  %% "play3-scala-pdf"            % "4.3.1" excludeAll ("org.scala-lang.modules" %% "scala-xml", "org.bouncycastle"),
    "org.bouncycastle"       % "bcprov-jdk18on"             % "1.78.1", //Overrides the old version brought in transitively above
    "org.bouncycastle"       % "bcpkix-jdk18on"             % "1.78.1" //Overrides the old version brought in transitively above)
  )

  val test = Seq(
    "org.scalatest"          %% "scalatest"               % "3.2.18"             % Test,
    "org.jsoup"               % "jsoup"                   % jsoupVersion         % Test,
    "org.playframework"      %% "play-test"               % current              % Test,
    "org.scalamock"          %% "scalamock"               % "5.2.0"              % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"            % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "7.0.1"              % Test,
    "com.github.chocpanda"   %% "scalacheck-magnolia"     % "0.5.1"              % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.8"             % Test,
    "org.pegdown"             % "pegdown"                 % "1.6.0"              % Test,
    "org.scalameta"          %% "munit-diff"              % "1.0.0"              % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30" % hmrcMongoPlayVersion % Test
  )

}
