import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import wartremover.Wart

val appName = "cds-reimbursement-claim-frontend"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")

resolvers += Resolver.bintrayRepo("hmrc", "releases")

lazy val wartremoverSettings =
  Seq(
    Compile / compile / wartremoverErrors ++= Warts.allBut(
      Wart.DefaultArguments,
      Wart.ImplicitConversion,
      Wart.ImplicitParameter,
      Wart.Nothing,
      Wart.Overloading,
      Wart.ToString
    ),
    WartRemover.autoImport.wartremoverExcluded += baseDirectory.value / "app" / "uk" / "gov" / "hmrc" / "cdsreimbursementclaimfrontend" / "models" / "ui",
    WartRemover.autoImport.wartremoverExcluded += target.value,
    Compile / compile / WartRemover.autoImport.wartremoverExcluded ++=
      (Compile / routes).value ++
        (baseDirectory.value ** "*.sc").get ++
        Seq(sourceManaged.value / "main" / "sbt-buildinfo" / "BuildInfo.scala"),
    Test / compile / wartremoverErrors --= Seq(Wart.Any, Wart.NonUnitStatements, Wart.Null, Wart.PublicInference, Wart.Equals)
  )

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*Reverse.*;.*(config|testonly|views|utils).*;.*(BuildInfo|Routes).*;.*(models).*",
    ScoverageKeys.coverageMinimum := 81.00,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )

lazy val microservice = Project(appName, file("."))
  .enablePlugins(
    play.sbt.PlayScala,
    SbtAutoBuildPlugin,
    SbtGitVersioning,
    SbtDistributablesPlugin
  )
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"))
  .settings(addCompilerPlugin(scalafixSemanticdb))
  .settings(scalaVersion := "2.12.12")
  .settings(routesImport := Seq("_root_.controllers.Assets.Asset"))
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(
    majorVersion := 1,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .settings(
    scalacOptions ++= Seq(
      "-Yrangepos",
      "-language:postfixOps",
      "-Ypartial-unification"
    ),
    Test / scalacOptions --= Seq("-Ywarn-dead-code", "-Ywarn-value-discard"),
    scalacOptions += "-P:silencer:pathFilters=routes"
  )
  .settings( Test / resourceDirectories += baseDirectory.value / "conf" / "resources")
  .settings(publishingSettings: _*)
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(wartremoverSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7500)
  .settings(scalafmtOnCompile := true)

lazy val welshExport = taskKey[Unit]("Generate Welsh Translations'")

welshExport := WelshTranslation.welshExport()