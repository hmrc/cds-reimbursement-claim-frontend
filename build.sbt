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
    wartremoverErrors in (Compile, compile) ++= Warts.allBut(
      Wart.DefaultArguments,
      Wart.ImplicitConversion,
      Wart.ImplicitParameter,
      Wart.Nothing,
      Wart.Overloading,
      Wart.ToString
    ),
    WartRemover.autoImport.wartremoverExcluded += baseDirectory.value / "app" / "uk" / "gov" / "hmrc" / "cdsreimbursementclaimfrontend" / "models" / "ui",
    WartRemover.autoImport.wartremoverExcluded += target.value,
    WartRemover.autoImport.wartremoverExcluded in (Compile, compile) ++=
      routes.in(Compile).value ++
        (baseDirectory.value ** "*.sc").get ++
        Seq(sourceManaged.value / "main" / "sbt-buildinfo" / "BuildInfo.scala"),
    wartremoverErrors in (Test, compile) --= Seq(Wart.Any, Wart.NonUnitStatements, Wart.Null, Wart.PublicInference, Wart.Equals)
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
    scalacOptions in Test --= Seq("-Ywarn-dead-code", "-Ywarn-value-discard"),
    scalacOptions += "-P:silencer:pathFilters=routes"
  )
  .settings(resourceDirectories in Test += baseDirectory.value / "conf" / "resources")
  .settings(publishingSettings: _*)
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(wartremoverSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7500)
  .settings(scalafmtOnCompile := true)