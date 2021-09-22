import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "cds-reimbursement-claim-frontend"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")

resolvers += Resolver.bintrayRepo("hmrc", "releases")

//lazy val wartremoverSettings =
//  Seq(
//    Compile / compile / wartremoverErrors ++= Warts.allBut(
//      Wart.DefaultArguments,
//      Wart.ImplicitConversion,
//      Wart.ImplicitParameter,
//      Wart.Nothing,
//      Wart.Overloading,
//      Wart.ToString
//    ),
//    WartRemover.autoImport.wartremoverExcluded += target.value,
//    Compile / compile / WartRemover.autoImport.wartremoverExcluded ++=
//      (Compile / routes).value ++
//        (baseDirectory.value ** "*.sc").get ++
//        Seq(sourceManaged.value / "main" / "sbt-buildinfo" / "BuildInfo.scala"),
//    Test / compile / wartremoverErrors --= Seq(
//      Wart.Any,
//      Wart.NonUnitStatements,
//      Wart.Null,
//      Wart.PublicInference,
//      Wart.Equals,
//      Wart.GlobalExecutionContext
//    )
//  )

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedFiles := (Compile / managedSourceDirectories).value.map(d => s"${d.getPath}/.*").mkString(";"),
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*(config|testonly|views|utils|models).*",
    ScoverageKeys.coverageMinimumStmtTotal := 90,
    ScoverageKeys.coverageMinimumBranchTotal := 90,
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
  .settings(scalafmtOnCompile := true)
  .settings(scalaVersion := "2.12.14")
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(
    routesImport := Seq(
      "_root_.controllers.Assets.Asset",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode"
    )
  )
  .settings(majorVersion := 1)
  .settings(
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .settings(
    Test / scalacOptions --= Seq("-Ywarn-dead-code", "-Ywarn-value-discard")
  )
  .settings(Test / resourceDirectories += baseDirectory.value / "conf" / "resources")
  .settings(publishingSettings: _*)
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(resolvers += Resolver.jcenterRepo)
//  .settings(wartremoverSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7500)
  .settings(scalacOptions += s"-Wconf:src=${target.value}/scala-${scalaBinaryVersion.value}/routes/.*:s")
  .settings(Compile / doc / sources := Seq.empty)

lazy val welshTranslation = taskKey[Unit]("Generate Welsh Translations'")

welshTranslation := WelshTranslation.importAndExport()
