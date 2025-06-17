import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "cds-reimbursement-claim-frontend"

Global / semanticdbEnabled := true

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedFiles := (Compile / managedSourceDirectories).value
      .map(d => s"${d.getPath}/.*")
      .mkString(";"),
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*(config|testonly|views|utils).*",
    ScoverageKeys.coverageMinimumStmtTotal := 100,
    ScoverageKeys.coverageMinimumBranchTotal := 100,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true
  )

lazy val microservice = Project(appName, file("."))
  .enablePlugins(
    play.sbt.PlayScala,
    SbtDistributablesPlugin
  )
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(scalafmtOnCompile := true)
  .settings(scalaVersion := "3.4.3")
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(
    routesImport := Seq(
      "_root_.controllers.Assets.Asset",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PathBinders._",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN",
      "java.util.UUID"
    )
  )
  .settings(majorVersion := 1)
  .settings(
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .settings(
    dependencyOverrides += "org.jsoup" % "jsoup" % AppDependencies.jsoupVersion
  )
  .settings(Test / resourceDirectories += baseDirectory.value / "conf" / "resources")
  .settings(Assets / pipelineStages := Seq(uglify))
  // .settings(uglifyCompressOptions := Seq("unused=false", "dead_code=false"))
  .settings(uglifyOps := UglifyOps.singleFile)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7500)
  .settings(
    scalacOptions += "-Wunused:all",
    scalacOptions += "-Xmax-inlines:128",
    scalacOptions += "-experimental"
  )
  .settings(Compile / doc / sources := Seq.empty)
  .settings(scalacOptions --= Seq("-Xfatal-warnings"))
  .settings(
    Test / scalacOptions --= Seq("-Ywarn-dead-code", "-Ywarn-value-discard", "-Wvalue-discard")
  )
  .settings(Test / envVars := Map("SCALACTIC_FILL_FILE_PATHNAMES" -> "yes"))
  .settings(Test / fork := false)

lazy val welshTranslation = taskKey[Unit]("Generate Welsh Translations'")

welshTranslation := WelshTranslation.importAndExport()
