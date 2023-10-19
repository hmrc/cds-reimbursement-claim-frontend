import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import wartremover.Wart

val appName = "cds-reimbursement-claim-frontend"

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml"        % VersionScheme.Always
ThisBuild / scalafixDependencies += "com.github.liancheng"       %% "organize-imports" % "0.6.0"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")

lazy val wartremoverSettings =
  Seq(
    Compile / compile / wartremoverErrors ++= Warts.allBut(
      Wart.DefaultArguments,
      Wart.ImplicitConversion,
      Wart.ImplicitParameter,
      Wart.Nothing,
      Wart.Overloading,
      Wart.ToString,
      Wart.PublicInference,
      Wart.SizeIs,
      Wart.StringPlusAny,
      Wart.Any,
      Wart.SeqApply,
      Wart.CaseClassPrivateApply
    ),
    WartRemover.autoImport.wartremoverExcluded += target.value,
    Compile / compile / WartRemover.autoImport.wartremoverExcluded ++=
      (Compile / routes).value ++
        (baseDirectory.value ** "*.sc").get ++
        Seq(sourceManaged.value / "main" / "sbt-buildinfo" / "BuildInfo.scala"),
    Test / compile / wartremoverErrors --= Seq(
      Wart.Any,
      Wart.NonUnitStatements,
      Wart.Null,
      Wart.PublicInference,
      Wart.Equals,
      Wart.GlobalExecutionContext,
      Wart.OptionPartial,
      Wart.Throw,
      Wart.IterableOps
    )
  )

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
    SbtAutoBuildPlugin,
    SbtGitVersioning,
    SbtDistributablesPlugin
  )
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(scalafmtOnCompile := true)
  .settings(scalaVersion := "2.13.12")
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(
    routesImport := Seq(
      "_root_.controllers.Assets.Asset",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PathBinders._",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType",
      "uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode",
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
  .settings(uglifyCompressOptions := Seq("unused=false", "dead_code=false"))
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(wartremoverSettings: _*)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7500)
  .settings(
    scalacOptions += s"-Wconf:src=${target.value}/scala-${scalaBinaryVersion.value}/routes/.*:s,src=${target.value}/scala-${scalaBinaryVersion.value}/twirl/.*:s"
  )
  .settings(Compile / doc / sources := Seq.empty)
  .settings(scalacOptions --= Seq("-Xfatal-warnings"))
  .settings(Test / scalacOptions --= Seq("-Ywarn-dead-code", "-Ywarn-value-discard", "-Wvalue-discard"))
  .settings(Test / envVars := Map("SCALACTIC_FILL_FILE_PATHNAMES" -> "yes"))
  .settings(Test / fork := false)

lazy val welshTranslation = taskKey[Unit]("Generate Welsh Translations'")

welshTranslation := WelshTranslation.importAndExport()
