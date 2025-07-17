resolvers += MavenRepository("HMRC-open-artefacts-maven2", "https://open.artefacts.tax.service.gov.uk/maven2")
resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.typesafeRepo("releases")

libraryDependencies += "org.scalameta" %% "scalameta" % "4.12.3"

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("uk.gov.hmrc"       % "sbt-auto-build"     % "3.24.0")
addSbtPlugin("uk.gov.hmrc"       % "sbt-distributables" % "2.6.0")
addSbtPlugin("org.playframework" % "sbt-plugin"         % "3.0.8")
addSbtPlugin("uk.gov.hmrc"       % "sbt-sass-compiler"  % "0.12.0")
addSbtPlugin("org.scalameta"     % "sbt-scalafmt"       % "2.5.4")
addSbtPlugin("org.scoverage"     % "sbt-scoverage"      % "2.3.1")
addSbtPlugin("ch.epfl.scala"     % "sbt-scalafix"       % "0.13.0")
addSbtPlugin("com.github.sbt"    % "sbt-uglify"         % "3.0.1")
