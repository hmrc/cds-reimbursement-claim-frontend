resolvers += MavenRepository("HMRC-open-artefacts-maven2", "https://open.artefacts.tax.service.gov.uk/maven2")
resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.typesafeRepo("releases")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("uk.gov.hmrc"               % "sbt-auto-build"     % "3.22.0")
addSbtPlugin("uk.gov.hmrc"               % "sbt-distributables" % "2.5.0")
addSbtPlugin("org.playframework"         % "sbt-plugin"         % "3.0.5")
addSbtPlugin("io.github.irundaia"        % "sbt-sassify"        % "1.5.2")
addSbtPlugin("org.wartremover"           % "sbt-wartremover"    % "3.2.4")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"       % "2.5.2")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"       % "0.13.0")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"      % "2.2.2")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"       % "0.1.22")
addSbtPlugin("com.typesafe.sbt"          % "sbt-uglify"         % "2.0.0")
