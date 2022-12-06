name := "aoc22-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "org.scoverage"    % "sbt-scoverage"       % "1.6.1" )
addSbtPlugin( "com.eed3si9n"     % "sbt-buildinfo"       % "0.9.0" )
addSbtPlugin( "com.typesafe.sbt" % "sbt-native-packager" % "1.7.0" )
addSbtPlugin( "org.scalameta"    % "sbt-scalafmt"        % "2.3.2" )
