
organization := "com.igeolise"

name := "chrome-headless-crawler"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "commons-net"                 %   "commons-net"       % "3.3",
  "io.webfolder"                %   "cdp4j"             % "2.1.5",
  "org.scalaz"                  %% "scalaz-core"        % "7.2.20",
  "org.specs2"                  %%  "specs2-core"       % "4.0.3" % "test",
  "org.specs2"                  %%  "specs2-mock"       % "4.0.3" % "test",
  "org.slf4j"                   %   "slf4j-simple"      % "1.7.21",
  "com.github.julien-truffaut"  %%  "monocle-core"      % "1.5.0",
  "com.github.julien-truffaut"  %%  "monocle-macro"     % "1.5.0"
)

scalacOptions := Seq("-unchecked", "-deprecation")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)