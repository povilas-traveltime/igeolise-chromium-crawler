
organization := "com.igeolise"

name := "chrome-headless-crawler"

version := "0.1.7"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.12.4")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "commons-net"                 %   "commons-net"       % "3.3",
  "commons-io"                  %   "commons-io"        % "2.5",
  "org.scalaz"                  %%  "scalaz-core"       % "7.2.20",
  "org.specs2"                  %%  "specs2-core"       % "4.0.3" % "test",
  "org.specs2"                  %%  "specs2-mock"       % "4.0.3" % "test",
  "com.github.julien-truffaut"  %%  "monocle-core"      % "1.5.0",
  "com.github.julien-truffaut"  %%  "monocle-macro"     % "1.5.0",
  "org.scala-lang"              %   "scala-xml"         % "2.11.0-M4",
  "com.typesafe.play"           %% "play-json"          % "2.6.9",
  "org.seleniumhq.selenium"     %   "selenium-chrome-driver" % "3.11.0"
)

scalacOptions := Seq("-unchecked", "-deprecation")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)