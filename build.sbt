
organization := "com.igeolise"

name := "chrome-headless-crawler"

version := "0.1.0"

scalaVersion := "2.11.8"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "commons-net"               %   "commons-net"       % "3.3",
  "com.igeolise.utilities"  %%  "shell"             % "1.0.0",
  "io.webfolder"            %   "cdp4j"             % "2.1.5",
  "org.specs2"              %%  "specs2-core"       % "3.0" % "test",
  "org.specs2"              %%  "specs2-mock"       % "3.0" % "test",
  "org.slf4j"               %   "slf4j-simple"      % "1.7.21"
)
