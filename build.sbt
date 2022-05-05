
organization := "com.igeolise"

name := "chrome-headless-crawler"

version := "0.2.3.10-SNAPSHOT"

scalaVersion := "2.12.12"

crossScalaVersions := Seq("2.11.12", "2.12.7")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq (
  "commons-net"                 %   "commons-net"       % "3.3",
  "commons-io"                  %   "commons-io"        % "2.5",
  "org.specs2"                  %%  "specs2-core"       % "4.10.5" % "test",
  "org.specs2"                  %%  "specs2-mock"       % "4.10.5" % "test",
  "org.scala-lang"              %   "scala-xml"         % "2.11.0-M4",
  "com.typesafe.play"           %%  "play-json"         % "2.9.1",
  "org.seleniumhq.selenium"     %   "selenium-chrome-driver" % "3.141.59",
  "org.apache.httpcomponents"   %   "httpclient"        % "4.5.13",
  "com.softwaremill.quicklens"  %%  "quicklens"         % "1.6.1",
  "org.typelevel"               %%  "cats-core"         % "2.2.0"
)

scalacOptions := Seq("-unchecked", "-deprecation", "-Ypartial-unification")
