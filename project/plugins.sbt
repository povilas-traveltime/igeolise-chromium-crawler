addSbtPlugin("com.igeolise.sbt" % "artifactory-plugin" % "1.2.2")

resolvers := Seq(
  "iGeolise releases" at "https://artifactory.igeolise.com/artifactory/libs-release",
  Resolver.url("iGeolise ivy releases", url("https://artifactory.igeolise.com/artifactory/libs-release-ivy"))(Resolver.ivyStylePatterns)
)

externalResolvers := Resolver.defaultLocal +: resolvers.value

fullResolvers := projectResolver.value +: externalResolvers.value

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")