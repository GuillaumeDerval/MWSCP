name := "MaximumSubMatrices"

version := "0.1"

scalaVersion := "2.12.6"

javaOptions in run += "-Xmx4G"
scalacOptions += "-deprecation"
resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-CUSTOM" withSources()

mainClass in assembly := Some("apps.Solve")

// https://mvnrepository.com/artifact/junit/junit
libraryDependencies += "junit" % "junit" % "4.12" % Test

//libraryDependencies += "org.gnu.glpk" % "glpk-java" % "1.8.0"
//libraryDependencies += "de.xypron.linopt" % "linopt" % "1.17"
//resolvers += "XypronRelease" at "http://rsync.xypron.de/repository"


//log

libraryDependencies ~= { _ map (m => {
  if(m.organization == "oscar") {
    m.exclude("xerces", "xmlParserAPIs").exclude("jaxen", "jaxen")
  }
  else
    m
})}