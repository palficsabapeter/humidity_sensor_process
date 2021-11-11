name := "sensor_process"

version := "0.1"

scalaVersion := "2.13.7"

idePackagePrefix := Some("csaba.palfi")

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files"  % "3.9.1",
  "org.scalatest"        %% "scalatest"     % "3.2.10"  % Test,
  "org.mockito"          %% "mockito-scala" % "1.16.46" % Test,
)

addCommandAlias("check", "scalafmtCheck scalafmtSbtCheck test")
