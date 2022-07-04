name := "IndSciSimWithSchools"

version := "0.1"

scalaVersion := "2.13.6"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case "reference.conf"              => MergeStrategy.concat
  case x                             => MergeStrategy.first
}

mainClass in (Compile, run) := Some("schools.Main")

mainClass in assembly := Some("schools.Main")