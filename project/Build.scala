import sbt._
import Keys._

object ToolXiTBuild extends Build {

  lazy val `toolxit-tex` = (Project(id = "toolxit-tex",
    base = file(".")) settings (
    organization := "org.gnieh",
    name := "toolxit-tex",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.1",
    libraryDependencies ++= globalDependencies,
    parallelExecution := false,
    compileOptions)
    settings(publishSettings: _*)
  )

  lazy val globalDependencies = Seq(
  )

  lazy val compileOptions = scalacOptions ++= Seq("-deprecation", "-feature")

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    // The Nexus repo we're publishing to.
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/gnieh/toolxit-tex</url>
      <licenses>
        <license>
          <name>The Apache Software License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>https://github.com/gnieh/toolxit-tex</url>
        <connection>scm:git:git://github.com/gnieh/toolxit-tex.git</connection>
        <developerConnection>scm:git:git@github.com:gnieh/toolxit-tex.git</developerConnection>
        <tag>HEAD</tag>
      </scm>
      <developers>
        <developer>
          <id>satabin</id>
          <name>Lucas Satabin</name>
          <email>lucas.satabin@gnieh.org</email>
        </developer>
      </developers>
      <ciManagement>
        <system>travis</system>
        <url>https://travis-ci.org/#!/gnieh/toolxit-tex</url>
      </ciManagement>
      <issueManagement>
        <system>github</system>
        <url>https://github.com/gnieh/toolxit-tex/issues</url>
      </issueManagement>
    )
  )

}
