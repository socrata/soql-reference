import sbt._
import Keys._

object SoqlTypes {
  lazy val settings: Seq[Setting[_]] = BuildSettings.projectSettings() ++ Seq(
    crossScalaVersions += "2.8.1",
    resolvers <++= (scalaVersion) { sv =>
      oldRojomaJsonRepo(sv) ++
      Seq("Open Source Geospatial Foundation Repository" at "http://download.osgeo.org/webdav/geotools")
    },
    libraryDependencies <++= (scalaVersion) { sv =>
      Seq(
        "joda-time" % "joda-time" % "2.1",
        "org.joda" % "joda-convert" % "1.2",
        "com.rojoma" %% "rojoma-json" % rojomaJsonVersion(sv),
        "org.bouncycastle" % "bcprov-jdk15on" % "1.48",
        "org.geotools" % "gt-geojson" % "11.0",
        "commons-io" % "commons-io" % "1.4",

        // Only used by serialization
        "com.google.protobuf" % "protobuf-java" % "2.4.1" % "optional",

        "org.scalacheck" %% "scalacheck" % scalaCheckVersion(sv) % "test"
      )
    }
  )

  def oldRojomaJsonRepo(scalaVersion: String) = scalaVersion match {
    case "2.8.1" => Seq("rjmac maven" at "http://rjmac.github.io/maven/releases")
    case _ => Nil
  }

  def rojomaJsonVersion(scalaVersion: String) = scalaVersion match {
    case "2.8.1" => "1.4.4"
    case _ => "[2.0.0,3.0.0)"
  }

  def scalaCheckVersion(scalaVersion: String) = scalaVersion match {
    case "2.8.1" => "1.8"
    case _ => "1.10.0"
  }
}
