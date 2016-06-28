name := "scalacv"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	"org.apache.spark" %% "spark-core" % "1.6.1"
)

resolvers ++= Seq(
	"Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

classpathTypes += "maven-plugin"

libraryDependencies += "org.bytedeco" % "javacpp" % "1.2.1"
libraryDependencies += "org.bytedeco.javacpp-presets" % "opencv" % "3.1.0-1.2"
libraryDependencies += "org.bytedeco.javacpp-presets" % "opencv" % "3.1.0-1.2" classifier "macosx-x86_64"
libraryDependencies += "org.bytedeco.javacpp-presets" % "ffmpeg" % "3.0.2-1.2"
libraryDependencies += "org.bytedeco.javacpp-presets" % "ffmpeg" % "3.0.2-1.2" classifier "macosx-x86_64"
libraryDependencies += "org.bytedeco" % "javacv" % "1.2"
