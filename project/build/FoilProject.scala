import sbt._

class FoilProject(info : ProjectInfo)  extends DefaultProject(info) {
  val joda16 = "joda-time" % "joda-time" % "1.6"
  val saturn = "saturn" % "saturn" % "0.0" from "http://github.com/downloads/oxbowlakes/saturn/saturn_2.8.0-0.0.jar"
  //val jsr310 = "jsr310" % "jsr310" % "0.6.2" from "file:C:/Work/Scala/jsr310/jsr-310-ri-0.6.2.jar"
  val jsr310 = "jsr310" % "jsr310" % "0.6.2" from "https://jsr-310.dev.java.net/files/documents/6445/151013/jsr-310-ri-0.6.2.jar"
}