import sbt._

object Dependencies {
  val ZioVersion     = "2.0.17"
  val ZHTTPVersion   = "3.0.0-RC2"
  val ZioJsonVersion = "0.6.1"

  val `zio` = "dev.zio" %% "zio" % ZioVersion

  val `zio-http`      = "dev.zio" %% "zio-http" % ZHTTPVersion
  val `zio-http-test` = "dev.zio" %% "zio-http" % ZHTTPVersion % Test

  val `zio-json`      = "dev.zio" %% "zio-json" % ZioJsonVersion

  val `zio-test`      = "dev.zio" %% "zio-test"     % ZioVersion % Test
  val `zio-test-sbt`  = "dev.zio" %% "zio-test-sbt" % ZioVersion % Test
}
