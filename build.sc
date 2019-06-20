import mill._, scalalib._

object bencode extends ScalaModule {
  def scalaVersion = "2.12.8"
  def ivyDeps = Agg(
    ivy"org.scodec::core:1.10.4",
    ivy"com.monovore::decline:0.5.0",
    ivy"com.lihaoyi::pprint:0.5.5",
    ivy"org.typelevel::cats-core:1.6.1"
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  def scalacOptions = List(
    "-language:higherKinds",
    "-Ypartial-unification"
  )
}

object bittorrent extends Module {
  def moduleDeps = List(bencode)
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:1.6.1",
    ivy"com.github.bigwheel::util-backports:1.1"
  )
  object test extends TestModule
}

trait Module extends ScalaModule {
  def scalaVersion = "2.12.8"

  trait TestModule extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
