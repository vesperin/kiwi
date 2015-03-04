packageArchetype.java_application

lazy val commonSettings = Seq(
  organization  := "com.vesperin",
  version       := "0.1",
  scalaVersion  := "2.10.4",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8") 
)

lazy val kiwi = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    resolvers ++= Seq(
      "spray repo" at "http://repo.spray.io/",
      "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
      "sprest snapshots" at "http://sprest.io/releases"
    ),
    libraryDependencies ++= {
      val akkaV = "2.3.6"
      val sprayV = "1.3.2"
      Seq(
        "io.spray"            %%  "spray-can"             % sprayV,
        "io.spray"            %%  "spray-routing"         % sprayV,
        "io.spray"            %%  "spray-json"            % "1.3.1",
        "io.spray"            %%  "spray-testkit"         % sprayV  % "test",
        "com.typesafe.akka"   %%  "akka-actor"            % akkaV,
        "com.typesafe.akka"   %%  "akka-testkit"          % akkaV   % "test",
        "org.reactivemongo"   %%  "reactivemongo"         % "0.10.0",
        "sprest"              %%  "sprest-reactivemongo"  % "0.3.7",
        "com.lihaoyi"         %%  "scalatags"             % "0.4.5",
        "org.twitter4j"       %   "twitter4j-core"        % "4.0.2"
      )
    }
  )

