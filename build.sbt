import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

name 			:= "vesper-http"

organization  	:= "com.vesperin"

version 		:= "0.0.1"

scalaVersion 	:= "2.10.3"

scalacOptions 	:= Seq("-unchecked", "-deprecation", "-encoding", "utf8")

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  "sprest snapshots" at "http://markschaake.github.com/releases"
)


libraryDependencies ++= {
  val akkaVersion  = "2.2.4"
  val sprayVersion = "1.2.1"
  Seq(
    "io.spray"              %   "spray-can"            % sprayVersion
    , "io.spray"            %   "spray-routing"        % sprayVersion
    , "io.spray"            %%  "spray-json"           % "1.2.5"
    , "io.spray"            %   "spray-testkit"        % sprayVersion  % "test"
    , "com.typesafe.akka"   %%  "akka-actor"           % akkaVersion
    , "com.typesafe.akka"   %%  "akka-testkit"         % akkaVersion   % "test"
    , "org.reactivemongo"   %%  "reactivemongo"        % "0.10.0"
    , "sprest"              %%  "sprest-reactivemongo" % "0.3.2"
    , "org.specs2"          %%  "specs2-core"          % "2.3.7" % "test"
    , "org.twitter4j"       %   "twitter4j-core"       % "4.0.2"
    , "com.scalatags"       %%  "scalatags"            % "0.3.8"
  )
}

seq(Revolver.settings: _*)
