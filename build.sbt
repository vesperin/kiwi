name 			:= "vesper-http"

organization  	:= "com.vesperin"

version 		:= "0.0.1"

scalaVersion 	:= "2.10.3"

scalacOptions 	:= Seq("-unchecked", "-deprecation", "-encoding", "utf8")

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)


libraryDependencies ++= {
  val akkaVersion  = "2.2.3"
  val sprayVersion = "1.2.0"
  Seq(
    "io.spray"            %   "spray-can"        % sprayVersion,
    "io.spray"            %   "spray-routing"    % sprayVersion,
    "io.spray"            %   "spray-testkit"    % sprayVersion  % "test",
    "com.typesafe.akka"   %%  "akka-actor"       % akkaVersion,
    "com.typesafe.akka"   %%  "akka-testkit"     % akkaVersion   % "test",
    "org.specs2"          %%  "specs2-core"      % "2.3.7" % "test"
  )
}

seq(Revolver.settings: _*)

