name := "telegram-bot"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "info.mukel"                %% "telegrambot4s"     % "3.0.9",

  "com.typesafe.akka"         %% "akka-actor"        % "2.4.20",
  "com.typesafe.akka"         %% "akka-persistence"  % "2.4.20",
  "org.iq80.leveldb"           % "leveldb"           % "0.9",
  "org.fusesource.leveldbjni"  % "leveldbjni-all"    % "1.8"
)
