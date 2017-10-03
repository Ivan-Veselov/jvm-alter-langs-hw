package ru.spbau.bachelors2015.veselov.database

sealed trait DatabaseMessage

sealed trait DatabaseModification extends DatabaseMessage

case class DatabaseWrite(key: String, value: String) extends DatabaseModification

case class DatabaseGet(key: String) extends DatabaseMessage

case class DatabaseGetResponse(value: Option[String]) extends DatabaseMessage
