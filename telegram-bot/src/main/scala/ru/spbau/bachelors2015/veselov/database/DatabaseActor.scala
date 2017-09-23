package ru.spbau.bachelors2015.veselov.database

import akka.persistence.PersistentActor

import scala.collection.mutable

class DatabaseActor extends PersistentActor {
  val map: mutable.HashMap[String, String] = mutable.HashMap.empty

  private def handleModification(modification: DatabaseModification) = {
    modification match {
      case DatabaseWrite(key, value) => map.update(key, value)
    }
  }

  override def receiveRecover: Receive = {
    case modification: DatabaseModification => handleModification(modification)
  }

  override def receiveCommand: Receive = {
    case modification: DatabaseModification => persist(modification)(handleModification)

    case DatabaseGet(key) => sender ! DatabaseGetResponse(map.get(key))
  }

  override def persistenceId: String = "mediator-bot-database"
}
