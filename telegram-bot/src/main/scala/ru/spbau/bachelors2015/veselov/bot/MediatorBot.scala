package ru.spbau.bachelors2015.veselov.bot

import akka.pattern.ask
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.bachelors2015.veselov.database.{DatabaseActor, DatabaseGet, DatabaseGetResponse, DatabaseWrite}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Success

class MediatorBot(val token: String) extends TelegramBot with Commands with Polling {
  private val database = ActorSystem().actorOf(Props(classOf[DatabaseActor]))

  private val userState: mutable.HashMap[Long, UserState] = mutable.HashMap.empty

  onMessage {
    implicit message =>
      message.text.foreach {
        text =>
          implicit val timeout: Timeout = 1.second
          userState.getOrElseUpdate(message.chat.id, DefaultState) match {
            case DefaultState => (database ? DatabaseGet(text)).onComplete {
              case Success(DatabaseGetResponse(value)) =>
                value match {
                  case Some(answer) => reply(answer)

                  case None => userState.update(message.chat.id, AnsweringState(text))
                }

              case _ =>
                reply("Ошибка базы данных!")
            }

            case AnsweringState(phrase) =>
              database ! DatabaseWrite(phrase, text)
              userState.update(message.chat.id, DefaultState)
          }
      }
  }
}
