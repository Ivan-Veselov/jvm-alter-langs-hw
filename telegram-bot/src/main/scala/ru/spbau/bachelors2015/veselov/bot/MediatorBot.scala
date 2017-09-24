package ru.spbau.bachelors2015.veselov.bot

import akka.pattern.ask
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.SendMessage
import ru.spbau.bachelors2015.veselov.database.{DatabaseActor, DatabaseGet, DatabaseGetResponse, DatabaseWrite}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.{Random, Success}

/**
  * This bot receives message from user and resends it to another random user. Answer to original
  * message is persisted in the database and returned back to the original message sender. If answer
  * to some phrase is in the database then it's immediately returned as an answer.
  */
class MediatorBot(val token: String) extends TelegramBot with Commands with Polling {
  private val database = ActorSystem().actorOf(Props(classOf[DatabaseActor]))

  private val userState: mutable.HashMap[Long, UserState] = mutable.HashMap.empty

  private val random = new Random

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

                  case None =>
                    val freeUsers = userState.filter(pair =>
                                                          pair._2.isInstanceOf[DefaultState.type] &&
                                                          pair._1 != message.chat.id)
                                             .keySet

                    if (freeUsers.isEmpty) {
                      reply("Я занят, зайди позже.")
                    } else {
                      val userId = freeUsers.toVector(random.nextInt(freeUsers.size))
                      userState.update(userId,
                                       AnsweringState(text, message.messageId, message.chat.id))

                      request(SendMessage(userId, text))

                      userState.update(message.chat.id, AwaitingState)
                    }
                }

              case _ =>
                reply("Ошибка базы данных!")
            }

            case AnsweringState(phrase, messageId, recipient) =>
              database ! DatabaseWrite(phrase, text)
              request(SendMessage(recipient, text, None, None, None, Some(messageId)))
              userState.update(recipient, DefaultState)

              userState.update(message.chat.id, DefaultState)

            case AwaitingState =>
              reply("Погоди, я не успеваю.")
          }
      }
  }
}
