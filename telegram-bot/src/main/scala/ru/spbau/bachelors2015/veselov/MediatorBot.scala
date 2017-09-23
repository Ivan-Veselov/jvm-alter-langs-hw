package ru.spbau.bachelors2015.veselov

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.Commands

class MediatorBot(val token: String) extends TelegramBot with Commands with Polling {
  onMessage {
    implicit message =>
      message.text.foreach {
        text =>
          reply(text)
      }
  }
}
