package ru.spbau.bachelors2015.veselov.bot

sealed trait UserState

case object DefaultState extends UserState

case class AnsweringState(phrase: String, idOfRecipient: Long) extends UserState

case object AwaitingState extends UserState
