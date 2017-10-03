package ru.spbau.bachelors2015.veselov

import ru.spbau.bachelors2015.veselov.bot.MediatorBot
;

object Main extends App {
  val token = "444093259:AAHTiIG98KyVA9t23dz39oyxUp9SJqAnHnI"

  val bot = new MediatorBot(token)

  bot.run()
}
