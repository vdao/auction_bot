package ru.yandex.money.ebay

import java.util.Date
import com.yandex.money.api.net.OAuth2Authorization
import com.yandex.money.api.net.OAuth2Session
import com.yandex.money.api.net.DefaultApiClient
import com.yandex.money.api.methods.AccountInfo

class Auction(val seller: String, val startingSum: Int, val itemDescription: String, val callback: (String, Option[Tuple2[String, Int]]) => Unit) {

  
  object State extends Enumeration {
    type State = Value
    val Offered, Started, Finished, Cancelled = Value
  }

  var currentState = State.Offered
  var finishTime: Date = null
  var bestOffer: Option[Tuple2[String, Int]] = None

  println("New auction started: " + seller)

  def answer(question: String, actor: String): Option[String] = {
    println("Question: " + question)
    currentState match {
      case State.Offered   => replyWhenOffered(question, actor)
      case State.Started   => replyWhenStarted(question, actor)
      case State.Cancelled => replyWhenCancelled(question)
      case State.Finished  => replyWhenFinished(question)
    }
  }

  def replyWhenOffered(question: String, actor: String): Option[String] = {
    if (question.startsWith("/info")) {
      return Some("""Объявляется аукцион:
         Товар: %s
         Стартовая цена: %d
         Ожидайте начала торгов.""" format (itemDescription, startingSum))
    } else if (question.startsWith("/begin") && actor.equals(seller)) {
      val time = 60000
      finishTime = new Date(System.currentTimeMillis() + time)
      currentState = State.Started

      new Thread(new Runnable {
        def run() {
          Thread.sleep(time);
          currentState = State.Finished
          println("Торги закончены по таймауту.")

          if (bestOffer.isEmpty) {
            callback.apply("Аукцион завершен. Товар не продан", bestOffer)
          } else {
            callback.apply("Аукцион завершен, победила ставка %d, пользователь %s" format (bestOffer.get._2, bestOffer.get._1), bestOffer)
          }
        }
      }).start();

      return Some("Внимание! Объявлено начало аукциона, до окончания торгов осталось 5 минут. Делайте ваши ставки!")
    }

    None
  }

  def replyWhenStarted(question: String, actor: String): Option[String] = {
    if (question.startsWith("/info")) {
      return Some("""Объявляется аукцион:
         Товар: %s
         Стартовая цена: %d
         торги закончатся %s, 
         текущее предложние: %s""" format (itemDescription, startingSum, finishTime, curSum()))
    } else if (question.startsWith("/bid")) {
      if (!Judge.tokensMap.contains(actor)) {
        val link = "https://money.yandex.ru/oauth/authorize?client_id=%s&response_type=code&redirect_uri=http://localhost:8080/%s&scope=account-info+payment-p2p" format (Judge.client_id, actor) 
       
        return Some("Авторизируйтесь в я.деньгах: " + link)
      }
      
      val session = new OAuth2Session(new DefaultApiClient(Judge.client_id))
      session.setAccessToken(Judge.tokensMap(actor))
      val info = session.execute(new AccountInfo.Request())
      
      val bid = parseBid(question);

      if (new java.math.BigDecimal(bid.toString()).compareTo(info.balance) >= 0) {
        return Some("Недостаточно денег для ставки")
      }
      
      if (bestOffer.isEmpty) {
        if (bid >= startingSum) {
          bestOffer = Some((actor, bid))
          return Some("Ставка принята %d" format (bid))
        } else {
          return Some("Ставка не принята, начальная цена %d" format (startingSum))
        }
      } else {
        val currentBid = bestOffer.get._2

        println("current_bid=%d, new_offer=%d" format (currentBid, bid))

        if (bid > currentBid) {
          bestOffer = Some((actor, bid))
          return Some("Cтавка перебита, лучшая цена %d" format (bid))
        } else {
          return Some("Ставка не принята, текущая ставка %d" format (currentBid))
        }
      }

    }
    None
  }

  def curSum(): String = bestOffer.map(_._2.toString()).getOrElse("'нет'")

  def replyWhenCancelled(question: String): Option[String] = {
    None
  }

  def replyWhenFinished(question: String): Option[String] = {
    None
  }

  def parseBid(question: String): Int = question.substring("/bid ".length()).toInt

}