package ru.yandex.money.auction

import java.io.{File, PrintWriter}
import java.net.URLEncoder
import java.util.concurrent.TimeUnit

import com.squareup.okhttp.{ConnectionPool, OkHttpClient, Request}
import com.typesafe.config.ConfigFactory
import com.yandex.money.api.methods.params.P2pTransferParams
import com.yandex.money.api.model.{MoneySource, Wallet}
import com.yandex.money.api.net.{DefaultApiClient, OAuth2Session}
import com.yandex.money.api.processes.{IPaymentProcess, PaymentProcess}
import com.yandex.money.api.utils.MillisecondsIn
import io.vertx.core.{Handler, Vertx}
import io.vertx.core.http.{HttpClient, HttpServerRequest}
import org.json4s._
import org.json4s.native.JsonMethods._

object Judge extends App {
  implicit val formats = DefaultFormats

  val config = ConfigFactory.load("bot.properties")

  val client_id = config.getString("client_id")
  val api_token = config.getString("api_token")
  val server_port = config.getInt("server_port")
  val telegram_url = config.getString("telegram_url")
  val bot_public_url = config.getString("bot_public_url")

  val vertx = Vertx.vertx()

  var offset =  0

  val httpClient = createHttpClient()
  val okHttpClient = createOkHttpClient()
  var receiver: String = null

  var auctionMap = Map[String, Auction]()

  var tokensMap = if (new File("tokens.properties").exists())
      scala.io.Source.fromFile("tokens.properties").getLines()
          .map((x) => x.trim.split("="))
          .map((x) => x(0) -> x(1))
          .toMap
     else Map[String, String]()

  println("restored token = " + tokensMap)

  startWS()
  run()

  def startWS(): Unit = {

    val server = vertx.createHttpServer();

    server.requestHandler(new Handler[HttpServerRequest] {
      def handle(request: HttpServerRequest) {
        // This handler gets called for each request that arrives on the server
        val response = request.response()
        response.putHeader("Location", bot_public_url)
        response.setStatusCode(302)

        val code = request.getParam("code")
        val uri = "https://money.yandex.ru/oauth/token?code=" + code + "&client_id=" + client_id + "&grant_type=authorization_code&redirect_uri=http%3A%2F%2Flocalhost:8080"
        println(uri)

        val requestBuilder = new com.squareup.okhttp.Request.Builder()
        val httpResp = okHttpClient.newCall(requestBuilder.url(uri).get().build()).execute()
        val tokenResponse = httpResp.body().string()
        Judge.tokensMap = Judge.tokensMap.+((request.path().substring(1), (parse(tokenResponse) \ "access_token").extract[String]))
        new PrintWriter("tokens.properties") {
          write(tokensMap.map((x: Tuple2[String, String]) => x._1 + "=" + x._2).fold("")((a, b) => a + "\n" + b).substring(1)); close
        }
        response.end()
      }
    });

    server.listen(server_port);
    println("Server started at localhost:" + server_port)

  }

  def run(): Unit = {
    while (true) {
      processMessage()
    }
  }

  def processMessage(): Unit = {
    val lastUpdates = callService("getUpdates", Map(("offset", offset.toString), ("timeout", "3"), ("limit", "1")))
    val command = parse(lastUpdates)

    println("json = " + lastUpdates)
    println("command = " + command)

    println(command \ "result")

    command \ "result" match {
      case JArray(List()) => Unit
      case JArray(h :: t) => {
        println("Got new message")

        offset = ((command \ "result")(0) \ "update_id").extract[Int] + 1
        //saveLastUpdate(offset);

        if (hasText(command)) {
          val chatId = ((command \ "result")(0) \ "message" \ "chat" \ "id").extract[String]
          val userInput = ((command \ "result")(0) \ "message" \ "text").extract[String]
          val actor = ((command \ "result")(0) \ "message" \ "from" \ "id").extract[String]

          println("chatId=" + chatId + ", text=" + userInput + ", actor=" + actor)

          if (!auctionMap.contains(chatId)) {
            if (userInput.startsWith("/init")) {
              val auction = parseInitText(chatId, actor, userInput)
              auctionMap = withNewAuction(chatId, auction)

              auction.answer("/info", actor) match {
                case Some(message) =>
                  sendMessage(chatId, message)
                case None => Unit
              }
            } else {
              return
            }
          }

          val auction = auctionMap.get(chatId).get;
          val reply = auction.answer(userInput, actor)

          reply match {
            case Some(message) =>
              sendMessage(chatId, message)
            case None => Unit
          }
        }

      }
      case _ => Unit
    }
  }

  def sendMessage(chatId: String, text: String): Unit = {
    callService("sendMessage", Map[String, String](("chat_id", chatId),
      ("text", text)))
  }

  def withNewAuction(chatId: String, auction: Auction) = {
    auctionMap.+((chatId, auction))
  }

  def parseInitText(chatId: String, actor: String, text: String): Auction = {
    var subtext = text.substring("/init".length()).trim()
    val split = subtext.split(" ", 3)
    receiver = split(1)
    new Auction(actor, split(0).toInt, split(2), (s, bestOffer) => {
      sendMessage(chatId, s)
      auctionMap = auctionMap.filterNot(_._1 == chatId)

      if (bestOffer.isDefined) {
        processPayment(bestOffer.get._1, bestOffer.get._2, receiver)
      }
    });
  }
  def callService(commandName: String, params: Map[String, String] = Map()) = {
    var uri = telegram_url + api_token + "/" + commandName + queryParams(params)
    okHttpClient.newCall(new Request.Builder().url(uri).get().build()).execute().body().string().replaceAllLiterally("\\/", "/")
  }

  def queryParams(params: Map[String, String]): String = {
    if (params isEmpty) {
      return ""
    }

    "?" + params.seq.map(p => p._1 + "=" + URLEncoder.encode(p._2)).reduce((a, b) => a + "&" + b)
  }

  def hasText(command: JValue): Boolean = {
    try {
      //      command.result(0).message.text.as[String]
      return true;
    } catch {
      case _ => return false;
    }
  }

  def createHttpClient(): HttpClient = vertx.createHttpClient();

  def createOkHttpClient(): OkHttpClient = {
    val client = new OkHttpClient();
    client.setReadTimeout(5, TimeUnit.SECONDS);
    client.setConnectTimeout(1, TimeUnit.SECONDS);
    client.setConnectionPool(new ConnectionPool(8, 10 * MillisecondsIn.MINUTE));
    client.setFollowSslRedirects(false);
    client.setFollowRedirects(false);
    return client;
  }

  def processPayment(payer: String, amount: Int, receiver: String) = {
    println("process-payment " +(payer, amount, receiver))

    val session = new OAuth2Session(new DefaultApiClient(client_id))
    session.setAccessToken(tokensMap(payer))

    val params = new P2pTransferParams.Builder(receiver)
      .setAmountDue(new java.math.BigDecimal(amount.toString()))
      .setComment("Аукцион")
      .setMessage("Аукцион")
      .create()

    val paymentProcess = new PaymentProcess(session, new IPaymentProcess.ParameterProvider() {
      def getPatternId(): String = params.getPatternId()

      def getPaymentParameters(): java.util.Map[String, String] = params.makeParams()

      def getMoneySource(): MoneySource = {
        Wallet.INSTANCE
      }

      def getCsc(): String = {
        null
      }

      def getExtAuthSuccessUri(): String = {
        null
      }

      def getExtAuthFailUri(): String = {
        null
      }
    });

    while (!paymentProcess.proceed()) {
      println("process-payment, waiting")
      Thread.sleep(2000);
    }

    println("payment completed")
  }

}