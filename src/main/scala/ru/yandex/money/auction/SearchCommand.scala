package ru.yandex.money.auction

import java.net.URLEncoder
import java.util.Objects

import org.json4s.{DefaultFormats, Formats}
import com.squareup.okhttp.Request.Builder
import com.squareup.okhttp.{Call, Request, OkHttpClient}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization


/**
  * Created by pavel2 on 16.03.2016.
  */
class SearchCommand {

  val httpClient = Judge.createOkHttpClient()

  //
  //
  //  {
  //    id: 335,
  //    title: "МТС (Россия)"
  //  },
  //{
  //  id: 9824,
  //  title: "МТС: домашний интернет, телевидение и телефония",
  //  format: "json"
  //},
  //  {
  //    id: 5551,
  //    title: "ООО Мтс-Агропродукт",
  //    url: "https://money.yandex.ru/api/showcase/validate/5551/step_INN_3038",
  //    params: {
  //      supplierInn: "6164252382"
  //    },
  //    format: "json"
  //  },

  trait Showcase

  case class XForms(id: Int, title: String) extends Showcase

  case class JForms(id: Int, title: String, format: String) extends Showcase

  case class JFormsRequisites(id: Int, title: String, url: String, params: Map[String, String], format: String) extends Showcase

  object BarSerializer extends CustomSerializer[Showcase](format => ( {
    case x: JObject =>
      x match {
        case JObject(JField("id", JInt(i)) :: JField("title", JString(t)) :: Nil) => XForms(i.intValue(), t)
        case JObject(JField("id", JInt(i)) :: JField("title", JString(t)) :: JField("format", JString(f)) :: Nil) => JForms(i.intValue(), t, f)
        case JObject(JField("id", JInt(i)) :: JField("title", JString(t)) :: JField("url", JString(u)) :: JField("params", p) :: JField("format", JString(f)) :: Nil) => JFormsRequisites(i.intValue(), t, u, p.extract[Map[String, String]], f)
        case _ => null
      }
    case JObject(JField("id", JInt(i)) :: JField("title", JString(s)) :: Nil) => XForms(i.intValue(), s)
  }, {
    case x: Showcase => null
  }
    ))

  implicit val formats: Formats = Serialization.formats(NoTypeHints) + BarSerializer

  def execute(query: String): String = {
    val result: Call = httpClient.newCall(new Builder().get().url("https://money.yandex.ru/api/showcase-search?records=15&query=" + URLEncoder.encode(query)).build())
    val jsonResponse = parse(result.execute().body().string())
    val showcases: List[Showcase] = (jsonResponse \ "result").extract[List[Showcase]]

    val jsonForms = showcases.map((e) => e match {
      case JForms(i, t, f) => t
      case _ => null
    }).filter(Objects.nonNull)

    val answer = new StringBuilder("Found:\n")
    for (i <- 1 to jsonForms.length) {
      answer.append(i + ": " + jsonForms(i - 1) + "\n")
    }

    println(answer)
    return answer.toString()
  }

}
