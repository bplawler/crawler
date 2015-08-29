package crawler

import akka.actor._
import akka.stream._
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import org.specs2._
import org.specs2.specification.BeforeAfterAll
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.http.scaladsl.model.headers.RawHeader

trait CrawlerSpec extends BeforeAfterAll { self: mutable.Specification =>

  implicit var sys: ActorSystem = _
  var address: String = _

  def beforeAll = {
    sys = ActorSystem("CrawlerSpec")
    implicit val mat = ActorMaterializer()

    val route = path("") {
      get {
        parameterMap { map =>
          if (map.isEmpty) {
            getFromResource("simple_form.html")
          }
          else {
            // marshall as html instead of xml which is the default
            implicit val mar = ScalaXmlSupport.nodeSeqMarshaller(MediaTypes.`text/html`)
            complete(<html><body><div id="result_stats">{map}</div></body></html>)
          }
        }
      }
    }

    val bindTimeout = 1.second
    val binding = Await.result(Http().bindAndHandle(route, "localhost", 0), bindTimeout)
    address = "http://" + binding.localAddress.getHostString + ":" + binding.localAddress.getPort
  }

  def afterAll = {
    sys.shutdown()
    sys.awaitTermination()
  }

}
