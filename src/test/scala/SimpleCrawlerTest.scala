package crawler

import org.specs2._
import java.net.InetSocketAddress

case class TestCrawler(url: String) extends Crawler {
  var result = ""
  def crawl = {
    navigateTo(url) {
      in(form having id("simple_form")) {
        in(input having id("simple_input")) {
          typeIn("bplawler")
        }
        in(input having name("simple_button")) {
          click ==>
        }
      }
    }
    onCurrentPage {
      result = from(div having id("result_stats")).getTextContent
    }
  }
}

class SimpleCrawlerTest extends mutable.Specification with CrawlerSpec {
  "crawler" should {
    "work" in {
      val c = TestCrawler(address)
      c.crawl()
      c.result must contain("(input_name,bplawler)")
      ok
    }
  }
}
