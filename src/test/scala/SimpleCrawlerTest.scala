import org.specs._

class TestCrawler extends Crawler {
  var result = ""
  def crawl = {
    navigateTo("http://www.google.com") {
      in(form having id("tsf")) {
        in(textField having id("lst-ib")) {
          typeIn("bplawler")
        }
        in(submit having name("btnK")) {
          click ==>
        }
      }
    }
    onCurrentPage {
      result = from(div having id("resultStats")) getTextContent
      
      forAll(div having xPath("""//ol[@id = "rso"]/li/div[@class = "vsc"]""")) {
        println(from(anchor having xPath("h3/a")) getTextContent)
      }
    }
  }
}

object SimpleCrawlerTest extends Specification {
  "Vanity Googling for bplawler" should {
    val c = new TestCrawler
    c.crawl
    "give me search results that start with 'About'" in {
      c.result must startWith("About")
    }
  }
}
