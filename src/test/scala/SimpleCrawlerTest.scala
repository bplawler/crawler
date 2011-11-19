import org.specs._

class testCrawler extends crawler {
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
    }
  }
}

object SimpleCrawlerTest extends Specification {
  "Vanity Googling for bplawler" should {
    val c = new testCrawler
    c.crawl
    "give me search results that start with 'About'" in {
      c.result must startWith("About")
    }
  }
}
