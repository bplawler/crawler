# crawler - A DSL for web crawling in Scala

The purpose of this project is to provide a nice DSL wrapper around the
cumbersome htmlunit Java library.  Here is an example taken from a unit 
test in this package:

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

This `testCrawler` class defines a crawl that will navigate to google, 
find the form whose id is `tsf`, type something into the form, then
click on the submit button named `btnK`, which will then take us to a 
new page (the search results) where we can then grab the content of the
`resultStats` div.
