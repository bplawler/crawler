package crawler

import com.gargoylesoftware.htmlunit._
import com.gargoylesoftware.htmlunit.html._
import org.apache.http.HttpEntity
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

/*
 * The following family of objects represent the HTML element types that this
 * DSL understands.  The fact they are declared as objects means that they 
 * can simply show up in the grammar as tokens, as in:
 *
 *   in(form having id("tsf")) { ... }
 *
 * The "having" method of all of these objects declarations receives a
 * "discriminator type" which will ultimately be used to find the HTML
 * element that we're looking for.  In the above example, "form" is an object,
 * "having" is a method on that object, and "id" is a case class (defined 
 * later) that extends from DiscriminatorType.  The example above basically
 * means "in a form (in the current context) which has id attribute "tsf", 
 * do something. 
 */
object form {
  def having(dType: DiscriminatorType)(implicit c: Crawler) =
    new FormProcessor(c, dType)
}

object input {
  def having(dType: DiscriminatorType)(implicit c: Crawler) =
    new InputProcessor(c, dType)
}

object image {
  def having(dType: DiscriminatorType)(implicit c: Crawler) =
    new ImageProcessor(c, dType)
}

object anchor {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new AnchorProcessor(c, dType)
}

object div {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new DivProcessor(c, dType)
}

object li {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new LiProcessor(c, dType)
}

object area {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new AreaProcessor(c, dType)
}

object paragraph {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new ParagraphProcessor(c, dType)
}

object span {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new SpanProcessor(c, dType)
}

object script {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new ScriptProcessor(c, dType)
}

object tableDataCell {
  def having(dType: DiscriminatorType)(implicit c: Crawler) = 
    new TableDataCellProcessor(c, dType)
}

/*
 * The following family of case classes all descend from DiscriminatorType,
 * and provide the ElementProcessor classes with the information they need
 * to find a given HTML element in the page, a process called "node 
 * resolution".  Based on the DiscriminatorType, the ElementProcessor classes
 * will call different API's against the underlying htmlunit library to 
 * resolve the node.
 */
abstract class DiscriminatorType
case class id(id: String) extends DiscriminatorType
case class name(name: String) extends DiscriminatorType
case class title(title: String) extends DiscriminatorType
case class xPath(xPath: String) extends DiscriminatorType
case class text(text: String) extends DiscriminatorType

/**
 * Base class which contains the common functionality shared by all of the
 * different element processor children, and to provide the abstract contract
 * that children must fill in.  The basic job of an ElementProcessor is to
 * encapsulate the htmlunit calls that need to be done in order to find a
 * certain HTML node in the DOM tree.
 */
abstract class ElementProcessor(
         val crawler: Crawler = null, 
         val discriminatorType: DiscriminatorType = null) 
{
 /**
  * This variable contains the node that this element processor resolved to.
  */
  protected var mainElement: DomNode = null

 /**
  * Contract for the method that is used to resolve the node for this element
  * processor.  Note that a parent node is passed in.  Frequently in htmlunit
  * DOM processing is done relative to a parent.  This DSL represents that
  * by allowing embedded code blocks which represent operations to be done
  * within the scope of the ElementProcessor directly preceeding the code 
  * block.
  */
  def resolveNode(parent: DomNode): DomNode

 /**
  * List resolution is a special case that occurs with the "forAll"
  * functionality in the DSL.  Conveniently, the underlying htmlunit call
  * that retrieves lists by xpath is high up in the hierarchy, at the
  * DomNode level, so we can just make list resolution a general thing - 
  * we don't need to put it in the individual ElementProcessor subclasses.
  */
  def resolveList(parentNode: DomNode): Seq[_] = {
    discriminatorType match {
      case dt: xPath => { 
        parentNode.getByXPath(dt.xPath)
      }
    }
  }

 /**
  * "push" operator, which has the effect of pushing the main element of 
  * the current processor onto the BOTTOM OF the node stack, so that when
  * we unravel the nested processing of the DSL, the item left at the top
  * of the stack will be the object that was pushed onto the bottom with
  * this operator.
  */
  def ==> = { 
    crawler.pushToEnd(this.mainElement) 
  }

 /**
  * "push" operator, which rather than pushing the main element onto the stack
  * will instead push the main element to a code block (typically a function
  * call).
  */
  def ==> (block: => Unit) = {
    crawler.push(this.mainElement)
    block
    crawler.pop()
  }
}

/*
 * Here are all of the ElementProcessor implementations.
 */
class PageProcessor(page: Page, c: Crawler, dType: DiscriminatorType = null)
 extends ElementProcessor(c, dType) {
  mainElement = page.asInstanceOf[DomNode]
  def resolveNode(parent: DomNode): DomNode = mainElement
  def url = page.getUrl.toString
}

class FormProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: id => { 
        parentElement.asInstanceOf[HtmlPage].
                      getHtmlElementById[HtmlForm](dt.id) 
      }
      case dt: name => { 
        parentElement.asInstanceOf[HtmlPage].
                      getElementByName[HtmlForm](dt.name) 
      }
    }
  }
}

class InputProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: name => { 
        parentElement.asInstanceOf[HtmlForm].
                      getInputByName[HtmlInput](dt.name) 
      }
      case dt: id => { 
        parentElement.asInstanceOf[HtmlElement]
                     .getPage
                     .asInstanceOf[HtmlPage]
                     .getElementById[HtmlInput](dt.id, false) 
      }
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlInput](dt.xPath) 
      }
    }
  }
}

class AnchorProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlAnchor](dt.xPath) 
      }
      case dt: text => {
        parentElement.asInstanceOf[HtmlPage].getAnchorByText(dt.text) 
      }
      case dt: id => {
        parentElement.asInstanceOf[HtmlPage].getElementById(dt.id) 
      }
      case dt: title => {
        parentElement.getFirstByXPath[HtmlAnchor](
          """//a[@title="%s"]""".format(dt.title)
        )
      }
    }
  }
}

class ImageProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlImage](dt.xPath) 
      }
    }
  }
}

class LiProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlListItem](dt.xPath) 
      }
    }
  }
}

class DivProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlDivision](dt.xPath) 
      }
      case dt: id => { 
        parentElement.asInstanceOf[HtmlPage].
         getElementById(dt.id)
      }
    }
  }
}

class AreaProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlArea](dt.xPath) 
      }
    }
  }
}

class ParagraphProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlParagraph](dt.xPath) 
      }
    }
  }
}

class SpanProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlSpan](dt.xPath) 
      }
    }
  }
}

class ScriptProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlScript](dt.xPath) 
      }
    }
  }
}

class TableDataCellProcessor(c: Crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlTableDataCell](dt.xPath) 
      }
    }
  }
}

trait CrawlObserver {
  def configure(m: java.util.Map[String, String])
  def click: crawler.PageProcessor
  def doCrawl()
}

/**
 * The main Crawler class, which servers as the base class for individual
 * crawls.  This class is itself an element processor as it serves as the
 * starting point for navigation, and it also provides most of the tokens
 * that are part of the crawler DSL.
 */
abstract class Crawler( version: BrowserVersion = BrowserVersion.FIREFOX_24
                      , failOnJSError: Boolean = false
                      , javaScriptEnabled: Boolean = true
                      , throwExceptionOnFailingStatusCode: Boolean = false
                      , cssEnabled: Boolean = false
                      , useInsecureSSL: Boolean = true
                      , dumpPreProcessedJavaScript: Boolean = false
                      ) extends ElementProcessor with CrawlObserver
{
 /**
  * Set up the current Crawler as an implicit value that will be 
  * automatically filled into a method call if not provided by the
  * caller.  This was added in order to make the Crawler instance 
  * implicitly available to the HTMLElement object definitions.
  */
  implicit val c: Crawler = this

 /**
  * HtmlUnit class that actually does all the work.
  */
  private[this] val client = new WebClient(version)

  // Set the various switches to affect the behavior of this client.
  client.getOptions.setThrowExceptionOnScriptError(failOnJSError)
  client.getOptions.setJavaScriptEnabled(javaScriptEnabled)
  client.getOptions.setThrowExceptionOnFailingStatusCode(throwExceptionOnFailingStatusCode)
  client.getOptions.setUseInsecureSSL(useInsecureSSL)
  client.getOptions.setCssEnabled(cssEnabled)
  if (! cssEnabled) {
    client.setCssErrorHandler(new SilentCssErrorHandler())
  }
  val actualScriptPreProcessor = client.getScriptPreProcessor
  lazy val debugScriptPreProcessor = new ScriptPreProcessor() {
    def preProcess(htmlPage: HtmlPage, sourceCode: String, sourceName: String, lineNumber: Int, htmlElement: HtmlElement) = {
      System.out.println(s"** preProcessing script: name <$sourceName> code <$sourceCode>")
      actualScriptPreProcessor.preProcess(htmlPage, sourceCode, sourceName, lineNumber, htmlElement);
    }
  }
  if(dumpPreProcessedJavaScript) {
    client.setScriptPreProcessor(debugScriptPreProcessor)
  }

  protected var config = collection.mutable.Map[String, Any]();
 
 /**
  * List of DomNode instances that functions as a stack.  As the 
  * DSL finds nodes the user is allowed to embed operations to 
  * perform on those nodes, including the discovery of sub-nodes
  * within the current node.  Therefore, we need to store the
  * notion of a current node on the stack so that when processing
  * of the child nodes is complete we can restore this node 
  * back into the current.
  */
  var nodeStack = List[DomNode]()

 /**
  * Used by the navigateTo method to store the URL that the 
  * user is trying to visit.
  */
  private var currentUrl: String = ""

 /**
  * Simple implemention of resolveNode, which will simply attempt
  * to go to the page requested by the user.
  */
  def resolveNode(parentElement: DomNode) = {
    client.getPage[SgmlPage](currentUrl)
  }

 /**
  * Pushes a new DomNode onto the top of the stack.  This is called
  * when the DSL has resolved a new node and now needs to push it
  * onto the stack so that the next processing block will have
  * access to it.
  */
  protected[crawler] def push(d: DomNode) = { 
    nodeStack = d +: nodeStack 
  }

 /**
  * Pops a DomNode off of the node stack.  Called when a process
  * block for a node concludes, and we want to restore the previous
  * node to the top of the stack.
  */
  protected[crawler] def pop() = { nodeStack = nodeStack drop 1 }

 /**
  * Receives an ElementProcessor instance and the 
  * block of code that is to be executed against the node that is resolved
  * by that ElementProcessor.
  */
  private def processBlock(processor: ElementProcessor)(block: => Unit) = {
    // attempt to resolve the node,
    (
      nodeStack.length match {
        case 0 => processor.resolveNode(null)
        case _ => processor.resolveNode(nodeStack.head)
      }
    // then process the block.  If no node is resolved, skip the block.
    ) match {
      case null =>
      case n: DomNode => push(n); block; pop()
    }
  }

 /** 
  * Similar to the processBlock function, processList is a special case 
  * function invoked by the forAll() token in this DSL.  Rather than resolve
  * a single node, this method will resolve a list of nodes, iterate over them,
  * and call the block of code that was passed in on each.
  */
  private def processList(processor: ElementProcessor)(block: => Unit) = { 
    for(element <- processor.resolveList(nodeStack.head)) {
      push(element.asInstanceOf[DomNode])
      block 
      pop
    }
  }

 /**
  * Method called by the push operator (==>) which has the effect of 
  * pushing the node parameter onto the bottom of the stack so that
  * when the stack has been emptied, there at the very bottom will 
  * be this new node.
  */
  protected[crawler] def pushToEnd(node: DomNode) = { nodeStack :+= node }

  def crawl()

  override def doCrawl() {
    crawl()
    client.closeAllWindows()
  }

 /**
  * For some crawls, additional configuration parameters will be passed 
  * in post-instantiation.  This method is on the base Crawler class so that
  * it may be called generically on any child class.  It is up to the 
  * crawler implementation to validate and make use of the configuration.
  */
  override def configure(m: java.util.Map[String, String]) = { config ++= m }

  def navigateTo(url: String) = {
    currentUrl = url
    processBlock(this) _
  }

  def retrieveUrl(url: String): Page = {
    client.getPage[Page](url)
  }

  def onCurrentPage(block: => Unit) = {
    block 
    nodeStack = nodeStack drop 1
  }

  def typeIn(s: String) = {
    nodeStack.head.asInstanceOf[HtmlInput].setValueAttribute(s)
  }

  override def click = {
    val stackItem = nodeStack.head
    val element = stackItem.asInstanceOf[HtmlElement]
    val clickResult = Await.result(
                        Future { element.click[HtmlPage]() }
                      , 120.second
                      )
    val start = (new java.util.Date).getTime
    val stillRunning = client.waitForBackgroundJavaScript(1000 /* ms */)
    println("waited %d ms for background JS, %d still running..."
      .format((new java.util.Date).getTime - start, stillRunning))
    new PageProcessor(clickResult, this)
  }

  def mouseOver = {
    val stackItem = nodeStack.head
    val element = stackItem.asInstanceOf[HtmlElement]
    val clickResult = element.mouseOver()
    new PageProcessor(clickResult, this)
  }

  def in(processor: ElementProcessor) = {
    try {
      processBlock(processor) _
    } catch {
      case e: Throwable => println(page.asString); throw e
    }
  }

  def forAll(processor: ElementProcessor) = {
    processList(processor) _
  }

  def from(processor: ElementProcessor): DomNode = {
    processor.resolveNode(nodeStack.head)
  }

  def page = nodeStack.head.asInstanceOf[SgmlPage]

  /**
   * Downloads given url via GET and returns response entity.
   *
   * Import `crawler._` and use `download(url).getBytes` to get
   * actual content bytes.
   */
  def download(url: String): HttpEntity = {
    val cookies = client.getCookieManager.getCookies.map(_.toHttpClient).toSet
    Downloader.download(url, cookies)
  }

  @deprecated("Please use `println(page.asString)` instead.", "0.6.0")
  def printPage() { println(page.asString) }

  @deprecated(
    "Instead of `printElement(node)` please use `node.asString`.", "0.6.0"
  )
  def printElement(node: org.w3c.dom.Node): String = node.asString
}
