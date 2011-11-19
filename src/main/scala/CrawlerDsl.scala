import com.gargoylesoftware.htmlunit._
import com.gargoylesoftware.htmlunit.html._
import scala.collection.JavaConversions._

abstract class ElementProcessor(val crawler: crawler = null, val discriminatorType: DiscriminatorType = null) {
  protected var mainElement: DomNode = null
  abstract class ElementType {
    def foo : String
    def having(dType: DiscriminatorType)(implicit c: crawler) = {
      this.foo match {
        case "form" => { new FormProcessor(c, dType) }
        case "textField" => { new TextFieldProcessor(c, dType) }
        case "image" => { new ImageInputProcessor(c, dType) }
        case "anchor" => { new AnchorProcessor(c, dType) }
        case "div" => { new DivProcessor(c, dType) }
      }
    }
  }
  object form extends ElementType { def foo = "form" }
  object textField extends ElementType { def foo = "textField" }
  object image extends ElementType { def foo = "image" }
  object anchor extends ElementType { def foo = "anchor" }
  object div extends ElementType { def foo = "div" }

  def resolveNode(parent: DomNode): DomNode

  def resolveList(parentNode: DomNode): Seq[_] = {
    discriminatorType match {
      case dt: xPath => { 
        parentNode.getByXPath(dt.xPath)
      }
    }
  }

  def ==> = { 
    crawler.pushToEnd(this.mainElement) 
  }

  def ==> (block: => Unit) = {
    crawler.push(this.mainElement)
    block
    crawler.pop
  }
}

abstract class DiscriminatorType
case class id(id: String) extends DiscriminatorType
case class name(name: String) extends DiscriminatorType
case class xPath(xPath: String) extends DiscriminatorType
case class text(text: String) extends DiscriminatorType

class PageProcessor(page: HtmlPage, c: crawler, dType: DiscriminatorType = null)
 extends ElementProcessor(c, dType) {
  mainElement = page
  def resolveNode(parent: DomNode): DomNode = mainElement
}

class FormProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: id => { 
        parentElement.asInstanceOf[HtmlPage].
                      getHtmlElementById[HtmlForm](dt.id) 
      }
    }
  }
}

class TextFieldProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: name => { 
        parentElement.asInstanceOf[HtmlForm].
                      getInputByName[HtmlTextInput](dt.name) 
      }
    }
  }
}

class AnchorProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.asInstanceOf[HtmlPage].
          getFirstByXPath[HtmlAnchor](dt.xPath) 
      }
      case dt: text => {
        parentElement.asInstanceOf[HtmlPage].getAnchorByText(dt.text) 
      }
      case dt: id => {
        parentElement.asInstanceOf[HtmlPage].getElementById(dt.id) 
      }
    }
  }
}

class ImageInputProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: name => { 
        parentElement.asInstanceOf[HtmlForm].
                      getInputByName[HtmlImageInput](dt.name) 
      }
    }
  }
}

class DivProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: xPath => { 
        parentElement.getFirstByXPath[HtmlAnchor](dt.xPath) 
      }
    }
  }
}

class crawler(version: BrowserVersion = BrowserVersion.FIREFOX_3_6,
              failOnJSError: Boolean = false) extends ElementProcessor
{
  implicit val c: crawler = this
  private val client = new WebClient(BrowserVersion.FIREFOX_3_6)
  var nodeStack = List[DomNode]()
  private var currentUrl: String = ""

  client.setThrowExceptionOnScriptError(failOnJSError)
  var currentPage: HtmlPage = null // temporary, will need a context

  def resolveNode(parentElement: DomNode) = {
    client.getPage[HtmlPage](currentUrl)
  }

  protected def push(d: DomNode) = { nodeStack = d +: nodeStack }
  protected def pop = { nodeStack = nodeStack drop 1 }

  private def processBlock(processor: ElementProcessor)(block: => Unit) = { 
    nodeStack.length match {
      case 0 => push(processor.resolveNode(null))
      case _ => push(processor.resolveNode(nodeStack(0)))
    }
    block 
    pop
  }

  private def processList(processor: ElementProcessor)(block: => Unit) = { 
    for(element <- processor.resolveList(nodeStack(0))) {
      push(element.asInstanceOf[DomNode])
      block 
      pop
    }
  }

  protected def pushToEnd(node: DomNode) = { nodeStack = nodeStack :+ node }

  def navigateTo(url: String) = {
    currentUrl = url
    processBlock(this) _
  }

  def onCurrentPage(block: => Unit) = {
    block 
    nodeStack = nodeStack drop 1
  }

  def typeIn(s: String) = {
    nodeStack(0).asInstanceOf[HtmlInput].setValueAttribute(s)
  }

  def click = {
    new PageProcessor(
      nodeStack(0).asInstanceOf[HtmlElement].click[HtmlPage](), this)
  }

  def in(processor: ElementProcessor) = {
    processBlock(processor) _
  }

  def forAll(processor: ElementProcessor) = {
    processList(processor) _
  }

  def from(processor: ElementProcessor): DomNode = {
    processor.resolveNode(nodeStack(0))
  }
}
