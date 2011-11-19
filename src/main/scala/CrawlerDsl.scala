import com.gargoylesoftware.htmlunit._
import com.gargoylesoftware.htmlunit.html._
import scala.collection.JavaConversions._

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
  def having(dType: DiscriminatorType)(implicit c: crawler) =
    new FormProcessor(c, dType)
}

object textField {
  def having(dType: DiscriminatorType)(implicit c: crawler) =
    new TextFieldProcessor(c, dType)
}

object image {
  def having(dType: DiscriminatorType)(implicit c: crawler) =
    new ImageInputProcessor(c, dType)
}

object anchor {
  def having(dType: DiscriminatorType)(implicit c: crawler) = 
    new AnchorProcessor(c, dType)
}

object div {
  def having(dType: DiscriminatorType)(implicit c: crawler) = 
    new DivProcessor(c, dType)
}

object submit {
  def having(dType: DiscriminatorType)(implicit c: crawler) = 
    new SubmitProcessor(c, dType)
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
         val crawler: crawler = null, 
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
  * that retrieves lists by xpath is high up in the hierarchy, and the
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
    crawler.pop
  }
}

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
      case dt: id => { 
        parentElement.asInstanceOf[HtmlElement].
                      getElementById[HtmlTextInput](dt.id) 
      }
    }
  }
}

class SubmitProcessor(c: crawler, dType: DiscriminatorType) 
 extends ElementProcessor(c, dType) {
  def resolveNode(parentElement: DomNode): DomNode = {
    discriminatorType match {
      case dt: name => { 
        parentElement.asInstanceOf[HtmlForm].
                      getInputByName[HtmlSubmitInput](dt.name) 
      }
      case dt: id => { 
        parentElement.asInstanceOf[HtmlElement].
                      getElementById[HtmlSubmitInput](dt.id) 
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
        parentElement.getFirstByXPath[HtmlDivision](dt.xPath) 
      }
      case dt: id => { 
        parentElement.asInstanceOf[HtmlPage].
         getElementById(dt.id)
      }
    }
  }
}

/**
 * The main crawler class, which servers as the base class for individual
 * crawls.  This class is itself an element processor as it serves as the
 * starting point for navigation, and it also provides most of the tokens
 * that are part of the crawler DSL.
 */
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
