import com.gargoylesoftware.htmlunit.Page
import javax.xml.transform.{TransformerFactory, OutputKeys}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import java.io.{ByteArrayOutputStream, InputStream, StringWriter}
import org.apache.http.HttpEntity

package object crawler {
  implicit class NodeExtensions(val node: org.w3c.dom.Node) extends AnyVal {
    private def transformer = {
      val t = TransformerFactory.newInstance().newTransformer()
      t.setOutputProperty(OutputKeys.INDENT, "yes")
      t
    }

    /** Transform node into string **/
    def asString: String = {
      // initialize StreamResult with File object to save to file
      val result = new StreamResult(new StringWriter())
      val source = new DOMSource(node)
      transformer.transform(source, result)
      result.getWriter.toString
    }
  }

  implicit class StreamExtensions(val is: InputStream) extends AnyVal {
    def getBytes(bufferSize: Int): Array[Byte] = {
      val buffer = new ByteArrayOutputStream

      var nRead = 0
      val data = new Array[Byte](bufferSize)
      def read(): Int = {
        nRead = is.read(data, 0, data.length)
        nRead
      }

      while (read() != -1) { buffer.write(data, 0, nRead) }

      buffer.flush()
      buffer.toByteArray
    }

    def getBytes: Array[Byte] = getBytes(32 * 1024)
  }

  implicit class HttpEntityExtensions(val he: HttpEntity) extends AnyVal {
    def getBytes: Array[Byte] = {
      val buffer = new ByteArrayOutputStream
      he.writeTo(buffer)
      buffer.flush()
      buffer.toByteArray
    }
  }
}
