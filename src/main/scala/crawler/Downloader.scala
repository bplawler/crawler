package crawler

import org.apache.http.HttpEntity
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.cookie.Cookie

/**
 * Created with IntelliJ IDEA.
 * User: arturas
 * Date: 8/16/13
 * Time: 12:34 PM
 * To change this template use File | Settings | File Templates.
 */
object Downloader {
  /**
   * Downloads given url via GET and returns response entity.
   *
   * Import `crawler._` and use `download(url).getContent.getBytes` to get
   * actual content bytes.
   */
  def download(url: String, cookies: Set[Cookie]=Set.empty): HttpEntity = {
    val http = new DefaultHttpClient()

    val cookieStore = http.getCookieStore
    cookies.foreach(cookieStore.addCookie)

    val response = http.execute(new HttpGet(url))
    response.getEntity
  }
}
