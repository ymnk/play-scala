import org.junit._
import play.test._
import play.mvc._
import play.mvc.Http._
import org.scalatest.matchers.ShouldMatchers

class ApplicationTest extends UnitFlatSpec with ShouldMatchers  {
  import FunctionalTest._

  it should "test That IndexPage Works" in {
    val response = GET("/")
    assertIsOk(response)
    assertContentType("text/html", response)
    assertCharset("utf-8", response)
  }

}
