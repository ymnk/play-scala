import play.test._
import org.scalatest.matchers.ShouldMatchers
 
class BasicTest extends UnitFlatSpec with ShouldMatchers {

  it should "a Very Inportant Thing to Test" in {
    (1+1) should equal (2)
  }

}
