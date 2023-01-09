// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import bulbyvr.ktane.{*, given}
import io.circe.* 
import scala.io.Source 
class MySuite extends munit.FunSuite {
  test("Soobway parses") {
    val json = parser.parse(Source.fromResource("subway.json").mkString).getOrElse(???)
    val data = json.as[KtaneMod]
    data match
      case Left(e) => 
       println(e.getMessage)
       throw new AssertionError("Got left")
      case Right(v) => ()
    
  }
}
