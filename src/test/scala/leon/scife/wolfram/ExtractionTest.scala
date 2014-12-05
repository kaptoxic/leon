package leon
package scife
package wolfram

import scala.util.parsing.combinator._

import org.scalatest._

class ExtractionTest extends FunSuite with JavaTokenParsers with Matchers {

  test("test") {
    import Extractor._

    Extractor.parsePortion(identifier ~ operator ~ expr, "s<=v<=e") shouldBe false
    Extractor.parsePortion(expr ~ operator ~ identifier ~ operator ~ expr, "s<=v<=e") shouldBe true
    Extractor.parsePortion(Extractor.constraint, "s<=v<=e") shouldBe true
    Extractor.parsePortion(Extractor.identifier, "s") shouldBe true
    Extractor.parsePortion(Extractor.identifier, "0") shouldBe false
    Extractor.parsePortion(Extractor.operator, "<") shouldBe true
    Extractor.parsePortion(Extractor.expr, "0") shouldBe true
    Extractor.parsePortion(Extractor.identifier, "s<0") shouldBe false
    Extractor.parsePortion(Extractor.identifier, "s<") shouldBe false
    Extractor.parsePortion(Extractor.expr ~ Extractor.operator, "s<0") shouldBe false
    Extractor.parsePortion(Extractor.expr ~ Extractor.operator, "s<") shouldBe true
    Extractor.parsePortion(Extractor.constraint, "s<0") shouldBe true
    
    Extractor.parsePortion(Extractor.constraint, "s<=x<v/2") shouldBe true
    
    val results = Extractor.parse("s<0 and s<=v<=e and s<=x<v/2")
    results.size shouldBe 5 
    System.out.println( results )

  }
  
}
