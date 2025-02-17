import org.scalatest.freespec.AnyFreeSpec
import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.should.Matchers._
import Parsers._

class TestOperacionesUnariasParsers extends AnyFreeSpec {
  "satisfy" - {
    val parserBase = stringParser("ab") <|> stringParser("a")
    val parser = parserBase.satisfy((p: String) => p.length == 2)
    "Puede realizar un parseo exitoso" in {
      parser("abc") shouldBe Success(Parseo("ab", "c"))
    }
    "Puede realizar un parseo erroneo" in {
      parser("bbb").isFailure shouldBe true // el parser Base reconoce "a", pero la condicion no se cumple porque el largo de "a" no es 2
    }
  }
  "opt" - {
    val parserBase = stringParser("a")
    val parser = parserBase.opt
    "Puede realizar un parseo exitoso con contenido" in {
      parser("abc") shouldBe Success(Parseo("a", "bc"))
    }
    "Puede realizar un parseo exitoso sin contenido" in {
      parser("bbb") shouldBe Success(Parseo(null, "bbb"))
    }
  }

  "Clasura de Kleene" - {
    val parserBase = stringParser("a")
    val parser = parserBase.*()
    "Puede realizar un parseo exitoso con contenido" in {
      parser("aaaabc") shouldBe Success(Parseo(List("a", "a", "a", "a"), "bc"))
    }
    "Puede realizar un parseo exitoso sin contenido" in {
      parser("bbb") shouldBe Success(Parseo(List(), "bbb"))
    }
  }

  "Clausura positiva" - {
    val parserBase = stringParser("a")
    val parser = parserBase.+()
    "Puede realizar un parseo exitoso con contenido" in {
      parser("aaaabc") shouldBe Success(Parseo(List("a", "a", "a", "a"), "bc"))
    }
    "Puede realizar un parseo erroneo sin contenido" in {
      parser("bbb").isFailure shouldBe true
    }
  }

  "map" - {
    val parserBase = stringParser("a")
    val parser = parserBase.map((p: String) => p.length)
    "Puede realizar un parseo exitoso" in {
      parser("abc") shouldBe Success(Parseo(1, "bc"))
    }
    "Puede realizar un parseo erroneo" in {
      parser("bbb").isFailure shouldBe true
    }
  }

  "sepBy" - {
    val parserBase = digit
    val parser = parserBase.sepBy(char('-'))

    "Puede realizar un parseo exitoso" in {
      parser("1-2-3-4-5-6-7-8-9-0") shouldBe Success(Parseo(List('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'), ""))
    }
    "Puede realizar un parseo fallido donde hay un separador de mas al final" in {
      parser("1-2-3-4-5-6-7-8-9-0-").isFailure shouldBe true
    }
    "Puede realizar un parseo fallido donde hay un separador de mas al inicio" in {
      parser("-1-2-3-4-5-6-7-8-9-0").isFailure shouldBe true
    }
    "Puede realizar un parseo fallido donde hay dos separadores" in {
      parser("1--2").isFailure shouldBe true
    }
    "Puede realizar un parseo fallido donde hay solo separadores" in {
      parser("--").isFailure shouldBe true
    }
  }
}
