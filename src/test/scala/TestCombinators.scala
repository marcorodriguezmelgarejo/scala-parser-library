import org.scalatest.freespec.AnyFreeSpec
import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.should.Matchers._
import Parsers._

class TestCombinators extends AnyFreeSpec {
  "OR Combinator" - {
    val aob = stringParser("a") <|> stringParser("b")

    "Puede realizar un parseo exitoso si parsea el primer parser" in {
      aob("abc") shouldBe Success(Parseo("a", "bc"))
    }
    "Puede realizar un parseo exitoso si parsea el segundo parser" in {
      aob("b++") shouldBe Success(Parseo("b", "++"))
    }
    "Puede realizar un parseo erroneo" in {
      aob("XXX").isFailure shouldBe true
    }
  }

  "Concat Combinator" - {
    "De dos tipos iguales" - {
      val holaMundo = stringParser("hola") <> stringParser("mundo")

      "Puede realizar un parseo exitoso" in {
        holaMundo("holamundo1") shouldBe Success(Parseo(("hola", "mundo"), "1"))
      }
      "Puede realizar un parseo erroneo" in {
        holaMundo("holachau").isFailure shouldBe true
      }
    }

    "De dos tipos distintos" - {
      val starwars1 = stringParser("starwars") <> char('1')

      "Puede realizar un parseo exitoso" in {
        starwars1("starwars12") shouldBe Success(Parseo(("starwars", '1'), "2"))
      }

      "Puede realizar un parseo erroneo" in {
        starwars1("starwars").isFailure shouldBe true
      }
    }
  }

  "Rightmost Combinator" - {
    val holaMundo = stringParser("hola") ~> stringParser("mundo")

    "Puede realizar un parseo exitoso" in {
      holaMundo("holamundo1") shouldBe Success(Parseo("mundo", "1"))
    }

    "Puede realizar un parseo erroneo cuando no parsea el primero" in {
      holaMundo("mundo").isFailure shouldBe true
    }

    "Puede realizar un parseo erroneo cuando no parsea el segundo" in {
      holaMundo("hola").isFailure shouldBe true
    }
  }

  "Leftmost Combinator" - {
    val holaMundo = stringParser("hola") <~ stringParser("mundo")

    "Puede realizar un parseo exitoso" in {
      holaMundo("holamundo1") shouldBe Success(Parseo("hola", "1"))
    }

    "Puede realizar un parseo erroneo cuando no parsea el primero" in {
      holaMundo("mundo").isFailure shouldBe true
    }

    "Puede realizar un parseo erroneo cuando no parsea el segundo" in {
      holaMundo("hola").isFailure shouldBe true
    }
  }
}
