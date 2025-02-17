import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.{Failure, Success, Try}
import Parsers._


class ParsersSimples extends AnyFreeSpec {

  "Parsers simples" - {
    "String" - {
      val parser = Parsers.stringParser("ab")
      "Puede realizar un parseo exitoso" in {
        parser("abc") shouldBe Success(Parseo("ab", "c"))
      }
      "Puede realizar un parseo erroneo" in {
        parser("b++").isFailure shouldBe true
      }
    }
    "Char" - {
      val parser = Parsers.char('a')
      "Puede realizar un parseo exitoso" in {
        parser("abc") shouldBe Success(Parseo('a', "bc"))
      }
      "Puede realizar un parseo erroneo" in {
        parser("bbb").isFailure shouldBe true
      }
    }
    "Digit" - {
      val parser = Parsers.digit
      "Puede realizar un parseo exitoso" in {
        parser("1ab") shouldBe Success(Parseo('1', "ab"))
      }
      "Puede realizar un parseo erroneo" in {
        parser("bbb").isFailure shouldBe true
      }
    }
    "Void" - {
      val parser = Parsers.void
      "Puede realizar un parseo exitoso" in {
        parser("ab") shouldBe Success(Parseo(null, "b"))
      }
      "Puede realizar un parseo erroneo" in {
        parser("").isFailure shouldBe true
      }
    }
    "AnyChar" - {
      val parser = Parsers.anyChar
      "Puede realizar un parseo exitoso" in {
        parser("+b") shouldBe Success(Parseo('+', "b"))
      }
      "Puede realizar un parseo erroneo" in {
        parser("").isFailure shouldBe true
      }
    }
  }

}