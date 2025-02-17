import org.scalatest.freespec.AnyFreeSpec
import scala.util.{Failure, Success, Try}
import Parsers._
import Musiquita._
import ParsersMusica._
import org.scalatest.matchers.should.Matchers._

class TestMusiquita extends AnyFreeSpec {
  "Silencios" - {
    val parser = silencio
    "Se puede parsear un silencio de blanca" in {
      silencio("_") shouldBe Success(Parseo(Silencio(Blanca), ""))
    }
    "Se puede parsear un silencio de negra" in {
      silencio("-") shouldBe Success(Parseo(Silencio(Negra), ""))
    }
    "Se puede parsear un silencio de corchea" in {
      silencio("~") shouldBe Success(Parseo(Silencio(Corchea), ""))
    }
  }
  "Sonido" - {
    "Tono" - {
      "Puede realizar un parseo exitoso" in {
        tono("8A") shouldBe Success(Parseo(Tono(8, A), ""))
      }
      "Puede realizar un parseo erroneo" in {
        tono("I").isFailure shouldBe true
      }
    }
    "Nota" - {
      "Puede realizar un parseo exitoso de nota sola" in {
        nota("A") shouldBe Success(Parseo(A, ""))
      }
      "Puede realizar un parseo exitoso de nota sostenido" in {
        nota("A#") shouldBe Success(Parseo(A.sostenido, ""))
      }
      "Puede realizar un parseo exitoso de nota bemol" in {
        nota("Ab") shouldBe Success(Parseo(A.bemol, ""))
      }
      "Puede realizar un parseo erroneo" in {
        nota("I").isFailure shouldBe true
      }
    }
    "Figura" - {
      "Puede realizar un parseo exitoso" in {
        figura("1/1") shouldBe Success(Parseo(Redonda, ""))
      }
      "Puede realizar un parseo erroneo" in {
        figura("1/3").isFailure shouldBe true
      }
    }
  }
  "Acorde" - {
    "Definicion explicita" - {
      "Puede realizar un parseo exitoso" in {
        acorde("6A+6C#+6G1/8") shouldBe Success(Parseo(Acorde(List(Tono(6, A), Tono(6, C.sostenido), Tono(6, G)), Corchea), ""))
      }
      "Puede realizar un parseo erroneo" in {
        acorde("6A+6C#++6G1/8").isFailure shouldBe true
      }
    }

    "Acorde Mayor" - {
      "Puede realizar un parseo exitoso" in {
        acorde("6AM1/2") shouldBe Success(Parseo(A.acordeMayor(6, Blanca), ""))
      }
      "Puede realizar un parseo erroneo" in {
        acorde("6AZ1/2").isFailure shouldBe true
      }
    }

    "Acorde Menor" - {
      "Puede realizar un parseo exitoso" in {
        acorde("6Am1/2") shouldBe Success(Parseo(A.acordeMenor(6, Blanca), ""))
      }
      "Puede realizar un parseo erroneo" in {
        acorde("6Az1/2").isFailure shouldBe true
      }
    }
  }

  "Melodia" - {
    "Puede realizar un parseo de melodia" in {
      melodia("4A#1/4 - 4DM1/8") shouldBe Success(Parseo(List(Sonido(Tono(4,As),Negra),Silencio(Negra),D.acordeMayor(4,Corchea)),""))
    }
    "Melodia compleja" in {
      melodia("4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2 - 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2") shouldBe
        Success(Parseo(List(Acorde(List(Tono(4, A), Tono(4, Cs), Tono(4, E)), Corchea), Sonido(Tono(5, C), Corchea), Sonido(Tono(5, Cs), Corchea),
          Sonido(Tono(5, Cs), Corchea), Sonido(Tono(5, Ds), Corchea), Sonido(Tono(5, C), Corchea), Sonido(Tono(4, As), Corchea), Sonido(Tono(4, Gs),
            Blanca), Silencio(Negra), Sonido(Tono(4, As), Corchea), Sonido(Tono(4, As), Corchea), Sonido(Tono(5, C), Negra), Sonido(Tono(5, Cs),
            Corchea), Sonido(Tono(4, As), Negra), Sonido(Tono(4, Gs), Blanca), Sonido(Tono(5, Gs), Negra), Sonido(Tono(5, Gs), Negra), Sonido(Tono(5, Ds), Blanca)), ""))
    }
    "Reproducir" in {
//      AudioPlayer.reproducir(melodia("5D#1/8 4A#1/4 4G1/8 5D#1/4 4A#1/4 4G1/8 5D#1/4 4A#1/8 5C#1/4 4G#1/4 4F1/8 5C#1/4 4G#1/4 4F1/8 5C#1/4 4G#1/4 5C#1/4 4G#1/4 4F1/8 5C#1/4 4G#1/4 4F1/8 5C#1/4 4G#1/4 5C1/4 4G#1/4 4F1/8 5C1/4 4G#1/4 4F1/8 5C1/4 4G#1/8").get.consumido)
     }

    "Puede realizar un parseo erroneo de melodia" in {
      melodia("4A#1/4 - 4DM1/8 ").isFailure shouldBe true
    }
  }
}
