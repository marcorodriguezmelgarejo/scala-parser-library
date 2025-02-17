import Parsers._
import ParsersMusica._

package object Musiquita {
  //silencio
  val silencio: Parser[Silencio] = char('_').const(Silencio(Blanca)) <|>
    char('-').const(Silencio(Negra)) <|>
    char('~') .const(Silencio(Corchea))
  //sonido
  val notasola: Parser[Nota] = char('A').const(A) <|>
    char('B').const(B) <|> char('C').const(C) <|>
    char('D').const(D) <|> char('E').const(E) <|>
    char('F').const(F) <|> char('G').const(G)
"b#"
  val notaSostenido: Parser[Nota] = (notasola <~ char('#')).map(nota => nota.sostenido)
  val notaBemol: Parser[Nota] = (notasola <~ char('b')).map(nota => nota.bemol)
  val notaConModificador: Parser[Nota] = notaSostenido <|> notaBemol
  val nota: Parser[Nota] = notaConModificador <|> notasola

  val figura: Parser[Figura] = stringParser("1/1").const(Redonda) <|>
    stringParser("1/2").const(Blanca) <|>
    stringParser("1/4").const(Negra) <|>
    stringParser("1/8").const(Corchea) <|>
    stringParser("1/16").const(SemiCorchea)
  val tono: Parser[Tono] = (digit <> nota).map{case (octava, nota) => Tono(octava.getNumericValue, nota)}
  val sonido: Parser[Sonido] = (tono <> figura).map{case (tono, figura) => Sonido(tono, figura)}

  //acorde
  val acordeExplicito: Parser[Acorde] = (tono.sepBy(char('+')) <> figura)
    .map({case(tonos,figura) => Acorde(tonos,figura)})

  val acordeMenor: Parser[Acorde] = ((tono <~ char('m')) <> figura)
    .map({case(tono,figura) => tono.nota.acordeMenor(tono.octava, figura)})
  val acordeMayor: Parser[Acorde] = ((tono <~ char('M')) <> figura)
    .map({case(tono,figura) => tono.nota.acordeMayor(tono.octava, figura)})
  val acordeMenorOMayor: Parser[Acorde] = acordeMayor <|> acordeMenor
  val acorde: Parser[Acorde] = acordeExplicito <|> acordeMenorOMayor

  //melodia
  val tocable: Parser[Tocable] = silencio <|> sonido <|> acorde
  val melodia: Parser[Melodia] = tocable.sepBy(char(' '))
}
