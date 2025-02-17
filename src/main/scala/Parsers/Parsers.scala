import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex


import scala.annotation.tailrec

package object Parsers {
  type ResultadoParseo[+T] = Try[Parseo[T]]
  case class Parseo[+T](consumido: T, noConsumido: String)

  def resultadoParseo[T](consumido: T, noConsumido: String): ResultadoParseo[T] = Try(Parseo(consumido, noConsumido))
  case class ParserFallidoException(str: String) extends RuntimeException

  // El Parser es covariante, porque un Parser de Notas, es tambien un Parser de Tocables. Lo mismo puede decirse sobre
  //  el Parseo y el ResultadoParseo
  case class Parser[+T](parseo: String => ResultadoParseo[T]) extends (String => ResultadoParseo[T]) {
    def apply(parseable: String): ResultadoParseo[T] =
      if (parseable.isEmpty) {
        Failure[Parseo[T]](ParserFallidoException("No se puede parsear una string vacía."))
      } else {
        parseo(parseable)
      }

    def filter(cond: T => Boolean, stringFallo: String): Parser[T] =
      Parser({ input: String =>
        this (input) match {
          case s@Success(Parseo(c, _)) if cond(c) => s
          case _ => Failure(ParserFallidoException(stringFallo))
        }
      })

    // Retorna un nuevo parser, que si logra realizar el parseo, retorna un resultadoParseo con la transformacion aplicada,
    //  y si no lo logra, realiza el parseo alternativo.
    // (transform es como un flatmap para el try. Recibe dos funciones: la que le tiene que aplicar si es un Success,
    //  y la que le tiene que aplicar si es un Failure)
    private def transformOrElse[T1](transformacion: Parseo[T] => ResultadoParseo[T1], parseoAlternativo: String => ResultadoParseo[T1]): Parser[T1] =
      Parser({ input: String => this(input).transform(transformacion, _ => parseoAlternativo(input)) })

    def map[T1](f: T => T1): Parser[T1] =
      Parser({ input: String =>
        this(input).map(parseo => Parseo(f(parseo.consumido), parseo.noConsumido))
      })

    def mapParseo[T1](f: Parseo[T] => Parseo[T1]): Parser[T1] =
      Parser({ input: String =>
        this(input).map(parseo => f(parseo))
      })

    def flatMap[T2](f: T => Parser[T2]): Parser[T2]= {
      Parser(s => {
        this (s) match {
          case Success(parseo) => f(parseo.consumido)(parseo.noConsumido)
          case Failure(e) => Failure(e)
        }})
    }

    def <|>[T1, T2 >: T with T1](parser: Parser[T1]): Parser[T2] = Parser({ parseable =>
      val resultado = this (parseable)
      resultado match {
        case Success(_) => resultado.asInstanceOf[ResultadoParseo[T2]]
        case Failure(_) => parser(parseable).asInstanceOf[ResultadoParseo[T2]]
      }
    })

//    def <|>[T1, T2 >: T with T1](parser: Parser[T1]): Parser[T2] =
//      this.transformOrElse(parseo => Try(parseo).asInstanceOf[ResultadoParseo[T2]], parser.asInstanceOf[Parser[T2]])

    def <>[T1](parser: Parser[T1]): Parser[(T, T1)] = this.mapParseo(parseo =>
      {
        val resultadoSegundoParser: ResultadoParseo[T1] = parser(parseo.noConsumido)
        Parseo((parseo.consumido, resultadoSegundoParser.get.consumido), resultadoSegundoParser.get.noConsumido)
      }
    )

    def ~>[T1](parser: Parser[T1]): Parser[T1] = (this <> parser).map{case (primerConsumido, segundoConsumido) => segundoConsumido}

    def <~[T1](parser: Parser[T1]): Parser[T] = (this <> parser).map{case (primerConsumido, segundoConsumido) => primerConsumido}

    def satisfy(cond: T => Boolean): Parser[T] = this.filter(cond, "No cumple la condicion")

    def opt: Parser[T] = this <|>[Null, T] parserNulo

    def const[T1](constante: T1): Parser[T1] = map(_ => constante)

    @tailrec
    private def recursivo[T1 >: T](anterior: ResultadoParseo[List[T1]]) : ResultadoParseo[List[T1]] = {
      this(anterior.get.noConsumido) match {
        case Success(Parseo(c,nc)) => this.recursivo(resultadoParseo(anterior.get.consumido :+ c, nc))
        case Failure(_) => anterior
      }
    }

    def *(): Parser[List[T]] = this.map(p => List(p)).transformOrElse(parseo => recursivo(Try(parseo)), input => resultadoParseo(List(), input))

    def +(): Parser[List[T]] = this.map(p => List(p)).transformOrElse(parseo => recursivo(Try(parseo)), _ => Failure(ParserFallidoException("No se pudo parsear ni una vez")))

    @tailrec
    private def pasoMedio[T1 , T2 >: T](separador: Parser[T1], anterior: ResultadoParseo[List[T2]]): ResultadoParseo[List[T2]] = {
        separador(anterior.get.noConsumido) match {
          case Success(Parseo(_, noConsumido)) =>
            this(noConsumido) match {
              case Failure(_) => Failure(ParserFallidoException("No se pudo realizar el parser contenido luego del parser separador"))
              case Success(Parseo(consumido, noConsumido)) => pasoMedio(separador, resultadoParseo(anterior.get.consumido :+ consumido, noConsumido))
            }
          case Failure(_) => anterior
        }
    }

    def sepBy[T1](parserPasoMedio:Parser[T1]): Parser[List[T]] =
      Parser({parseable =>
        val resultado = this.map(p => List(p)) (parseable)
        resultado match {
          case Success(_) => pasoMedio(parserPasoMedio,resultado)
          case Failure(algo) => Failure(algo)
        }
      })
  }

  val parserNulo: Parser[Null] = Parser(s => Try(Parseo(null, s)))

  def parserExpresionRegular(expresionRegular: Regex): Parser[String] = Parser[String](
    s =>
      Try(
        expresionRegular.findPrefixOf(s).map(consumido => resultadoParseoString(consumido, s))
          .getOrElse(throw ParserFallidoException(
            "No se encontro la expresion regular "+ expresionRegular + " en la string " + s + "."))
      )
  )

  private def resultadoParseoString(consumido: String, original: String) = {
    Parseo(consumido, original.stripPrefix(consumido))
  }

  // parsers
  def stringParser(stringABuscar: String): Parser[String] = Parser[String](
    s => {
    require(stringABuscar.nonEmpty, "No se puede buscar una string vacía.")
      Try(
        if(s.startsWith(stringABuscar)) {
          resultadoParseoString(stringABuscar, s)
        } else {
          throw ParserFallidoException("No se encontro la string " + stringABuscar + " en la string " + s + ".")
        })
    })

  def char(caracterABuscar: Char): Parser[Char] = anyChar.satisfy(_ == caracterABuscar)

  val anyChar: Parser[Char] = Parser(s => resultadoParseo(s.head, s.tail))

  val void: Parser[Null] = anyChar.const(null)

  val digit: Parser[Char] = anyChar.satisfy(_.isDigit)

  val letter: Parser[Char] = anyChar.satisfy(_.isLetter)

  val alphaNum: Parser[Char] = letter <|> digit
}
