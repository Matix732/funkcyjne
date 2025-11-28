import upickle.default.{ReadWriter, macroRW, read, write}
import scala.io.Source

case class SafeAppendIN(list: List[Int], element: Int, index: Int)
object SafeAppendIN { implicit val rw: ReadWriter[SafeAppendIN] = macroRW }

case class OptionIntListResponse(result: Option[List[Int]])
object OptionIntListResponse { implicit val rw: ReadWriter[OptionIntListResponse] = macroRW }

case class VarianceIN(list: List[Double])
object VarianceIN { implicit val rw: ReadWriter[VarianceIN] = macroRW }

case class OptionDoubleResponse(result: Option[Double])
object OptionDoubleResponse { implicit val rw: ReadWriter[OptionDoubleResponse] = macroRW }

case class TwoOptionsIN(list1: Option[List[Int]], list2: Option[List[Int]])
object TwoOptionsIN { implicit val rw: ReadWriter[TwoOptionsIN] = macroRW }

case class MeanIN(list: List[Double])
object MeanIN { implicit val rw: ReadWriter[MeanIN] = macroRW }

case class EitherResponse(isSuccess: Boolean, value: String)
object EitherResponse { implicit val rw: ReadWriter[EitherResponse] = macroRW }


// main

object Zadanie extends cask.MainRoutes {
  override def verbose = true

  def read_json[T: ReadWriter](nazwaPliku: String): T = {
    println(s"Wczytuję dane z: $nazwaPliku") // Dodatkowy log dla pewności
    val jsonString = Source.fromFile(nazwaPliku).getLines().mkString
    read[T](jsonString)
  }

  // Zadanie 3.0 safeAppend
  @cask.post("/safeAppend")
  def safeAppend(): ujson.Value = {
    // Wczytuje zad.3.0.json
    val dane = read_json[SafeAppendIN]("zad.3.0.json") //3.0 lub 3.0-bad dla błędu
    
    val wynik: Option[List[Int]] = 
      if (dane.index < 0 || dane.index > dane.list.length) None
      else {
        val (poczatek, koniec) = dane.list.splitAt(dane.index)
        Some(poczatek ++ (dane.element :: koniec))
      }

    write(OptionIntListResponse(wynik))
  }

  // Zadanie 3.5 wariancja z option
  @cask.post("/variance")
  def variance(): ujson.Value = {
    val dane = read_json[VarianceIN]("zad.3.5.json")

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    val wynik: Option[Double] = mean(dane.list).flatMap { m =>
      mean(dane.list.map(x => math.pow(x - m, 2)))
    }

    write(OptionDoubleResponse(wynik))
  }

  // Zadanie 4.0 połączenie dwóch list
  @cask.post("/concatOption")
  def concatOption(): ujson.Value = {
    val dane = read_json[TwoOptionsIN]("zad40.json")
    
    val wynik: Option[List[Int]] = for {
      l1 <- dane.list1
      l2 <- dane.list2
    } yield l1 ++ l2

    write(OptionIntListResponse(wynik))
  }

  // Zadanie 4.5 mojeMap 
  @cask.post("/mojeMap")
  def endpointMojeMap(): ujson.Value = {
    val dane = read_json[TwoOptionsIN]("zad.4.5.json")

    def mojeMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    val wynik = mojeMap(dane.list1, dane.list2)((l1, l2) => l1 ++ l2)

    write(OptionIntListResponse(wynik))
  }

  // Zadanie 5.0 średnia Either
  @cask.post("/meanEither")
  def meanEither(): ujson.Value = {
    val dane = read_json[MeanIN]("zad.5.0.json")

    val wynik: Either[String, Double] = 
      if (dane.list.isEmpty) Left("Lista jest pusta, nie można policzyć średniej")
      else Right(dane.list.sum / dane.list.length)

    val response = wynik match {
      case Right(v) => EitherResponse(isSuccess = true, value = v.toString)
      case Left(e)  => EitherResponse(isSuccess = false, value = e)
    }

    write(response)
  }

  initialize()
}