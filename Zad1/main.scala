import upickle.default.{ReadWriter, macroRW, read, write} 
import scala.io.Source

case class IsSortedIN(list: List[Int], order: String)
object IsSortedIN {
  implicit val rw: ReadWriter[IsSortedIN] = macroRW
}
case class IsSortedOUT(isSorted: Boolean)
object IsSortedOUT {
  implicit val rw: ReadWriter[IsSortedOUT] = macroRW
}
case class ThreeListsIN(list1: List[Int], list2: List[Int], list3: List[Int])
object ThreeListsIN {
  implicit val rw: ReadWriter[ThreeListsIN] = macroRW
}
case class SumOUT(result: List[Int])
object SumOUT {
  implicit val rw: ReadWriter[SumOUT] = macroRW
}
case class SetHeadIN(element: Int, list: List[Int])
object SetHeadIN {
  implicit val rw: ReadWriter[SetHeadIN] = macroRW
}
case class ListOUT(result: List[Int])
object ListOUT {
  implicit val rw: ReadWriter[ListOUT] = macroRW
}
case class AppendIN(element: Int, index: Int, list: List[Int])
object AppendIN {
  implicit val rw: ReadWriter[AppendIN] = macroRW
}
case class TwoListsIN(list1: List[Int], list2: List[Int])
object TwoListsIN {
  implicit val rw: ReadWriter[TwoListsIN] = macroRW
}
case class SquareOUT(result: List[Double])
object SquareOUT {
  implicit val rw: ReadWriter[SquareOUT] = macroRW
}


// main
object Zadanie extends cask.MainRoutes {
  override def verbose = true

  def read_json[T: ReadWriter](nazwaPliku: String): T = {
    val jsonString = Source.fromFile(nazwaPliku).getLines().mkString
    read[T](jsonString) 
  }

  //    Zadanie 3.0: Sprawdzanie posortowania listy
  //        http://localhost:8080/isSorted
  @cask.post("/isSorted")
  def isSorted(): ujson.Value = {
    val dane = read_json[IsSortedIN]("zad3.0.json")
    val compare: (Int, Int) => Boolean = dane.order match {
      case "abc"  => (a, b) => a <= b // sortuj rosnąco
      case "cba" => (a, b) => a >= b // sortuj malejąco
    }
    // 1) .sliding - podziel na listy par sasiednich elementów
    // 2) compare(a, b) - porównaj pary
    // .forall sprawdza, czy warunek jest prawdziwy dla wszystkich okienek 
    val wynik = dane.list.sliding(2).forall {
      case List(a, b) => compare(a, b)
      case _ => true // brak pary
    }
    write(IsSortedOUT(wynik))
  }

  //    Zadanie 3.5: Sumowanie liczb z trzech list
  //        http://localhost:8080/sumRows
  @cask.post("/sumRows")
  def sumRows(): ujson.Value = {
    val dane = read_json[ThreeListsIN]("zad3.5.json")
    val sumujTrzy = (a: Int, b: Int, c: Int) => a + b + c
  
    // zipped - łączy listy element po elemencie
    val wynik = (dane.list1, dane.list2, dane.list3).zipped.map(sumujTrzy)
    write(SumOUT(wynik))
  }

  //  Zadanie 4.0: Dodanie elementu na początek listy
  //     http://localhost:8080/setHead
  @cask.post("/setHead")
  def setHead(): ujson.Value = {
    val dane = read_json[SetHeadIN]("zad4.0.json")
    // element -> :: nowa lista z elementem na początku + reszta listy
    val wynik = dane.element :: dane.list
    write(ListOUT(wynik))
  }

  //  Zadanie 4.5: Dodanie elementu w wybranym miejscu listy
  //    http://localhost:8080/append
  @cask.post("/append")
  def append(): ujson.Value = {
    val dane = read_json[AppendIN]("zad4.5.json")
    // 1) przecinanie listy na dwie części
    val (part1, part2) = dane.list.splitAt(dane.index)
    // 2) :: dodanie elementu na początku drugiej części
    // 3) ++ łączenie list
    val wynik = part1 ++ (dane.element :: part2)
    write(ListOUT(wynik))
  }

  //  Zadanie 5.0: Kwadrat sumy liczb z dwóch list
  //    http://localhost:8080/squareLists
  @cask.post("/squareLists")
  def squareLists(): ujson.Value = {
    val dane = read_json[TwoListsIN]("zad5.0.json")
    // 1) zip - połączenie dwóch list w pary
    val pary = dane.list1.zip(dane.list2)
    // 2) map - dla każdej pary mapuj funkcje matem
    val wynik = pary.map {
      case (a, b) => math.pow(a + b, 2)
    }
    write(SquareOUT(wynik))
  }

  initialize()
}