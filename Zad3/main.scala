import upickle.default.{macroRW, read, write, readwriter, ReadWriter => RW} 
import scala.io.Source
import scala.annotation.tailrec

// zad 3 - struktury 

sealed trait ListaWiazana[+A] {

  // Zadanie 3.0 funkcja tail - zwraca tail listy
  def tail: ListaWiazana[A] = this match {
    case PustaLista => PustaLista
    case Element(head, tail) => tail
  }

  // Zadanie 3.5 funkcja drop - usuwa pierwsze n elementów listy
  @tailrec
  final def drop(n: Int): ListaWiazana[A] = this match {
    case Element(head, tail) if n > 0 => tail.drop(n - 1)
    case _ => this
  }

  // Zadanie 4.0 funkcja dropWhile - usuwa elementy z początku listy spełniające warunek p
  @tailrec
  final def dropWhile(p: A => Boolean): ListaWiazana[A] = this match {
    case Element(head, tail) if p(head) => tail.dropWhile(p)
    case _ => this
  }

  // Zadanie 4.5 foldLeft - składa listę od lewej z operacją op i wartością startową z
  @tailrec
  final def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case PustaLista => z
    case Element(head, tail) => tail.foldLeft(op(z, head))(op)
  }

  // Zadanie 5.0 funkcja concatenate - łączy dwie listy
  def concatenate[B >: A](other: ListaWiazana[B]): ListaWiazana[B] = this match {
    case PustaLista => other
    case Element(head, tail) => Element(head, tail.concatenate(other))
  }

  // konwersja naszej struktury na liste Scali
  def toScalaList: List[A] = this match {
    case PustaLista => Nil
    case Element(head, tail) => head :: tail.toScalaList
  }
}

case class Element[+A](head: A, override val tail: ListaWiazana[A]) extends ListaWiazana[A]
case object PustaLista extends ListaWiazana[Nothing]

object ListaWiazana {
  // konwersja listy Scali na naszą
  def fromScalaList[A](list: List[A]): ListaWiazana[A] = list match {
    case Nil => PustaLista
    case h :: t => Element(h, fromScalaList(t))
  }
  // ReadWriter json
  implicit def listRw[A: RW]: RW[ListaWiazana[A]] = 
    readwriter[List[A]].bimap(
      customList => customList.toScalaList,
      scalaList => ListaWiazana.fromScalaList(scalaList)
    )
}


case class TailRequest(list: ListaWiazana[Int])
object TailRequest { implicit val rw: RW[TailRequest] = macroRW }

case class ListaWiazanaResponse(result: ListaWiazana[Int])
object ListaWiazanaResponse { implicit val rw: RW[ListaWiazanaResponse] = macroRW }

case class DropRequest(list: ListaWiazana[Int], n: Int)
object DropRequest { implicit val rw: RW[DropRequest] = macroRW }

case class DropWhileRequest(list: ListaWiazana[Int])
object DropWhileRequest { implicit val rw: RW[DropWhileRequest] = macroRW }

case class FoldLeftRequest(list: ListaWiazana[Int])
object FoldLeftRequest { implicit val rw: RW[FoldLeftRequest] = macroRW }

case class FoldLeftResponse(result: Int)
object FoldLeftResponse { implicit val rw: RW[FoldLeftResponse] = macroRW }

case class ConcatRequest(listA: ListaWiazana[Int], listB: ListaWiazana[Int])
object ConcatRequest { implicit val rw: RW[ConcatRequest] = macroRW }


case class IsSortedIN(list: List[Int], order: String) 
object IsSortedIN { implicit val rw: RW[IsSortedIN] = macroRW }

case class IsSortedOUT(isSorted: Boolean) 
object IsSortedOUT { implicit val rw: RW[IsSortedOUT] = macroRW }

case class ThreeListsIN(list1: List[Int], list2: List[Int], list3: List[Int])
object ThreeListsIN { implicit val rw: RW[ThreeListsIN] = macroRW }

case class SumOUT(result: List[Int])
object SumOUT { implicit val rw: RW[SumOUT] = macroRW }

case class SetHeadIN(element: Int, list: List[Int]) 
object SetHeadIN { implicit val rw: RW[SetHeadIN] = macroRW }

case class ListOUT(result: List[Int])
object ListOUT { implicit val rw: RW[ListOUT] = macroRW }

case class AppendIN(element: Int, index: Int, list: List[Int])
object AppendIN { implicit val rw: RW[AppendIN] = macroRW }

case class TwoListsIN(list1: List[Int], list2: List[Int]) 
object TwoListsIN { implicit val rw: RW[TwoListsIN] = macroRW }

case class SquareOUT(result: List[Double]) 
object SquareOUT { implicit val rw: RW[SquareOUT] = macroRW }


//main

object Zadanie extends cask.MainRoutes {
  override def verbose = true

  def read_json[T: RW](nazwaPliku: String): T = {
    println(s"Wczytuję dane z pliku: $nazwaPliku")
    val jsonString = Source.fromFile(nazwaPliku).getLines().mkString
    read[T](jsonString)
  }

  // Zadanie 3.0 tail
  @cask.post("/tail")
  def tail(): ujson.Value = {
    val dane = read_json[TailRequest]("zad.3.0.json")
    val wynik = dane.list.tail 
    write(ListaWiazanaResponse(wynik))
  }

  // Zadanie 3.0 dla pustej listy
  @cask.post("/tailEmpty")
  def tailEmpty(): ujson.Value = {
    val dane = read_json[TailRequest]("zad.3.0-empty.json")
    val wynik = dane.list.tail 
    write(ListaWiazanaResponse(wynik))
  }

  // Zadanie 3.5 drop
  @cask.post("/drop")
  def drop(): ujson.Value = {
    val dane = read_json[DropRequest]("zad.3.5.json")
    val wynik = dane.list.drop(dane.n)
    write(ListaWiazanaResponse(wynik))
  }

  // Zadanie 4.0 dropWhile
  @cask.post("/dropWhile")
  def dropWhile(): ujson.Value = {
    val dane = read_json[DropWhileRequest]("zad.4.0.json")
    // warunek while (elementy mniejsze od 5)
    val warunek = (x: Int) => x < 5
    val wynik = dane.list.dropWhile(warunek)
    write(ListaWiazanaResponse(wynik))
  }

  // Zadanie 4.5 foldLeft
  @cask.post("/foldLeft")
  def foldLeft(): ujson.Value = {
    val dane = read_json[FoldLeftRequest]("zad.4.5.json")
    // def dodawania 
    val operacja = (suma: Int, element: Int) => suma + element
    val wartoscPoczatkowa = 0
    val wynik = dane.list.foldLeft(wartoscPoczatkowa)(operacja)
    write(FoldLeftResponse(wynik))
  }

  // Zadanie 5.0 concatenate
  @cask.post("/concatenate")
  def concatenate(): ujson.Value = {
    val dane = read_json[ConcatRequest]("zad.5.0.json")
    val wynik = dane.listA.concatenate(dane.listB)
    write(ListaWiazanaResponse(wynik))
  }



//=====================================================================================

  // Zadanie 3.0 sprawdzanie posortowania
  @cask.post("/isSorted")
  def isSorted(): ujson.Value = {
    val dane = read_json[IsSortedIN]("zad3.0.json")
    val compare: (Int, Int) => Boolean = dane.order match {
      case "abc"  => (a, b) => a <= b // sortuj rosnąco
      case "cba" => (a, b) => a >= b // sortuj malejąco
    }
    val wynik = dane.list.sliding(2).forall {
      case List(a, b) => compare(a, b)
      case _ => true // brak pary
    }
    write(IsSortedOUT(wynik))
  }

  // Zadanie 3.5 sumowanie liczb z trzech list
  @cask.post("/sumRows")
  def sumRows(): ujson.Value = {
    val dane = read_json[ThreeListsIN]("zad3.5.json")
    val sumujTrzy = (a: Int, b: Int, c: Int) => a + b + c
  
    // zipped - łączy listy element po elemencie
    val wynik = (dane.list1, dane.list2, dane.list3).zipped.map(sumujTrzy)
    write(SumOUT(wynik))
  }

  // Zadanie 4.0 dodanie elementu na początek 
  @cask.post("/setHead")
  def setHead(): ujson.Value = {
    val dane = read_json[SetHeadIN]("zad4.0.json")
    val wynik = dane.element :: dane.list
    write(ListOUT(wynik))
  }

  // Zadanie 4.5 dodanie elementu w wybranym miejscu
  @cask.post("/append")
  def append(): ujson.Value = {
    val dane = read_json[AppendIN]("zad4.5.json")
    val (part1, part2) = dane.list.splitAt(dane.index)
    val wynik = part1 ++ (dane.element :: part2)
    write(ListOUT(wynik))
  }

  // Zadanie 5.0 kwadrat sumy liczb z dwóch list
  @cask.post("/squareLists")
  def squareLists(): ujson.Value = {
    val dane = read_json[TwoListsIN]("zad5.0.json")
    val pary = dane.list1.zip(dane.list2)
    val wynik = pary.map {
      case (a, b) => math.pow(a + b, 2)
    }
    write(SquareOUT(wynik))
  }

  initialize()
}