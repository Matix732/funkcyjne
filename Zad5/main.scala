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


case class IntListIN(list: List[Int])
object IntListIN { implicit val rw: ReadWriter[IntListIN] = macroRW }

case class MapIntResponse(result: Map[String, Int])
object MapIntResponse { implicit val rw: ReadWriter[MapIntResponse] = macroRW }

case class StudentActivity(student: String, hours: Int)
object StudentActivity { implicit val rw: ReadWriter[StudentActivity] = macroRW }

case class StudentHoursIN(activities: List[StudentActivity])
object StudentHoursIN { implicit val rw: ReadWriter[StudentHoursIN] = macroRW }

case class SentencesIN(sentences: List[String])
object SentencesIN { implicit val rw: ReadWriter[SentencesIN] = macroRW }

case class BagOfWordsResponse(result: Map[String, Map[String, Int]])
object BagOfWordsResponse { implicit val rw: ReadWriter[BagOfWordsResponse] = macroRW }

case class StudentInfo(id: Int, name: String)
object StudentInfo { implicit val rw: ReadWriter[StudentInfo] = macroRW }

case class GradeInfo(studentId: Int, course: String, grade: Double)
object GradeInfo { implicit val rw: ReadWriter[GradeInfo] = macroRW }

case class JoinIN(students: List[StudentInfo], grades: List[GradeInfo])
object JoinIN { implicit val rw: ReadWriter[JoinIN] = macroRW }

case class AverageGradeResponse(result: Map[String, Double])
object AverageGradeResponse { implicit val rw: ReadWriter[AverageGradeResponse] = macroRW }


//main

object Zadanie extends cask.MainRoutes {
  override def verbose = true
  override def port = 8081 

  def read_json[T: ReadWriter](nazwaPliku: String): T = {
    println(s"Wczytuję dane z: $nazwaPliku") 
    val jsonString = Source.fromFile(nazwaPliku).getLines().mkString
    read[T](jsonString)
  }


//=====================================================================================

  // Zadanie 3.0: słownik: number - liczba jego wystąpień
  @cask.post("/countNumbers")
  def countNumbers(): ujson.Value = {
    val dane = read_json[IntListIN]("zad.3.0.json")
    val wynik = dane.list
      .map(num => (num.toString, 1))          // Krok Map
      .groupBy(_._1)                          // Krok Shuffle/Group
      .map { case (numStr, list) =>           // Krok Reduce
        numStr -> list.map(_._2).sum 
      }
  
    write(MapIntResponse(wynik))
  }

  // 3.5: Liczby do sześcianu (słownik: liczba -> sześcian)
  @cask.post("/cubeNumbers")
  def cubeNumbers(): ujson.Value = {
    val dane = read_json[IntListIN]("zad.3.5.json")
    val wynik = dane.list
      .map(num => (num.toString, num * num * num))
      .toMap 

    write(MapIntResponse(wynik))
  }

  // Zadanie 4.0 - słownik: suma godzin studenta
  @cask.post("/studentHours")
  def studentHours(): ujson.Value = {
    val dane = read_json[StudentHoursIN]("zad.4.0.json")
    val wynik = dane.activities
      .map(a => (a.student, a.hours))
      .groupBy(_._1)
      .map { case (student, entries) =>
        student -> entries.map(_._2).sum
      }

    write(MapIntResponse(wynik))
  }

  // Zadanie 4.0 - słownik: bag of words
  @cask.post("/bagOfWords")
  def bagOfWords(): ujson.Value = {
    val dane = read_json[SentencesIN]("zad.4.5.json")

    val wynik: Map[String, Map[String, Int]] = dane.sentences.map { zdanie =>
      val slowa = zdanie.toLowerCase.split("\\W+").filter(_.nonEmpty)
      val wektor = slowa
        .map(s => (s, 1))           // Map
        .groupBy(_._1)              // Group
        .map { case (s, l) => s -> l.map(_._2).sum } // Reduce
  
      zdanie -> wektor
    }.toMap

    write(BagOfWordsResponse(wynik))
  }

  // Zadanie 5.0 - słownik: średnia ocen studenta
  @cask.post("/studentAverage")
  def studentAverage(): ujson.Value = {
    val dane = read_json[JoinIN]("zad.5.0.json")

    val sredniePoId: Map[Int, Double] = dane.grades
      .groupBy(_.studentId)
      .map { case (id, gradesList) =>
        val suma = gradesList.map(_.grade).sum
        val ilosc = gradesList.size
        id -> (suma / ilosc)
      }

    val wynik: Map[String, Double] = dane.students.flatMap { student =>
      sredniePoId.get(student.id).map { srednia =>
        student.name -> srednia
      }
    }.toMap

    write(AverageGradeResponse(wynik))
  }

//=====================================================================================

  @cask.post("/safeAppend")
  def safeAppend(): ujson.Value = {
    val dane = read_json[SafeAppendIN]("zad.3.0.json")
    val wynik = if (dane.index < 0 || dane.index > dane.list.length) None
                else {
                  val (p, k) = dane.list.splitAt(dane.index)
                  Some(p ++ (dane.element :: k))
                }
    write(OptionIntListResponse(wynik))
  }

  @cask.post("/variance")
  def variance(): ujson.Value = {
    val dane = read_json[VarianceIN]("zad.3.5.json")
    def mean(xs: Seq[Double]) = if (xs.isEmpty) None else Some(xs.sum / xs.length)
    val wynik = mean(dane.list).flatMap { m => mean(dane.list.map(x => math.pow(x - m, 2))) }
    write(OptionDoubleResponse(wynik))
  }

  // @cask.post("/concatOption")
  // def concatOption(): ujson.Value = {
  //   val dane = read_json[TwoOptionsIN]("zad.4.0.json")
  //   val wynik = for { l1 <- dane.list1; l2 <- dane.list2 } yield l1 ++ l2
  //   write(OptionIntListResponse(wynik))
  // }

  // @cask.post("/mojeMap")
  // def endpointMojeMap(): ujson.Value = {
  //   val dane = read_json[TwoOptionsIN]("zad.4.5.json")
  //   def mojeMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))
  //   val wynik = mojeMap(dane.list1, dane.list2)((l1, l2) => l1 ++ l2)
  //   write(OptionIntListResponse(wynik))
  // }

  @cask.post("/meanEither")
  def meanEither(): ujson.Value = {
    val dane = read_json[MeanIN]("zad.5.0.json")
    val wynik = if (dane.list.isEmpty) Left("Lista jest pusta") else Right(dane.list.sum / dane.list.length)
    val response = wynik match {
      case Right(v) => EitherResponse(true, v.toString)
      case Left(e)  => EitherResponse(false, e)
    }
    write(response)
  }

  initialize()
}