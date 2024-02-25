import scala.util.Random

type Station = String
type Price = Int

case class Booking(from: Station, to: Station, seats: Int)

case class Segment(from: Station, to: Station, price: Price, availableSeats: Int)

case class Route(from: Station, to: Station, segments: List[Segment]) {
  def getPrice(from: Station, to: Station): Price = {
    segments.foldLeft((false, 0)) { case ((start, acc), s) =>
      if(s.from == from) (true, s.price)
      else if(s.to == to) (false, acc + s.price)
      else if(start) (start, acc + s.price)
      else (false, acc)
    }._2
  }

  def getAvailableSeats(from: Station, to: Station): Int = {
    val segment = segments.find(segment => segment.from == from && segment.to == to)
    segment match {
      case Some(value) => value.availableSeats
      case None => throw new Exception("No such segment")
    }
  }
}


val route = Route(
  from = "Lviv",
  to = "Kyiv",
  segments = List(
    Segment("Lviv", "Ternopil", 10, 10),
    Segment("Ternopil", "Rivne", 10, 10),
    Segment("Rivne", "Zhytomyr", 10, 10),
    Segment("Zhytomyr", "Kyiv", 10, 10))
)

println(route.getPrice("Lviv", "Zhytomyr"))
