package part1

object TestCafe extends App {
  val cafe1 = new Cafe
  val credit = CreditCard()
  println(cafe1.buyCoffee(credit)._2.amount)
  println(cafe1.buyCoffees(credit, 10)._2.amount)
}

class Cafe {

  case class Coffee(price: Double = 120.15)

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] =
      List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList
}

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can combine charges to different cards")
}

case class CreditCard()