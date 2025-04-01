object LemScalaIndent {
  def main(args: Array[String]): Unit = {
    for (i <- 0 until 100) {
      Option(i)
        .map(_ + 1)
        .map(_ % 2)

      val oneTwo: String = {
        val one = "one"
        val two = "two"
        s"$one$two"
      }

      trait Human[T] {
        val age: Int
      }

      trait Greeter {
        def sayHello: Unit
      }

      case class Person(
          val name: String,
          surname: String,
          override val age: Int
      ) extends Human[Person]
      with Greeter {
        override def sayHello: Unit = println(s"Hello, I'm $name! Nice to meet you")
      }

      val person = Person(
        name = "vasya",
        surname = "pupkin",
        age = 24
      )

      person match {
        case Person(name, surname, age) =>
          println(s"Person name: $name surname: $surname")
        case _ =>
          throw new RuntimeException("Unrecognizable person")
      }

      if (person.name == "vasya") {
        println("Vasya is hear")
      } else {
        println("Vasya is lost")
      }
    }
  }
}
