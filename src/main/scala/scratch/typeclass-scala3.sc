
  case class Person(name: String, age: Int)

  // part 1 - Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances
  given stringSerialize: JSONSerializer[String] with {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  given intSerializer: JSONSerializer[Int] with {
    override def toJson(value: Int): String = value.toString
  }

  given personSerializer: JSONSerializer[Person] with {
    override def toJson(person: Person): String = {
      s"""
         |{"name": "${person.name}", "age": ${person.age}}
         |""".stripMargin.trim
    }
  }

  // part 3 - user-facing API
  def convert2Json[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertList2Json[T](list: List[T])(using serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  // part 4 - extension methods just for the types we support
  extension [T](value: T)
    def toJson(using serializer: JSONSerializer[T]): String =
      serializer.toJson(value)


  println(convertList2Json(List(Person("Alice", 23), Person("Bob", 46))))
  val bob = Person("Bob", 46)
  println(bob.toJson)