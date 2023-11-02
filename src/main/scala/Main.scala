@main def hello: Unit = {

  println("Hello world!")
  val list = LList[Int]()
  println(s"${list}")
  val list1 = list.pushLeft(1).pushLeft(2).pushLeft(3);
  val list2 = list1.pushLeft(4);
  println(s"${list1}")
  println(s"${list2}")
  val (head, tail) = list2.headTail
  println(s"head: $head, tail: $tail")

}
