import scala.collection.immutable.List
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("LList.pushLeft works") {
    val list = LList().pushLeft(123);
    val (Some(head), tail) = list.headTail: @unchecked;
    assertEquals(head, 123);
    assertEquals(tail, LList())
  }

  test("LList.pushLeft works chained") {

    val list = LList().pushLeft(1).pushLeft(2).pushLeft(3);
    val (Some(head3), tail3) = list.headTail: @unchecked;
    assertEquals(head3, 3);

    val (Some(head2), tail2) = tail3.headTail: @unchecked;
    assertEquals(head2, 2);

    val (Some(head1), tail1) = tail2.headTail: @unchecked;
    assertEquals(head1, 1);

    val (None, emptyTail) = tail1.headTail: @unchecked;

    assertEquals(emptyTail, LList())
  }

  test("LList.isEmpty works when list is empty") {
    val list = LList();
    val expected = true;
    val res = list.isEmpty
    assertEquals(res, expected)
  }

  test("LList.isEmpty works when list is not empty") {
    val list = LList().pushLeft(13);
    val expected = false;
    val res = list.isEmpty
    assertEquals(res, expected)
  }

  test("Llist.size works when list is empty") {
    val list = LList();
    val expected = 0;
    val res = list.size
    assertEquals(res, expected)
  }

  test("Llist.size works when list is not empty") {
    val list = LList().pushLeft(1).pushLeft(2).pushLeft(3);
    val expected = 3;
    val res = list.size
    assertEquals(res, expected)
  }

  test("LList.toList works when list is empty") {
    val list = LList();
    val expected = List();
    val res = list.toList
    assertEquals(res, expected)
  }

  test("LList.toList works when list is not empty") {
    val list = LList().pushLeft(1).pushLeft(2).pushLeft(3);
    val expected = List(3, 2, 1);
    val res = list.toList
    assertEquals(res, expected)
  }

  test("element :: list works as pushLeft") {
    val list = LList[Int]();
    val res = 123 :: list;
    val expected = LList().pushLeft(123);
    assertEquals(res, expected)
  }

  test("element :: list works as pushLeft on String") {
    val list = LList[String]();
    val res = "123" :: list;
    val expected = LList().pushLeft("123");
    assertEquals(res, expected)
  }

  test("list.reverse works when list is empty") {
    val list = LList();
    val res = list.reverse;
    assertEquals(res, list);
  }

  test("list.reverse works when list is not empty") {
    val list = LList().pushLeft(1).pushLeft(2).pushLeft(3);
    val res = list.reverse;
    val expected = LList().pushLeft(3).pushLeft(2).pushLeft(1);
    assertEquals(res, expected);
  }

  test("no test") {
    val list: Iterable[Int] = List[Int]();
    // list.iterator
    // list.toIterable
    assert(true)
  }

  test("List to iterable") {
    // val list: Iterable[Int] = List[Int]();
    val iterable = (123 :: LList()).pushLeft(99).pushLeft(3).toIterable;
    val expected = "399123";
    val res = iterable.mkString;
    assertEquals(res, expected)
  }

  test("List can contain different types") {
    val list = LList().pushLeft("hello").pushLeft(123).pushLeft(66.32)
    println(list)
    assert(true)
  }

  test("list can be created using variable number of args") {
    val list = LList(1, 2, 3);
    val expected = LList().pushLeft(3).pushLeft(2).pushLeft(1);
    assertEquals(list, expected)
  }

  test("get element at index") {
    val list = LList(1, 2, 3);
    val expected = 3;
    val res = list(2);
    assertEquals(res, expected)
  }

}
