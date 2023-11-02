import scala.collection.immutable.List
case class LList(val head: Option[Node] = None) {
  // val head: Node

  // def new(): LList{
  //   new LList()
  // }

  override def toString(): String = {
    val listContent = this.head match {
      case Some(headNode) => headNode.toString();
      case None           => "";
    };
    s"LList: [$listContent]"
  }

  def pushLeft(value: Int): LList = {
    val newHead = Node(value, this.head)

    val newList = LList(Some(newHead))

    newList
  }

  def headTail: (Option[Int], LList) = {
    this.head match {
      case Some(head_node) => (Some(head_node.current), LList(head_node.next))
      case None            => (None, LList())
    }
  }

  def isEmpty: Boolean = {
    this.head.isEmpty
  }

  def len: Int = {
    def lenRec(list: LList, acc: Int): Int = {
      if list.isEmpty then{
        return acc
      }
      val (_head, tail) = list.headTail
      lenRec(tail, acc + 1)
    }
    lenRec(this, 0)
  }

  def toList: List[Int] = {
    def toListRec(list: LList, acc: List[Int]): List[Int] = {
      if list.isEmpty then{
        return acc
      }
      val (Some(head), tail) = list.headTail: @unchecked;
      val newAcc = head :: acc 
      toListRec(tail, newAcc).reverse
    }
    toListRec(this, List())
  }
  
  def ::(value: Int): LList = {
    val newHead = Node(value, this.head)
    val newList = LList(Some(newHead))
    newList
  }

  def reverse: LList = {
    def reverseRec(list: LList, acc: LList): LList = {
      if list.isEmpty then {
        return acc;
      }
      val (Some(head), tail) = list.headTail: @unchecked;
      val newAcc = head :: acc
      reverseRec(tail, newAcc)
    }

    reverseRec(this, LList())
  }

}

case class Node(val current: Int, val next: Option[Node]) {
  // val current: Int
  // val next: Node

  // def apply()

  override def toString(): String = {
    s"${this.current}" + (this.next match {
      case Some(v) => ", " + v.toString();
      case None    => "";
    })
  }
}
