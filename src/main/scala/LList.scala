import scala.collection.immutable.List
case class LList[T](val head: Option[Node[T]] = None) {

  override def toString(): String = {
    val listContent = this.head match {
      case Some(headNode) => headNode.toString();
      case None           => "";
    };
    s"LList: [$listContent]"
  }

  def pushLeft(value: T): LList[T] = {
    val newHead = Node(value, this.head)

    val newList =LList[T](Some(newHead))

    newList
  }

  def headTail: (Option[T],LList[T]) = {
    this.head match {
      case Some(head_node) => (Some(head_node.current),LList[T](head_node.next))
      case None            => (None,LList[T]())
    }
  }

  def isEmpty: Boolean = {
    this.head.isEmpty
  }

  def len: Int = {
    def lenRec(list:LList[T], acc: Int): Int = {
      if list.isEmpty then{
        return acc
      }
      val (_head, tail) = list.headTail
      lenRec(tail, acc + 1)
    }
    lenRec(this, 0)
  }

  def toList: List[T] = {
    def toListRec(list:LList[T], acc: List[T]): List[T] = {
      if list.isEmpty then{
        return acc
      }
      val (Some(head), tail) = list.headTail: @unchecked;
      val newAcc = head :: acc 
      toListRec(tail, newAcc).reverse
    }
    toListRec(this, List())
  }
  
  def ::(value: T):LList[T] = {
    val newHead = Node(value, this.head)
    val newList =LList[T](Some(newHead))
    newList
  }

  def reverse:LList[T] = {
    def reverseRec(list:LList[T], acc:LList[T]):LList[T] = {
      if list.isEmpty then {
        return acc;
      }
      val (Some(head), tail) = list.headTail: @unchecked;
      val newAcc = head :: acc
      reverseRec(tail, newAcc)
    }

    reverseRec(this,LList[T]())
  }

}

case class Node[T](val current: T, val next: Option[Node[T]]) {

  override def toString(): String = {
    s"${this.current}" + (this.next match {
      case Some(v) => ", " + v.toString();
      case None    => "";
    })
  }
}
