import scala.collection.immutable.List
import scala.collection.AbstractSeq

case class LListIterator[T](var list: LList[T]) extends Iterator[T] {
  override def hasNext: Boolean = {
    this.list.isEmpty == false
  }

  override def next(): T = {
    val Some(current_head) = this.list._head: @unchecked;
    val res = current_head.current
    val newHead = current_head.next
    this.list = LList[T](newHead)
    res
  }
}

object LList {
  // these two apply has to be implemented for the apply with args: T* to work correctly without self calling
  def apply[T](): LList[T] = {
    LList[T](None)
  }
  def apply[T](arg: T): LList[T] = {
    LList().pushLeft(arg)
  }
  def apply[T](args: T*): LList[T] = {

    def applyRec(args: List[T], acc: LList[T]): LList[T] = {
      if args.isEmpty then {
        return acc
      }
      val newAcc = args(0) :: acc
      applyRec(args.tail, newAcc)
    }
    applyRec(args.toList, LList()).reverse
  }

}

case class LList[T](val _head: Option[Node[T]] = None)
    extends AbstractSeq[T]
    with Iterable[T] {

  override def apply(i: Int): T = {
    // return this.iterator.
    this.getElementAt(i)
  }

  def length: Int = {
    this.len
  }

  private def len: Int = {
    def lenRec(list: LList[T], acc: Int): Int = {
      if list.isEmpty then {
        return acc
      }
      val (_head, tail) = list.headTail
      lenRec(tail, acc + 1)
    }
    lenRec(this, 0)
  }

  private def getElementAt(i: Int): T = {
    def getElementAtRec(i: Int, list: LList[T], counter: Int): T = {
      val (Some(head), tail) = list.headTail: @unchecked;
      if i == counter then {
        return head
      }
      getElementAtRec(i, tail, counter + 1)
    }
    getElementAtRec(i, this, 0)
  }

  override def iterator: Iterator[T] = {
    LListIterator(this)
  }

  override def toString(): String = {
    val listContent = this._head match {
      case Some(headNode) => headNode.toString();
      case None           => "";
    };
    s"LList: [$listContent]"
  }

  def pushLeft(value: T): LList[T] = {
    val newHead = Node(value, this._head)

    val newList = LList[T](Some(newHead))

    newList
  }

  def headTail: (Option[T], LList[T]) = {
    this._head match {
      case Some(head_node) =>
        (Some(head_node.current), LList[T](head_node.next))
      case None => (None, LList[T]())
    }
  }

  override def isEmpty: Boolean = {
    this._head.isEmpty
  }

  override def head: T = {
    val (Some(head), _tail) = this.headTail: @unchecked;
    head
  }

  def ::(value: T): LList[T] = {
    val newHead = Node(value, this._head)
    val newList = LList[T](Some(newHead))
    newList
  }

  override def reverse: LList[T] = {
    def reverseRec(list: LList[T], acc: LList[T]): LList[T] = {
      if list.isEmpty then {
        return acc;
      }
      val (Some(head), tail) = list.headTail: @unchecked;
      val newAcc = head :: acc
      reverseRec(tail, newAcc)
    }

    reverseRec(this, LList[T]())
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
