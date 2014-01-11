import scala.reflect.ClassTag
import scala.util.Try
import scala.runtime.ScalaRunTime._

protected object VecUtil {
  val branchingFactor = 32
  val mask = branchingFactor - 1

  def levelOffset(index: Int) = (Math.log(index) / Math.log(branchingFactor)).toInt
  def bitsPerMask = (Math.log(branchingFactor) / Math.log(2)).toInt
}
import VecUtil._

trait VecTrait[A] {
  override def toString: String
  def length: Int

  def append(v: A): VecTrait[A]
  def get(i: Int): A
  def updated(i: Int, v: A): VecTrait[A]
}

class NodeVec[A : ClassTag](start: Int, end: Int, val array: Array[VecTrait[A]]) extends VecTrait[A] {
  override def toString = stringOf(array)
  //private val array = new Array[Vec[A]](branchingFactor) //TODO how does this get set?

  private def localIndex(i: Int) = (i >> bitsPerMask) & mask //TODO this will need to be level aware
  def length = end - start

  def append(v: A) =
    if (localIndex(length) < array.length)
        new NodeVec(start, end + 1, array.updated(localIndex(length), array(localIndex(length)).append(v)))
    else
        new NodeVec(start, end + 1, array.:+(new LeafVec(Array(v))))

  def get(i: Int) = array(localIndex(i)).get(i)

  def updated(i: Int, value: A) = array(localIndex(i)).updated(i: Int, value: A)
}

class LeafVec[A : ClassTag](val array: Array[A]) extends VecTrait[A] {// extends IndexedSeq[A] {
  override def toString = stringOf(array)
  def length = array.length

  def append(v: A) = new LeafVec[A](array.:+(v).toArray)

  def get(i: Int) = array(i & mask)

  def updated(i: Int, value: A) = new LeafVec(array.updated(i, value))
}


object Vec {



  def main(args: Array[String]) {
    println("levelOffset: " + (levelOffset(31) == 0))
    println("levelOffset: " + (levelOffset(32) == 1))
    println("levelOffset: " + (levelOffset(32*32 - 1) == 1))
    println("levelOffset: " + (levelOffset(32*32) == 2))
    println("bitsPerMask: " + (bitsPerMask == 5))

    val vec = new NodeVec[Int](0, 0, Array(new LeafVec(Array())))
    println("len1: "+(vec.length==0))
    val vec2 = vec.append(17)
    println("len1: "+(vec.length == 0))
    println("len2: "+(vec2.length == 1))
    println("vec(0): "+(Try(vec.get(0)).toOption == None))
    println("vec2(0): "+ (vec2.get(0) == 17))
    val vec3 = vec2.updated(0, 18)
    println("vec3.len: "+(vec3.length == 1))
    println("vec3(0): "+(vec3.get(0) == 18))
    println("vec2(0): "+(vec2.get(0) == 17))
    
    val vec33 = (0 to 32).foldLeft(vec) { (vec, i) => vec.append(i) }
    println("vec33.len: " + (vec33.length == 33))
    println("vec33(0): " + (vec33.get(0) == 0))
    println("vec33(31): " + (vec33.get(31) == 31))
    println("vec33(32): " + (vec33.get(32) == 32))

  }
}
