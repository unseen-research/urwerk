package urwerk.cli

import urwerk.test.TestBase

import com.monovore.decline._
import cats.implicits._
import java.net.URI
import scala.concurrent.duration.Duration
import scala.deriving.Mirror
import org.checkerframework.checker.units.qual.A

class DeclineTest extends TestBase:

  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  trait FieldEncoder[A]:
    def encodeField(a: A): String

  type Row = List[String]

  trait RowEncoder[A]:
    def encodeRow(a: A): Row


  object BaseEncoders:
    given FieldEncoder[Int] with
      def encodeField(x: Int) = x.toString

    given FieldEncoder[Boolean] with
      def encodeField(x: Boolean) = if x then "true" else "false"

    given FieldEncoder[String] with
      def encodeField(x: String) = x // Ideally, we should also escape commas and double quotes
  end BaseEncoders    


  object TupleEncoders:
    // Base case
    given RowEncoder[EmptyTuple] with
      def encodeRow(empty: EmptyTuple) =
        List.empty

    // Inductive case
    given [H: FieldEncoder, T <: Tuple: RowEncoder]: RowEncoder[H *: T] with
      def encodeRow(tuple: H *: T) =
        summon[FieldEncoder[H]].encodeField(tuple.head) :: summon[RowEncoder[T]].encodeRow(tuple.tail)
  end TupleEncoders


  def tupleToCsv[X <: Tuple : RowEncoder](tuple: X): List[String] =
    summon[RowEncoder[X]].encodeRow(tuple)


  "main" in {

    val bob: Employee = Employee("Bob", 42, false)
    val bobTuple: (String, Int, Boolean) = Tuple.fromProductTyped(bob)
    println(s"TUPLE $bobTuple")
    val bobAgain: Employee = summon[Mirror.Of[Employee]].fromProduct(bobTuple)
    println(s"BobAGAIN $bobAgain")

  }
  
  "elem labels" in {
    case class A(int: Int, string: String)

    import scala.deriving.Mirror
    import scala.compiletime.summonAll

    val mirror = summon[Mirror.Of[A]]    
    
    type ValueOfs = Tuple.Map[mirror.MirroredElemLabels, ValueOf]
    
    val valueOfs = summonAll[ValueOfs]

    def values(t: Tuple): Tuple = t match
      case (h: ValueOf[_]) *: t1 => h.value *: values(t1)
      case EmptyTuple => EmptyTuple

    val x = values(valueOfs) // (i,s)

    println(s"Lables $x")
  }

  trait A
  given A with {
    override def toString="an A"
  }

  trait B
  given B with {
    override def toString="a  B"
  }

  trait C
  given C with {
    override def toString="a  C"
  }

  "summon all" in {
    import scala.compiletime.summonAll

    val x = summonAll[(A, B, C)]

    x.toList.foreach{elem=>
      println(s"Elem $elem")   
    }

    // Command[Config](
    //   //"a".help("")

    // )


  }