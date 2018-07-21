package tensor

// TensorComprehensions: everything is scalar, but dimensions not quantified

// LGen: everyting is a matrix, tiled into submatrixes [A]r,c --> tiled matrix where every element is an r x c matrix
/*

(00 01 02 03)
(10 11 12 13)
(10 11 12 13)
(10 11 12 13) 4x4 row x col

tile row -> 2

(0.00 0.01 0.02 0.03)
(0.10 0.11 0.12 0.13)

(1.00 1.01 1.02 1.03)
(1.10 1.11 1.12 1.13) (2x2)x4 (row1 x row2) x col

tile col -> 2

((00.00 00.01) (01.00 01.01))
((00.10 00.11) (01.10 01.11))
((10.00 10.01) (11.00 11.01))
((10.10 10.11) (11.10 11.11))  2x2x2x2 (row1 x col1) x (row2 x col2)

Halide: split and reorder!

*/

// This file: inspired by TC, but trying to simplify (pure functional, explicit reduction vars, ...)


object Test1 {
  def main(args: Array[String]): Unit = {

    abstract class Index

    abstract class Tensor {
      def apply(xs: Index*) = Apply(this, xs.toList)
    }

    abstract class Scalar {
      def +(that: Scalar): Scalar = Plus(this,that)
      def *(that: Scalar): Scalar = Times(this,that)
    }

    case class Var(s: String) extends Index { override def toString = s }
    case class Input(s: String, shape: List[Int]) extends Tensor
    case class Apply(x: Tensor, y: List[Index]) extends Scalar
    case class Func(s: String, ins: List[Var], red: List[Var], body: Scalar) extends Tensor // red + implicit

    case class Const(x: Double) extends Scalar
    case class Plus(x: Scalar, y: Scalar) extends Scalar
    case class Times(x: Scalar, y: Scalar) extends Scalar


    // example

    val i = Var("i")
    val j = Var("j")
    val k = Var("k")

    val A = Input("A", List(30,70))
    val B = Input("B", List(70,50))

    val C = Func("C", List(i,j), List(k), A(i,k) * B(k,j))


    // infer tensor shapes and variable ranges

    val shapes = Map(A -> A.shape, B -> B.shape, C -> List(A.shape(0), B.shape(1)))

    val storage = Map(A -> A.shape, B -> B.shape, C -> List(A.shape(0), B.shape(1))) // may be larger than shape!

    val ranges = Map(i -> shapes(A)(0), j -> shapes(B)(1), k -> shapes(A)(1))

    val inputs = List(A,B)

    val outputs = List(C)

    // generate code

    def indexOffset(storage: List[Int], idx: List[Index]): String = (storage, idx) match {
      case (_::Nil, i::Nil) => s"$i"
      case (s::ss,  i::is)  => s"$i*$s + ${indexOffset(ss,is)}"
    }

    def str(e: Scalar): String = e match {
      case Const(x)   => x.toString
      case Plus(x,y)  => s"(${ str(x) } + ${ str(y) })"
      case Times(x,y) => s"(${ str(x) } * ${ str(y) })"
      case Apply(x@Input(s,_),is) => s"$s[${indexOffset(storage(C),is)}]" // TODO: don't rely on Input
    }

    println(s"void kernel(double *A, double *B) {")
    println(s"double *C = (double*)malloc(${storage(C).product}*8);")

    for (x <- C.ins) {
      println(s"for (int $x; $x < ${ranges(x)}; ${x}++) {")
    }

    val acc = s"${C.s}_${C.ins.mkString("_")}"
    println(s"double $acc = 0.0;")

    for (x <- C.red) {
      println(s"for (int $x; $x < ${ranges(x)}; ${x}++) {")
    }

    print(s"$acc += ${ str(C.body) }")

    for (x <- C.red) {
      println(s"}")
    }

    println(s"${C.s}[${indexOffset(shapes(C),C.ins)}] = $acc;")

    for (x <- C.ins) {
      println(s"}")
    }


    println(s"}")


    println(C)

    println("done")
  }
}