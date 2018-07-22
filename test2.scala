package tensor

// This file: Halide in Scala

object Test2 {
    import scala.reflect.ClassTag

    type UByte = Int

    abstract class Type[T] {
      def classTag: ClassTag[T]
      def fromByte(d: Byte): T
      def fromInt(d: Int): T
      def fromDouble(d: Double): T
      def sin(a: T): T
      def plus(a: T, b: T): T
      def minus(a: T, b: T): T
      def times(a: T, b: T): T
      def div(a: T, b: T): T
      def min(a: T, b: T): T
    }

    implicit object IntTyp extends Type[Int] {
      def classTag = ClassTag.Int
      def fromByte(d: Byte) = d
      def fromInt(d: Int) = d
      def fromDouble(d: Double) = d.toInt
      def sin(a: Int) = math.sin(a).toInt
      def plus(a: Int, b: Int) = a + b
      def minus(a: Int, b: Int) = a - b
      def times(a: Int, b: Int) = a * b
      def div(a: Int, b: Int) = a / b
      def min(a: Int, b: Int) = a min b
    }

    implicit object DoubleTyp extends Type[Double] {
      def classTag = ClassTag.Double
      def fromByte(d: Byte) = d
      def fromInt(d: Int) = d
      def fromDouble(d: Double) = d
      def sin(d: Double) = math.sin(d)
      def plus(a: Double, b: Double) = a + b
      def minus(a: Double, b: Double) = a - b
      def times(a: Double, b: Double) = a * b
      def div(a: Double, b: Double) = a / b
      def min(a: Double, b: Double) = a min b
    }

    implicit object ByteTyp extends Type[Byte] {
      def classTag = ClassTag.Byte
      def fromByte(d: Byte) = d
      def fromInt(d: Int) = d.toByte
      def fromDouble(d: Double) = d.toByte
      def sin(d: Byte) = math.sin(d).toByte
      def plus(a: Byte, b: Byte) = (a + b).toByte
      def minus(a: Byte, b: Byte) = (a + b).toByte
      def times(a: Byte, b: Byte) = (a * b).toByte
      def div(a: Byte, b: Byte) = (a * b).toByte
      def min(a: Byte, b: Byte) = (a min b).toByte
    }

    implicit def extractClassTag[T](implicit tpe: Type[T]): ClassTag[T] = tpe.classTag

    implicit def doubleExpr(d: Double): Expr = Const(d)

    abstract class Expr {
      def +(that: Expr): Expr = Plus(this,that)
      def -(that: Expr): Expr = Minus(this,that)
      def *(that: Expr): Expr = Times(this,that)
      def /(that: Expr): Expr = Div(this,that)
      def toDouble: Expr = ToDouble(this)
      def toInt: Expr = ToInt(this)
      def toByte: Expr = ToByte(this)
    }

    def min(a: Expr, b: Expr): Expr = Min(a,b)
    def sin(a: Expr): Expr = Sin(a)

    case class Var(s: String) extends Expr { override def toString = s }
    case class Apply(x: Func, y: List[Expr]) extends Expr
    case class Apply3(f: Buffer3[_], x: Expr, y: Expr, c: Expr) extends Expr

    case class Const(x: Double) extends Expr
    case class Plus(x: Expr, y: Expr) extends Expr
    case class Minus(x: Expr, y: Expr) extends Expr
    case class Times(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr
    case class Min(x: Expr, y: Expr) extends Expr
    case class Sin(x: Expr) extends Expr
    case class ToDouble(x: Expr) extends Expr
    case class ToInt(x: Expr) extends Expr
    case class ToByte(x: Expr) extends Expr

    type Env = Map[String,Int]
    type FEnv = Map[String,Buffer[_]]

    def eval[T](g: Expr)(implicit tpe: Type[T], env: Env, fenv: FEnv): T = g match {
      case Const(c)   => tpe.fromDouble(c)
      case Var(s)     => tpe.fromInt(env(s))
      case Plus(a,b)  => tpe.plus(eval[T](a), eval[T](b))
      case Minus(a,b) => tpe.minus(eval[T](a), eval[T](b))
      case Times(a,b) => tpe.times(eval[T](a), eval[T](b))
      case Div(a,b)   => tpe.div(eval[T](a), eval[T](b))
      case Min(a,b)   => tpe.min(eval[T](a), eval[T](b))
      case Sin(a)     => tpe.sin(eval[T](a))
      case ToDouble(a)=> tpe.fromDouble(eval[Double](a))
      case ToInt(a)   => tpe.fromInt(eval[Int](a))
      case ToByte(a)  => tpe.fromByte(eval[Byte](a))
      case Apply(f,as)=> 
        val List(x,y) = as.map(eval[Int]) // FIXME: only 2
        if (f.computeRoot) {
          val buf = fenv(f.s).asInstanceOf[Buffer[T]]
          buf(x,y)
        } else {
          val env1: Env = Map(f.impl.x.s -> x, f.impl.y.s -> y)
          eval[T](f.impl.body)(tpe, env1, fenv)
        }
      case Apply3(f,a,b,c) => tpe.fromInt(f(eval[Int](a), eval[Int](b), eval[Int](c)).asInstanceOf[UByte]) // assume byte
    }

    // generic pre-order traversal
    def traverse(x: Any)(f: Any => Unit): Unit = { f(x); x match {
      case x: Product => x.productIterator.foreach(x => traverse(x)(f))
      case _ =>
    }}

    case class FuncInternal(x: Var, y: Var, body: Expr)

    case class Func(s: String) {
      var impl: FuncInternal = _
      var order: List[String] = _

      var trace = false
      var computeRoot = false

      var computeBounds: List[(String, Env => Int)] = _
      var computeVar: List[(String, (Env,Env) => Int)] = _

      var vectorized: List[String] = _
      var unrolled: List[String] = _
      var parallel: List[String] = _

      def apply(es: Expr*): Expr = 
        Apply(this, es.toList)

      def update(x: Var, y: Var, body: Expr): Unit = {
        impl = FuncInternal(x,y,body)
        order = List(y.s,x.s)
        computeBounds = Nil
        computeVar = Nil
        vectorized = Nil
        unrolled = Nil
        parallel = Nil
      }

      def reorder(xs: Var*): Unit = {
        assert(true) // todo: sanity checks! must be permutation
        //order = xs.map(_.s).toList.reverse // note: reverse!
        val ss = xs.map(_.s).reverse
        val is = ss.iterator
        order = order.map(s => if (ss contains s) is.next else s)
      }

      def split(x: Var, x_outer: Var, x_inner: Var, factor: Int): Unit = {
        assert(true) // todo: sanity checks! outer/inner fresh
        order = order.flatMap(s => if (s == x.s) List(x_outer.s, x_inner.s) else List(s))
        computeBounds :+= ((x_outer.s, (bounds: Env) => { assert(bounds(x.s) % factor == 0); bounds(x.s) / factor })) // assert evenly divisible!!
        computeBounds :+= ((x_inner.s, (bounds: Env) => factor))
        computeVar    ::= ((x.s, (env:Env,bounds:Env) => env(x_outer.s) * factor + env(x_inner.s)))
      }

      def fuse(x: Var, y: Var, fused: Var): Unit = {
        assert(true) // todo: sanity checks! fused fresh
        // Q: which position? need to be adjacent?
        // currently we assume y is outer, and we fuse into that position
        order = order.flatMap(s => if (s == y.s) List(fused.s) else if (s == x.s) List() else List(s))
        computeBounds :+= ((fused.s, (bounds: Env) => bounds(x.s) * bounds(y.s)))
        computeVar    ::= ((x.s, (env:Env,bounds:Env) => env(fused.s) % bounds(x.s)))
        computeVar    ::= ((y.s, (env:Env,bounds:Env) => env(fused.s) / bounds(x.s)))
      }

      def tile(x: Var, y: Var, x_outer: Var, y_outer: Var, x_inner: Var, y_inner: Var, y_factor: Int, x_factor: Int) = {
        split(x, x_outer, x_inner, x_factor)
        split(y, y_outer, y_inner, y_factor)
        reorder(x_inner, y_inner, x_outer, y_outer)
      }

      def vectorize(x: Var): Unit = {
        assert(true) // what to check?
        // currently a no-op
        vectorized :+= x.s
      }
      def unroll(x: Var): Unit = {
        assert(true) // what to check?
        // currently a no-op
        unrolled :+= x.s
      }
      def parallel(x: Var): Unit = {
        assert(true) // what to check?
        // currently a no-op
        parallel :+= x.s
      }

      def trace_stores() = trace = true

      def compute_root(): Unit = {
        computeRoot = true
      }

      type Stencil = (Int,Int)

      def getAllStages: List[(Func, Stencil, Stencil)] = {

        var fs: List[(Func, Stencil, Stencil)] = Nil

        def stencil(v: Var, e: Expr): Stencil = e match {
          case `v` => (0,0)
          case Plus(`v`, Const(a))  => (a.toInt,a.toInt)
          case Minus(`v`, Const(a)) => (-a.toInt,-a.toInt)
        }

        def merge(a: Stencil, b: Stencil): Stencil = (a._1 min b._1, a._2 max b._2)

        def traverseFun(f: Func, sx: Stencil, sy: Stencil): Unit = {
          fs ::= (f,sx,sy)
          traverse(f.impl.body) {
            case Apply(f1@Func(s), List(x,y)) =>  // FIXME: 2
              val sx1 = stencil(f.impl.x, x)
              val sy1 = stencil(f.impl.y, y)
              if (f1.computeRoot)
                traverseFun(f1, sx1, sy1)
              // XXX TODO: traverse body if not computeRoot, widen stencil
            case _ => 
          }
        }
        traverseFun(this, (0,0), (0,0))
        // XXX
        val gx = fs.groupBy(_._1).map(p => (p._1, p._2.map(_._2).reduce(merge)))
        val gy = fs.groupBy(_._1).map(p => (p._1, p._2.map(_._3).reduce(merge)))
        fs.map(_._1).distinct.map(k => (k,gx(k),gy(k)))
      }

      def realize[T:Type](w: Int, h: Int): Buffer[T] = {
        val fs = getAllStages
        implicit var fenv = Map[String, Buffer[_]]()
        fs.foreach { p => 
          val (f,(lx,hx),(ly,hy)) = p
          assert(lx == 0) // TODO: support negative stencils
          assert(ly == 0)
          fenv += (f.s -> f.realize_internal[T](w+hx, h+hy))
        }
        fenv(s).asInstanceOf[Buffer[T]]
      }

      def realize_internal[T:Type](w: Int, h: Int)(implicit fenv: FEnv): Buffer[T] = {
        val buf = new Buffer[T](w, h)
        var bounds = Map(impl.x.s -> w, impl.y.s -> h)

        for ((s,f) <- computeBounds)
          bounds += (s -> f(bounds))

        def loop(vs: List[String], env: Env)(f: Env => Unit): Unit = vs match {
          case Nil => f(env)
          case x::vs => for (i <- 0 until bounds(x)) loop(vs, env + (x -> i))(f)
        }

        // todo: vectorize/unroll/parallel are currently no-ops for execution

        loop(order, Map()) { e0 =>
          implicit var env = e0
          for ((s,f) <- computeVar)
            env += (s -> f(env,bounds))
          val i = env(impl.x.s)
          val j = env(impl.y.s)
          if (trace) println(s"$s $i,$j")
          buf(i,j) = eval[T](impl.body)
        }
        buf
      }

      def print_loop_nest(): Unit = {
        val fs = getAllStages
        println("stages: " + fs.mkString(", "))
        fs.foreach(_._1.print_loop_nest_internal)
      }

      def print_loop_nest_internal(): Unit = {
        for ((s,_) <- computeBounds)
          println(s"val max_${s} = ...")
        for (x <- order)
          if (unrolled contains x)
            println(s"for ${x} (unrolled):")
          else if (vectorized contains x)
            println(s"for ${x} (vectorized):")
          else if (parallel contains x)
            println(s"for ${x} (parallel):")
          else
            println(s"for ${x}:")
        for ((s,_) <- computeVar)
          println(s"val ${s} = ...")
        println(s"$s(...) = ...")
      }

    }

    case class Func3Internal(x: Var, y: Var, c: Var, body: Expr)

    case class Func3() {
      var impl: Func3Internal = _

      def update(x: Var, y: Var, c: Var, body: Expr): Unit = {
        this.impl = Func3Internal(x,y,c,body)
      }

      def realize[T:Type](w: Int, h: Int, d: Int): Buffer3[T] = {
        val buf = new Buffer3[T](w, h, d)
        for (j <- 0 until h; i <- 0 until w; c <- 0 until d) {
          implicit val env = Map(impl.x.s -> i, impl.y.s -> j, impl.c.s -> c)
          implicit val fenv = Map(): FEnv
          buf(i,j,c) = eval[T](impl.body)
        }
        buf
      }
    }

    class Buffer[T:Type](val width: Int, val height: Int) {
      val data: Array[Array[T]] = Array.ofDim[T](height, width)
      def apply(x: Int, y: Int) = data(y)(x)
      def update(x: Int, y: Int, v: T) = data(y)(x) = v
    }

    class Buffer3[T:Type](val width: Int, val height: Int, val channels: Int) {
      val data: Array[Array[Array[T]]] = Array.ofDim[T](height, width, channels)
      def apply(x: Int, y: Int, c: Int) = data(y)(x)(c)
      def update(x: Int, y: Int, c: Int, v: T) = data(y)(x)(c) = v

      def apply(x: Var, y: Var, c: Var) = Apply3(this,x,y,c)
    }

    def load_image(file: String): Buffer3[UByte] = {
      import java.io.File
      import java.awt.image._
      import javax.imageio._
      val imageFile = new File(file)
      val image: BufferedImage = ImageIO.read(imageFile)

      val buf = new Buffer3[UByte](image.getWidth, image.getHeight, 3) // todo: grayscale?? alpha?
      for (j <- 0 until image.getHeight; i <- 0 until image.getWidth) {
        val rgb = image.getRGB(i,j)
        buf(i,j,0) = ((rgb >>> 16) & 0xFF)//.toByte
        buf(i,j,1) = ((rgb >>> 8) & 0xFF)//.toByte
        buf(i,j,2) = ((rgb >>> 0) & 0xFF)//.toByte
      }

      buf
    }

    def save_image(buf: Buffer3[UByte], file: String): Unit = {
      assert(buf.channels == 3) // greyscale? alpha?

      import java.io.File
      import java.awt.image._
      import javax.imageio._
      val imageFile = new File(file)
      val image: BufferedImage = new BufferedImage(buf.width, buf.height, BufferedImage.TYPE_INT_RGB)

      for (j <- 0 until image.getHeight; i <- 0 until image.getWidth) {
        val rgb = ((buf(i,j,0) & 0xFF) << 16) | ((buf(i,j,1) & 0xFF) << 8) | ((buf(i,j,2) & 0xFF) << 0)
        image.setRGB(i,j,rgb)
      }

      ImageIO.write(image, "png", imageFile)
    }


    def test1(): Unit = {
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient")

      val e = x + y

      gradient(x, y) = e

      val output = gradient.realize[Int](800, 600)

      // test
      for (j <- 0 until output.height) {
          for (i <- 0 until output.width) {
              if (output(i, j) != i + j) {
                  println("Something went wrong!\n"+
                         s"Pixel $i, $j was supposed to be ${i+j}, but instead it's ${output(i, j)}")
                  return
              }
          }
      }
      println("Success!")
    }

    def test2(): Unit = {
      val x = Var("x")
      val y = Var("y")
      val c = Var("c")

      val img = load_image("/Users/me/Desktop/tryout/halide/tutorial/images/rgb.png")

      val brighter = Func3()

      // Note: we don't have a proper UByte type in Scala, hence map it to Int
      // Note: .toDouble introduces an expected type: all subexprs evaluated as double

      brighter(x,y,c) = min((img(x,y,c) * 1.5), 255.0).toDouble

      val output = brighter.realize[UByte](img.width, img.height, img.channels)

      save_image(output,"brighter.png")

      println("Success!")
    }

    def test51(): Unit = {
      // First we observe the default ordering.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient")
      gradient(x, y) = x + y
      gradient.trace_stores()

      println("Evaluating gradient row-major");
      val output = gradient.realize[Int](4, 4);

      println("Equivalent C:");
      for (y <- 0 until 4) {
          for (x <- 0 until 4) {
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
          }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test52(): Unit = {
      // Reorder variables.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_col_major")
      gradient(x, y) = x + y
      gradient.trace_stores()

      gradient.reorder(y, x)

      println("Evaluating gradient column-major");
      val output = gradient.realize[Int](4, 4);

      println("Equivalent C:");
      for (x <- 0 until 4) {
          for (y <- 0 until 4) {
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
          }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test53(): Unit = {
      // Split a variable into two.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_split")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      gradient.split(x, x_outer, x_inner, 2)

      println("Evaluating gradient with x split into x_outer and x_inner");
      val output = gradient.realize[Int](4, 4);

      println("Equivalent C:");
      for (y <- 0 until 4) {
        for (x_outer <- 0 until 2) {   // run until w / 2
          for (x_inner <- 0 until 2) { // split factor 2
              val x = x_outer * 2 + x_inner;
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
          }
        }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test54(): Unit = {
      // Fuse two variables into one.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_fused")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val fused = Var("fused")
      gradient.fuse(x, y, fused)

      println("Evaluating gradient with x and y fused");
      val output = gradient.realize[Int](4, 4);

      println("Equivalent C:");
      for (fused <- 0 until 4*4) {
          val y = fused / 4;
          val x = fused % 4;
          println(s"Evaluating at x = $x, y = $y: ${ x + y }")
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test55(): Unit = {
      // Evaluating in tiles.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_tiled")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      val y_outer = Var("y_outer")
      val y_inner = Var("y_inner")
      gradient.split(x, x_outer, x_inner, 4)
      gradient.split(y, y_outer, y_inner, 4)
      gradient.reorder(x_inner, y_inner, x_outer, y_outer)

      // This pattern is common enough that there's a shorthand for it:
      // gradient.tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4);

      println("Evaluating gradient in 4x4 tiles");
      val output = gradient.realize[Int](8, 8);

      println("Equivalent C:");
      for (y_outer <- 0 until 2) {
          for (x_outer <- 0 until 2) {
              for (y_inner <- 0 until 4) {
                  for (x_inner <- 0 until 4) {
                      val x = x_outer * 4 + x_inner
                      val y = y_outer * 4 + y_inner
                      println(s"Evaluating at x = $x, y = $y: ${ x + y }")
                  }
              }
          }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test56(): Unit = {
      // Evaluating in vectors.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_in_vectors")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      gradient.split(x, x_outer, x_inner, 4)
      gradient.vectorize(x_inner)

      // Splitting and then vectorizing the inner variable is common
      // enough that there's a short-hand for it. We could have also
      // said:
      //
      // gradient.vectorize(x, 4);
      //
      // which is equivalent to:
      //
      // gradient.split(x, x, x_inner, 4);
      // gradient.vectorize(x_inner);
      //
      // Note that in this case we reused the name 'x' as the new
      // outer variable. Later scheduling calls that refer to x
      // will refer to this new outer variable named x.

      println("Evaluating gradient with x_inner vectorized");
      val output = gradient.realize[Int](8, 4);

      println("Equivalent C:");
      for (y <- 0 until 4) {
          for (x_outer <- 0 until 2) {
              val x_vec = (x_outer * 4 + 0,
                           x_outer * 4 + 1,
                           x_outer * 4 + 2,
                           x_outer * 4 + 3)

              val value = (x_vec._1 + y,
                           x_vec._2 + y,
                           x_vec._3 + y,
                           x_vec._4 + y)
              println(s"Evaluating at $x_vec, ${(y,y,y,y)}: $value")
          }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test57(): Unit = {
      // Unrolling a loop.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_in_vectors")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      gradient.split(x, x_outer, x_inner, 2)
      gradient.unroll(x_inner)

      // The shorthand for this is:
      // gradient.unroll(x, 2);

      println("Evaluating gradient unrolled by a factor of two");
      val output = gradient.realize[Int](4, 4);

      println("Equivalent C:");
      for (y <- 0 until 4) {
          for (x_outer <- 0 until 2) {
            {
              val x_inner = 0
              val x = x_outer * 2 + x_inner;
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
            }
            {
              val x_inner = 1
              val x = x_outer * 2 + x_inner;
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
            }
          }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test58(): Unit = { // XXX TODO
      // Splitting by factors that don't divide the extent.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_split")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      gradient.split(x, x_outer, x_inner, 3)

      println("Evaluating gradient over a 7x2 box with x split by three");
      val output = gradient.realize[Int](7, 2);

      println("Equivalent C:");
      for (y <- 0 until 2) {
        for (x_outer <- 0 until 3) {   // Now runs from 0 to 2
          for (x_inner <- 0 until 3) { // split factor 2
              var x = x_outer * 3 + x_inner;
              // Before we add x_inner, make sure we don't
              // evaluate points outside of the 7x2 box. We'll
              // clamp x to be at most 4 (7 minus the split
              // factor).
              if (x > 4) x = 4;
              x += x_inner;
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
          }
        }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test59(): Unit = {
      // Fusing, tiling, and parallelizing.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_fused_tiles")
      gradient(x, y) = x + y
      gradient.trace_stores()

      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      val y_outer = Var("y_outer")
      val y_inner = Var("y_inner")
      val tile_index = Var("tile_index")
      gradient.tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4)
      gradient.fuse(x_outer, y_outer, tile_index)
      gradient.parallel(tile_index)

      println("Evaluating gradient tiles in parallel");
      val output = gradient.realize[Int](8,8);

      println("Equivalent (serial) C:");
      for (tile_index <- 0 until 4) { // should be parallel
        val y_outer = tile_index / 2
        val x_outer = tile_index % 2
        for (y_inner <- 0 until 4) {
          for (x_inner <- 0 until 4) {
              var y = y_outer * 4 + y_inner;
              var x = x_outer * 4 + x_inner;
              println(s"Evaluating at x = $x, y = $y: ${x + y}");
          }
        }
      }

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test5A(): Unit = {
      // Putting it all together.
      val x = Var("x")
      val y = Var("y")

      val gradient = Func("gradient_fast")
      gradient(x, y) = x + y
      //gradient.trace_stores()

      // We'll process 64x64 tiles in parallel.
      val x_outer = Var("x_outer")
      val x_inner = Var("x_inner")
      val y_outer = Var("y_outer")
      val y_inner = Var("y_inner")
      val tile_index = Var("tile_index")
      gradient.tile(x, y, x_outer, y_outer, x_inner, y_inner, 64, 64)
      gradient.fuse(x_outer, y_outer, tile_index)
      gradient.parallel(tile_index)

      // We'll compute two scanlines at once while we walk across
      // each tile. We'll also vectorize in x. The easiest way to
      // express this is to recursively tile again within each tile
      // into 4x2 subtiles, then vectorize the subtiles across x and
      // unroll them across y:
      val x_inner_outer = Var("x_inner_outer")
      val y_inner_outer = Var("y_inner_outer")
      val x_vectors = Var("x_vectors")
      val y_pairs = Var("y_pairs")
      gradient.tile(x_inner, y_inner, x_inner_outer, y_inner_outer, x_vectors, y_pairs, 4, 2)
      gradient.vectorize(x_vectors)
      gradient.unroll(y_pairs)


      println("Evaluating gradient tiles in parallel");
      //val output = gradient.realize[Int](350,250); Halide tutorial used non-multiple size
      //val output = gradient.realize[Int](320,256);

      println("Checking Halide result against equivalent C...");
      println("TODO!!")

      println("Pseudo-code for the schedule:");
      gradient.print_loop_nest();
    }

    def test81(): Unit = {
    // Let's examine various scheduling options for a simple two stage
    // pipeline. We'll start with the default schedule:
        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_default")
        val consumer = Func("consumer_default");

        // The first stage will be some simple pointwise math similar
        // to our familiar gradient function. The value at position x,
        // y is the sin of product of x and y.
        producer(x, y) = sin(x * y);

        // Now we'll add a second stage which averages together multiple
        // points in the first stage.
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;

        // We'll turn on tracing for both functions.
        consumer.trace_stores();
        producer.trace_stores();

        // And evaluate it over a 4x4 box.
        println("Evaluating producer-consumer pipeline with default schedule");
        consumer.realize[Double](4, 4);

        // There were no messages about computing values of the
        // producer. This is because the default schedule fully
        // inlines 'producer' into 'consumer'. It is as if we had
        // written the following code instead:

        // consumer(x, y) = (sin(x * y) +
        //                   sin(x * (y + 1)) +
        //                   sin((x + 1) * y) +
        //                   sin((x + 1) * (y + 1))/4);

        // All calls to 'producer' have been replaced with the body of
        // 'producer', with the arguments substituted in for the
        // variables.

        // The equivalent C code is:
        /*float result[4][4];
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {
                result[y][x] = (sin(x*y) +
                                sin(x*(y+1)) +
                                sin((x+1)*y) +
                                sin((x+1)*(y+1)))/4;
            }
        }*/

        // If we look at the loop nest, the producer doesn't appear
        // at all. It has been inlined into the consumer.
        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();
    }

    def test82(): Unit = {
    // Next we'll examine the next simplest option - computing all
    // values required in the producer before computing any of the
    // consumer. We call this schedule "root".
        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_root")
        val consumer = Func("consumer_root");

        // The first stage will be some simple pointwise math similar
        // to our familiar gradient function. The value at position x,
        // y is the sin of product of x and y.
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;

        // Tell Halide to evaluate all of producer before any of consumer.
        producer.compute_root();

        // Turn on tracing.
        consumer.trace_stores();
        producer.trace_stores();

        // Compile and run.
        println("Evaluating producer.compute_root()");
        consumer.realize[Double](4, 4);

        // Reading the output we can see that:
        // A) There were stores to producer.
        // B) They all happened before any stores to consumer.

        // See figures/lesson_08_compute_root.gif for a visualization.
        // The producer is on the left and the consumer is on the
        // right. Stores are marked in orange and loads are marked in
        // blue.

        // Equivalent C:

        /*float result[4][4];

        // Allocate some temporary storage for the producer.
        float producer_storage[5][5];

        // Compute the producer.
        for (int y = 0; y < 5; y++) {
            for (int x = 0; x < 5; x++) {
                producer_storage[y][x] = sin(x * y);
            }
        }

        // Compute the consumer. Skip the prints this time.
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {
                result[y][x] = (producer_storage[y][x] +
                                producer_storage[y+1][x] +
                                producer_storage[y][x+1] +
                                producer_storage[y+1][x+1])/4;
            }
        }*/

        // Note that consumer was evaluated over a 4x4 box, so Halide
        // automatically inferred that producer was needed over a 5x5
        // box. This is the same 'bounds inference' logic we saw in
        // the previous lesson, where it was used to detect and avoid
        // out-of-bounds reads from an input image.

        // If we print the loop nest, we'll see something very
        // similar to the C above.
        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();
    }


  def main(args: Array[String]): Unit = {
    test1()

    //test2()

    test51()
    test52()
    test53()
    test54()
    test55()
    test56()
    test57()
    //test58() TODO
    test59()
    test5A()

    test81()
    test82()

    println("done")
  }
}