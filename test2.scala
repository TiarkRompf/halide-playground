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

    type Env = Map[String,Int]        // var -> value
    type BEnv = Map[String,(Int,Int)] // var -> bounds
    type FEnv = Map[String,Buffer[_]] // function -> buffer

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
      var computeAt: Option[(Func,Var)] = None
      var storeRoot = false
      var storeAt: Option[(Func,Var)] = None

      var computeBounds: List[(String, BEnv => Stencil)] = _
      var computeVar: List[(String, (Env,BEnv) => Int)] = _

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


      def dim(s: Stencil) = s._2 - s._1

      def split(x: Var, x_outer: Var, x_inner: Var, factor: Int): Unit = {
        assert(true) // todo: sanity checks! outer/inner fresh
        order = order.flatMap(s => if (s == x.s) List(x_outer.s, x_inner.s) else List(s))

        // for uneven splits, Halide may compute values multiple times -- from the docs:
        // x_outer runs from 0 to (x_extent + factor - 1)/factor
        // x_inner runs from 0 to factor
        // x = min(x_outer * factor, x_extent - factor) + x_inner + x_min

        // even split only version below
        computeBounds :+= ((x_outer.s, (bounds: BEnv) => (0, (dim(bounds(x.s)) + factor - 1) / factor) ))
        computeBounds :+= ((x_inner.s, (bounds: BEnv) => (0, factor)))
        computeVar    ::= ((x.s, (env:Env,bounds:BEnv) => bounds(x.s)._1 + ((env(x_outer.s) * factor) min (dim(bounds(x.s))- factor)) + env(x_inner.s)))

        // computeBounds :+= ((x_outer.s, (bounds: BEnv) => { assert(dim(bounds(x.s)) % factor == 0); (0, dim(bounds(x.s)) / factor) })) // assert evenly divisible!!
        // computeBounds :+= ((x_inner.s, (bounds: BEnv) => (0, factor)))
        // computeVar    ::= ((x.s, (env:Env,bounds:BEnv) => bounds(x.s)._1 + env(x_outer.s) * factor + env(x_inner.s)))
      }

      def fuse(x: Var, y: Var, fused: Var): Unit = {
        assert(true) // todo: sanity checks! fused fresh
        // Q: which position? need to be adjacent?
        // currently we assume y is outer, and we fuse into that position
        order = order.flatMap(s => if (s == y.s) List(fused.s) else if (s == x.s) List() else List(s))
        computeBounds :+= ((fused.s, (bounds: BEnv) => (0, dim(bounds(x.s)) * dim(bounds(y.s)))))
        computeVar    ::= ((x.s, (env:Env,bounds:BEnv) => bounds(x.s)._1 + env(fused.s) % dim(bounds(x.s))))
        computeVar    ::= ((y.s, (env:Env,bounds:BEnv) => bounds(y.s)._1 + env(fused.s) / dim(bounds(x.s))))
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
      def vectorize(x: Var, n: Int): Unit = {
        val xo = Var(x.s +"_outer")
        val xi = Var(x.s +"_inner")
        split(x, xo, xi, n)
        vectorize(xi)
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

      def compute_at(f: Func, y: Var): Unit = {
        computeAt = Some((f,y))
      }

      def store_root(): Unit = {
        storeRoot = true
      }

      def store_at(f: Func, y: Var): Unit = {
        storeAt = Some((f,y))
      }



      type Stencil = (Int,Int)
      def stencil(v: Var, e: Expr): Stencil = e match {
        case `v` => (0,1)
        case Plus(`v`, Const(a))  => (a.toInt,a.toInt+1)
        case Minus(`v`, Const(a)) => (-a.toInt,-a.toInt+1)
      }
      def merge(a: Stencil, b: Stencil): Stencil = (a._1 min b._1, a._2 max b._2)

      def getRootStages: List[(Func, Stencil, Stencil)] = {
        var fs: List[(Func, Stencil, Stencil)] = Nil
        def traverseFun(f: Func, sx: Stencil, sy: Stencil): Unit = {
          fs ::= (f,sx,sy)
          traverse(f.impl.body) {
            case Apply(f1@Func(s), List(x,y)) =>  // FIXME: 2
              val sx1 = stencil(f.impl.x, x)
              val sy1 = stencil(f.impl.y, y)
              if (f1.computeRoot || f1.storeRoot)
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

      def getInternalStages(vy: Var): List[(Func, Stencil, Stencil)] = {
        var fs: List[(Func, Stencil, Stencil)] = Nil
        def traverseFun(f: Func, sx: Stencil, sy: Stencil): Unit = {
          traverse(f.impl.body) {
            case Apply(f1@Func(s), List(x,y)) =>  // FIXME: 2
              val sx1 = stencil(f.impl.x, x)
              val sy1 = stencil(f.impl.y, y)
              if (f1.computeAt == Some(this,vy)) {
                fs ::= (f1,sx1,sy1)
                traverseFun(f1, sx1, sy1)
              }
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
        val fs = getRootStages
        implicit var fenv = Map[String, Buffer[_]]()
        for ((f,(lx,hx),(ly,hy)) <- fs) {
          val buf = new Buffer[T](lx, ly, w+hx, h+hy)
          if (f.computeRoot || f == this)
            f.realize_internal[T](lx, ly, w+hx, h+hy, buf)
          fenv += (f.s -> buf)
        }
        fenv(s).asInstanceOf[Buffer[T]]
      }

      def realize_internal[T:Type](x0: Int, y0: Int, x1: Int, y1: Int)(implicit fenv: FEnv): Buffer[T] = {
        val buf = new Buffer[T](x0,y0,x1,y1)
        realize_internal(x0,y0,x1,y1,buf)
      }

      def realize_internal[T:Type](x0: Int, y0: Int, x1: Int, y1: Int, buf: Buffer[T])(implicit fenv: FEnv): Buffer[T] = {
        var bounds = Map(impl.x.s -> (x0,x1), impl.y.s -> (y0,y1))

        for ((s,f) <- computeBounds)
          bounds += (s -> f(bounds))

        def loop(vs: List[String], env: Env, fenv: FEnv)(f: (Env,FEnv) => Unit): Unit = vs match {
          case Nil => 
            f(env, fenv)

          case x::vs => 
            for (i <- bounds(x)._1 until bounds(x)._2) {
              val env1 = env + (x -> i)
              implicit var fenv1 = fenv
              for ((f,(lx,hx),(ly,hy)) <- getInternalStages(Var(x))) {
                //println("XXX! "+x+" --> "+f)

                // FIXME: this doesn't work if x/y have been split, because x/y are
                // put into env only in the innermost loop by computeVar

                if (!f.storeRoot) {
                  // we're allocating a fresh buffer

                  // find current ranges of x/y, narrowed by enclosing loops
                  val (llx, hhx) = if (env1 contains impl.x.s) (env1(impl.x.s), env1(impl.x.s)) else (x0,x1)
                  val (lly, hhy) = if (env1 contains impl.y.s) (env1(impl.y.s), env1(impl.y.s)) else (y0,y1)

                  fenv1 += (f.s -> f.realize_internal[T](llx+lx, lly+ly, hhx+hx, hhy+hy))
                } else {
                  // if store_root, then we already have a buffer. reuse scanlines already computed.
                  val buf = fenv1(f.s).asInstanceOf[Buffer[T]]

                  // find current ranges of x/y, narrowed by enclosing loops
                  val (llx, hhx) = if (env1 contains impl.x.s) { 
                    if (env1(impl.x.s) == 0) 
                      (env1(impl.x.s)+lx, env1(impl.x.s)+hx)
                    else {
                      // assert previous bounds ...
                      (env1(impl.x.s)+hx-1, env1(impl.x.s)+hx) // compute only one new scanline! 
                    }
                  } else (x0+lx,x1+hx)

                  val (lly, hhy) = if (env1 contains impl.y.s) {
                    if (env1(impl.y.s) == 0) 
                      (env1(impl.y.s)+ly, env1(impl.y.s)+hy)
                    else {
                      // assert previous bounds ...
                      (env1(impl.y.s)+hy-1, env1(impl.y.s)+hy) // compute only one new scanline!
                    }
                  } else (y0+ly,y1+hy)

                  f.realize_internal[T](llx, lly, hhx, hhy, buf)

                  // TODO: use a circular buffer! (discard old data...)
                }
              }

              loop(vs, env1, fenv1)(f)
            }
        }

        // todo: vectorize/unroll/parallel are currently no-ops for execution

        loop(order, Map(),fenv) { (e0,fe0) =>
          implicit var env = e0
          implicit var fenv = fe0
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
        val fs = getRootStages
        println("stages: " + fs.mkString(", "))
        fs.foreach(_._1.print_loop_nest_internal)
      }

      def print_loop_nest_internal(): Unit = {
        for ((s,_) <- computeBounds)
          println(s"val max_${s} = ...")
        for (x <- order) {
          if (unrolled contains x)
            println(s"for ${x} (unrolled):")
          else if (vectorized contains x)
            println(s"for ${x} (vectorized):")
          else if (parallel contains x)
            println(s"for ${x} (parallel):")
          else
            println(s"for ${x}:")

          for (p <- getInternalStages(Var(x))) {
            println(s"sub $p {")
            p._1.print_loop_nest()
            println("}")
          }
        }
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

    class Buffer[T:Type](val x0: Int, val y0: Int, val x1: Int, val y1: Int) {
      val data: Array[Array[T]] = Array.ofDim[T](y1-y0, x1-x0)
      def apply(x: Int, y: Int) = data(y-y0)(x-x0)
      def update(x: Int, y: Int, v: T) = data(y-y0)(x-x0) = v
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
      for (j <- 0 until 600) {
          for (i <- 0 until 800) {
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

    // Let's compare the two approaches above from a performance
    // perspective.

    // Full inlining (the default schedule):
    // - Temporary memory allocated: 0
    // - Loads: 0
    // - Stores: 16
    // - Calls to sin: 64

    // producer.compute_root():
    // - Temporary memory allocated: 25 floats
    // - Loads: 64
    // - Stores: 41
    // - Calls to sin: 25

    // There's a trade-off here. Full inlining used minimal temporary
    // memory and memory bandwidth, but did a whole bunch of redundant
    // expensive math (calling sin). It evaluated most points in
    // 'producer' four times. The second schedule,
    // producer.compute_root(), did the mimimum number of calls to
    // sin, but used more temporary memory and more memory bandwidth.

    // In any given situation the correct choice can be difficult to
    // make. If you're memory-bandwidth limited, or don't have much
    // memory (e.g. because you're running on an old cell-phone), then
    // it can make sense to do redundant math. On the other hand, sin
    // is expensive, so if you're compute-limited then fewer calls to
    // sin will make your program faster. Adding vectorization or
    // multi-core parallelism tilts the scales in favor of doing
    // redundant work, because firing up multiple cpu cores increases
    // the amount of math you can do per second, but doesn't increase
    // your system memory bandwidth or capacity.

    def test83(): Unit = {
    // We can make choices in between full inlining and
    // compute_root. Next we'll alternate between computing the
    // producer and consumer on a per-scanline basis:

        // Start with the same function definitions:
        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_y")
        val consumer = Func("consumer_y");
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;

        // Tell Halide to evaluate producer as needed per y coordinate
        // of the consumer:
        producer.compute_at(consumer, y);

        // This places the code that computes the producer just
        // *inside* the consumer's for loop over y, as in the
        // equivalent C below.

        // Turn on tracing.
        producer.trace_stores();
        consumer.trace_stores();

        // Compile and run.
        println("Evaluating producer.compute_at(consumer, y)");
        consumer.realize[Double](4, 4);

        // See figures/lesson_08_compute_y.gif for a visualization.

        // Reading the log or looking at the figure you should see
        // that producer and consumer alternate on a per-scanline
        // basis. Let's look at the equivalent C:

        /*float result[4][4];

        // There's an outer loop over scanlines of consumer:
        for (int y = 0; y < 4; y++) {

            // Allocate space and compute enough of the producer to
            // satisfy this single scanline of the consumer. This
            // means a 5x2 box of the producer.
            float producer_storage[2][5];
            for (int py = y; py < y + 2; py++) {
                for (int px = 0; px < 5; px++) {
                    producer_storage[py-y][px] = sin(px * py);
                }
            }

            // Compute a scanline of the consumer.
            for (int x = 0; x < 4; x++) {
                result[y][x] = (producer_storage[0][x] +
                                producer_storage[1][x] +
                                producer_storage[0][x+1] +
                                producer_storage[1][x+1])/4;
            }
        }*/

        // Again, if we print the loop nest, we'll see something very
        // similar to the C above.
        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();

        // The performance characteristics of this strategy are in
        // between inlining and compute root. We still allocate some
        // temporary memory, but less that compute_root, and with
        // better locality (we load from it soon after writing to it,
        // so for larger images, values should still be in cache). We
        // still do some redundant work, but less than full inlining:

        // producer.compute_at(consumer, y):
        // - Temporary memory allocated: 10 floats
        // - Loads: 64
        // - Stores: 56
        // - Calls to sin: 40
    }

    def test84(): Unit = {
    // We could also say producer.compute_at(consumer, x), but this
    // would be very similar to full inlining (the default
    // schedule). Instead let's distinguish between the loop level at
    // which we allocate storage for producer, and the loop level at
    // which we actually compute it. This unlocks a few optimizations.

        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_root_y")
        val consumer = Func("consumer_root_y");
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;


        // Tell Halide to make a buffer to store all of producer at
        // the outermost level:
        producer.store_root();
        // ... but compute it as needed per y coordinate of the
        // consumer.
        producer.compute_at(consumer, y);

        producer.trace_stores();
        consumer.trace_stores();

        println("Evaluating producer.store_root().compute_at(consumer, y)");
        consumer.realize[Double](4, 4);

        // See figures/lesson_08_store_root_compute_y.gif for a
        // visualization.

        // Reading the log or looking at the figure you should see
        // that producer and consumer again alternate on a
        // per-scanline basis. It computes a 5x2 box of the producer
        // to satisfy the first scanline of the consumer, but after
        // that it only computes a 5x1 box of the output for each new
        // scanline of the consumer!
        //
        // Halide has detected that for all scanlines except for the
        // first, it can reuse the values already sitting in the
        // buffer we've allocated for producer. Let's look at the
        // equivalent C:

        /*float result[4][4];

        // producer.store_root() implies that storage goes here:
        float producer_storage[5][5];

        // There's an outer loop over scanlines of consumer:
        for (int y = 0; y < 4; y++) {

            // Compute enough of the producer to satisfy this scanline
            // of the consumer.
            for (int py = y; py < y + 2; py++) {

                // Skip over rows of producer that we've already
                // computed in a previous iteration.
                if (y > 0 && py == y) continue;

                for (int px = 0; px < 5; px++) {
                    producer_storage[py][px] = sin(px * py);
                }
            }

            // Compute a scanline of the consumer.
            for (int x = 0; x < 4; x++) {
                result[y][x] = (producer_storage[y][x] +
                                producer_storage[y+1][x] +
                                producer_storage[y][x+1] +
                                producer_storage[y+1][x+1])/4;
            }
        }*/

        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();

        // The performance characteristics of this strategy are pretty
        // good! The numbers are similar compute_root, except locality
        // is better. We're doing the minimum number of sin calls,
        // and we load values soon after they are stored, so we're
        // probably making good use of the cache:

        // producer.store_root().compute_at(consumer, y):
        // - Temporary memory allocated: 10 floats
        // - Loads: 64
        // - Stores: 39
        // - Calls to sin: 25

        // Note that my claimed amount of memory allocated doesn't
        // match the reference C code. Halide is performing one more
        // optimization under the hood. It folds the storage for the
        // producer down into a circular buffer of two
        // scanlines. Equivalent C would actually look like this:

        /*{
            // Actually store 2 scanlines instead of 5
            float producer_storage[2][5];
            for (int y = 0; y < 4; y++) {
                for (int py = y; py < y + 2; py++) {
                    if (y > 0 && py == y) continue;
                    for (int px = 0; px < 5; px++) {
                        // Stores to producer_storage have their y coordinate bit-masked.
                        producer_storage[py & 1][px] = sin(px * py);
                    }
                }

                // Compute a scanline of the consumer.
                for (int x = 0; x < 4; x++) {
                    // Loads from producer_storage have their y coordinate bit-masked.
                    result[y][x] = (producer_storage[y & 1][x] +
                                    producer_storage[(y+1) & 1][x] +
                                    producer_storage[y & 1][x+1] +
                                    producer_storage[(y+1) & 1][x+1])/4;
                }
            }
        }*/
    }

    def test85(): Unit = {
    // We can do even better, by leaving the storage outermost, but
    // moving the computation into the innermost loop:

        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_root_x")
        val consumer = Func("consumer_root_x");
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;


        // Store outermost, compute innermost.
        producer.store_root()
        producer.compute_at(consumer, x);

        producer.trace_stores();
        consumer.trace_stores();

        println("Evaluating producer.store_root().compute_at(consumer, x)");
        consumer.realize[Double](4, 4);

        // See figures/lesson_08_store_root_compute_x.gif for a
        // visualization.

        // You should see that producer and consumer now alternate on
        // a per-pixel basis. Here's the equivalent C:

        /*float result[4][4];

        // producer.store_root() implies that storage goes here, but
        // we can fold it down into a circular buffer of two
        // scanlines:
        float producer_storage[2][5];

        // For every pixel of the consumer:
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {

                // Compute enough of the producer to satisfy this
                // pixel of the consumer, but skip values that we've
                // already computed:
                if (y == 0 && x == 0)
                    producer_storage[y & 1][x] = sin(x*y);
                if (y == 0)
                    producer_storage[y & 1][x+1] = sin((x+1)*y);
                if (x == 0)
                    producer_storage[(y+1) & 1][x] = sin(x*(y+1));
                producer_storage[(y+1) & 1][x+1] = sin((x+1)*(y+1));

                result[y][x] = (producer_storage[y & 1][x] +
                                producer_storage[(y+1) & 1][x] +
                                producer_storage[y & 1][x+1] +
                                producer_storage[(y+1) & 1][x+1])/4;
            }
        }*/

        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();

        // The performance characteristics of this strategy are the
        // best so far. One of the four values of the producer we need
        // is probably still sitting in a register, so I won't count
        // it as a load:
        // producer.store_root().compute_at(consumer, x):
        // - Temporary memory allocated: 10 floats
        // - Loads: 48
        // - Stores: 56
        // - Calls to sin: 40
    }

    // So what's the catch? Why not always do
    // producer.store_root().compute_at(consumer, x) for this type of
    // code?
    //
    // The answer is parallelism. In both of the previous two
    // strategies we've assumed that values computed on previous
    // iterations are lying around for us to reuse. This assumes that
    // previous values of x or y happened earlier in time and have
    // finished. This is not true if you parallelize or vectorize
    // either loop. Darn. If you parallelize, Halide won't inject the
    // optimizations that skip work already done if there's a parallel
    // loop in between the store_at level and the compute_at level,
    // and won't fold the storage down into a circular buffer either,
    // which makes our store_root pointless.

    def test86(): Unit = {
    // We're running out of options. We can make new ones by
    // splitting. We can store_at or compute_at at the natural
    // variables of the consumer (x and y), or we can split x or y
    // into new inner and outer sub-variables and then schedule with
    // respect to those. We'll use this to express fusion in tiles:

        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_tile")
        val consumer = Func("consumer_tile");
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;

        // We'll compute 8x8 of the consumer, in 4x4 tiles.
        val (x_outer, y_outer, x_inner, y_inner) = 
          (Var("x_outer"), Var("y_outer"), Var("x_inner"), Var("y_inner"))
        consumer.tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4);

        // Compute the producer per tile of the consumer
        producer.compute_at(consumer, x_outer);

        // Notice that I wrote my schedule starting from the end of
        // the pipeline (the consumer). This is because the schedule
        // for the producer refers to x_outer, which we introduced
        // when we tiled the consumer. You can write it in the other
        // order, but it tends to be harder to read.

        // Turn on tracing.
        producer.trace_stores();
        consumer.trace_stores();

        println("Evaluating:\n"+
               "consumer.tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4);\n"+
               "producer.compute_at(consumer, x_outer);");
        consumer.realize[Double](8, 8);

        // See figures/lesson_08_tile.gif for a visualization.

        // The producer and consumer now alternate on a per-tile
        // basis. Here's the equivalent C:

        /*float result[8][8];

        // For every tile of the consumer:
        for (int y_outer = 0; y_outer < 2; y_outer++) {
            for (int x_outer = 0; x_outer < 2; x_outer++) {
                // Compute the x and y coords of the start of this tile.
                int x_base = x_outer*4;
                int y_base = y_outer*4;

                // Compute enough of producer to satisfy this tile. A
                // 4x4 tile of the consumer requires a 5x5 tile of the
                // producer.
                float producer_storage[5][5];
                for (int py = y_base; py < y_base + 5; py++) {
                    for (int px = x_base; px < x_base + 5; px++) {
                        producer_storage[py-y_base][px-x_base] = sin(px * py);
                    }
                }

                // Compute this tile of the consumer
                for (int y_inner = 0; y_inner < 4; y_inner++) {
                    for (int x_inner = 0; x_inner < 4; x_inner++) {
                        int x = x_base + x_inner;
                        int y = y_base + y_inner;
                        result[y][x] =
                            (producer_storage[y - y_base][x - x_base] +
                             producer_storage[y - y_base + 1][x - x_base] +
                             producer_storage[y - y_base][x - x_base + 1] +
                             producer_storage[y - y_base + 1][x - x_base + 1])/4;
                    }
                }
            }
        }*/

        println("Pseudo-code for the schedule:");
        consumer.print_loop_nest();

        // Tiling can make sense for problems like this one with
        // stencils that reach outwards in x and y. Each tile can be
        // computed independently in parallel, and the redundant work
        // done by each tile isn't so bad once the tiles get large
        // enough.
    }

    def test87(): Unit = {
    // Let's try a mixed strategy that combines what we have done with
    // splitting, parallelizing, and vectorizing. This is one that
    // often works well in practice for large images. If you
    // understand this schedule, then you understand 95% of scheduling
    // in Halide.

        val x = Var("x")
        val y = Var("y")
        val producer = Func("producer_mixed")
        val consumer = Func("consumer_mixed");
        producer(x, y) = sin(x * y);
        consumer(x, y) = (producer(x, y) +
                          producer(x, y+1) +
                          producer(x+1, y) +
                          producer(x+1, y+1))/4;

        // Split the y coordinate of the consumer into strips of 16 scanlines:
        val yo = Var("yo"); val yi = Var("yi");
        consumer.split(y, yo, yi, 16);
        // Compute the strips using a thread pool and a task queue.
        consumer.parallel(yo);
        // Vectorize across x by a factor of four.
        consumer.vectorize(x, 4);

        // Now store the producer per-strip. This will be 17 scanlines
        // of the producer (16+1), but hopefully it will fold down
        // into a circular buffer of two scanlines:
        producer.store_at(consumer, yo);
        // Within each strip, compute the producer per scanline of the
        // consumer, skipping work done on previous scanlines.
        producer.compute_at(consumer, yi);
        // Also vectorize the producer (because sin is vectorizable on x86 using SSE).
        //producer.vectorize(x, 4);

        // Let's leave tracing off this time, because we're going to
        // evaluate over a larger image.
        // consumer.trace_stores();
        // producer.trace_stores();

        //Buffer<float> halide_result = consumer.realize(160, 160);
        val halide_result = consumer.realize[Double](160, 160);

        // See figures/lesson_08_mixed.mp4 for a visualization.

        // Here's the equivalent (serial) C:

        /*float c_result[160][160];

        // For every strip of 16 scanlines (this loop is parallel in
        // the Halide version)
        for (int yo = 0; yo < 160/16 + 1; yo++) {

            // 16 doesn't divide 160, so push the last slice upwards
            // to fit within [0, 159] (see lesson 05).
            int y_base = yo * 16;
            if (y_base > 160-16) y_base = 160-16;

            // Allocate a two-scanline circular buffer for the producer
            float producer_storage[2][161];

            // For every scanline in the strip of 16:
            for (int yi = 0; yi < 16; yi++) {
                int y = y_base + yi;

                for (int py = y; py < y+2; py++) {
                    // Skip scanlines already computed *within this task*
                    if (yi > 0 && py == y) continue;

                    // Compute this scanline of the producer in 4-wide vectors
                    for (int x_vec = 0; x_vec < 160/4 + 1; x_vec++) {
                        int x_base = x_vec*4;
                        // 4 doesn't divide 161, so push the last vector left
                        // (see lesson 05).
                        if (x_base > 161 - 4) x_base = 161 - 4;
                        // If you're on x86, Halide generates SSE code for this part:
                        int x[] = {x_base, x_base + 1, x_base + 2, x_base + 3};
                        float vec[4] = {sinf(x[0] * py), sinf(x[1] * py),
                                        sinf(x[2] * py), sinf(x[3] * py)};
                        producer_storage[py & 1][x[0]] = vec[0];
                        producer_storage[py & 1][x[1]] = vec[1];
                        producer_storage[py & 1][x[2]] = vec[2];
                        producer_storage[py & 1][x[3]] = vec[3];
                    }
                }

                // Now compute consumer for this scanline:
                for (int x_vec = 0; x_vec < 160/4; x_vec++) {
                    int x_base = x_vec * 4;
                    // Again, Halide's equivalent here uses SSE.
                    int x[] = {x_base, x_base + 1, x_base + 2, x_base + 3};
                    float vec[] = {
                        (producer_storage[y & 1][x[0]] +
                         producer_storage[(y+1) & 1][x[0]] +
                         producer_storage[y & 1][x[0]+1] +
                         producer_storage[(y+1) & 1][x[0]+1])/4,
                        (producer_storage[y & 1][x[1]] +
                         producer_storage[(y+1) & 1][x[1]] +
                         producer_storage[y & 1][x[1]+1] +
                         producer_storage[(y+1) & 1][x[1]+1])/4,
                        (producer_storage[y & 1][x[2]] +
                         producer_storage[(y+1) & 1][x[2]] +
                         producer_storage[y & 1][x[2]+1] +
                         producer_storage[(y+1) & 1][x[2]+1])/4,
                        (producer_storage[y & 1][x[3]] +
                         producer_storage[(y+1) & 1][x[3]] +
                         producer_storage[y & 1][x[3]+1] +
                         producer_storage[(y+1) & 1][x[3]+1])/4};

                    c_result[y][x[0]] = vec[0];
                    c_result[y][x[1]] = vec[1];
                    c_result[y][x[2]] = vec[2];
                    c_result[y][x[3]] = vec[3];
                }

            }
        }*/
        printf("Pseudo-code for the schedule:\n");
        consumer.print_loop_nest();
        printf("\n");

        // Look on my code, ye mighty, and despair!

        // Let's check the C result against the Halide result. Doing
        // this I found several bugs in my C implementation, which
        // should tell you something.
        /*for (int y = 0; y < 160; y++) {
            for (int x = 0; x < 160; x++) {
                float error = halide_result(x, y) - c_result[y][x];
                // It's floating-point math, so we'll allow some slop:
                if (error < -0.001f || error > 0.001f) {
                    printf("halide_result(%d, %d) = %f instead of %f\n",
                           x, y, halide_result(x, y), c_result[y][x]);
                    return -1;
                }
            }
        }*/

    }

    // This stuff is hard. We ended up in a three-way trade-off
    // between memory bandwidth, redundant work, and
    // parallelism. Halide can't make the correct choice for you
    // automatically (sorry). Instead it tries to make it easier for
    // you to explore various options, without messing up your
    // program. In fact, Halide promises that scheduling calls like
    // compute_root won't change the meaning of your algorithm -- you
    // should get the same bits back no matter how you schedule
    // things.

    // So be empirical! Experiment with various schedules and keep a
    // log of performance. Form hypotheses and then try to prove
    // yourself wrong. Don't assume that you just need to vectorize
    // your code by a factor of four and run it on eight cores and
    // you'll get 32x faster. This almost never works. Modern systems
    // are complex enough that you can't predict performance reliably
    // without running your code.

    // We suggest you start by scheduling all of your non-trivial
    // stages compute_root, and then work from the end of the pipeline
    // upwards, inlining, parallelizing, and vectorizing each stage in
    // turn until you reach the top.

    // Halide is not just about vectorizing and parallelizing your
    // code. That's not enough to get you very far. Halide is about
    // giving you tools that help you quickly explore different
    // trade-offs between locality, redundant work, and parallelism,
    // without messing up the actual result you're trying to compute.


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
    test58()
    test59()
    test5A()

    test81()
    test82()
    test83()
    test84()
    test85()
    test86()
    test87() 

    // TODO: +non-divisible split, circular buffer, store_at, compute_at after split

    // TODO: check results, parallelize

    println("done")
  }
}