package tensor

// This file: Halide in Scala

object Halide {
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
        val List(x,y) = as.map(eval[Int]) // FIXME: only 2 args
        if (f.computeRoot) {
          val buf = fenv(f.s).asInstanceOf[Buffer[T]]
          buf(x,y)
        } else {
          val env1: Env = Map(f.impl.x.s -> x, f.impl.y.s -> y)
          eval[T](f.impl.body)(tpe, env1, fenv)
        }
      case Apply3(f,a,b,c) => tpe.fromInt(f(eval[Int](a), eval[Int](b), eval[Int](c)).asInstanceOf[UByte]) // assume byte
    }

    type Stencil = (Int,Int)
    def stencil(v: Var, e: Expr): Stencil = e match {
      case `v` => (0,0)
      case Plus(`v`, Const(a))  => (a.toInt,a.toInt)
      case Minus(`v`, Const(a)) => (-a.toInt,-a.toInt)
    }
    def merge(a: Stencil, b: Stencil): Stencil = (a._1 min b._1, a._2 max b._2)
    def merge2(a: (Stencil,Stencil), b: (Stencil,Stencil)): (Stencil,Stencil) = (merge(a._1,b._1), merge(a._2,b._2))
    def widen(a: Stencil, b: Stencil): Stencil = (a._1 + b._1, a._2 + b._2)
    def widen2(a: (Stencil,Stencil), b: (Stencil,Stencil)): (Stencil,Stencil) = (widen(a._1,b._1), widen(a._2,b._2))
    def dim(s: Stencil) = s._2 - s._1

    // generic pre-order traversal
    def traverse(x: Any)(f: Any => Unit): Unit = { f(x); x match {
      case x: Product => x.productIterator.foreach(x => traverse(x)(f))
      case _ =>
    }}

    case class FuncInternal(x: Var, y: Var, body: Expr)

    case class Func(s: String) {
      var impl: FuncInternal = _
      var order: List[String] = _

      var trace = 0
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

      def trace_stores() = trace = 3

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



      def getDependentStages(p: Func => Boolean): List[(Func, Stencil, Stencil)] = {
        var fs: List[(Func, Stencil, Stencil)] = Nil
        def traverseFun(f: Func, sx: Stencil, sy: Stencil): Unit = {
          traverse(f.impl.body) {
            case Apply(f1@Func(s), List(x,y)) =>  // FIXME: 2
              val sx1 = widen(sx, stencil(f.impl.x, x))
              val sy1 = widen(sy, stencil(f.impl.y, y))
              if (p(f1))
                fs ::= (f1,sx1,sy1)
              traverseFun(f1, sx1, sy1)
            case _ => 
          }
        }
        traverseFun(this, (0,0), (0,0))
        
        val gx = fs.groupBy(_._1).map(p => (p._1, p._2.map(_._2).reduce(merge)))
        val gy = fs.groupBy(_._1).map(p => (p._1, p._2.map(_._3).reduce(merge)))
        fs.map(_._1).distinct.map(k => (k,gx(k),gy(k)))
      }

      def getRootStages = 
        getDependentStages(f1 => f1.computeRoot || f1.storeRoot)

      def getInternalStages(vy: Var) = 
        getDependentStages(f1 => f1.computeAt == Some(this,vy) || f1.storeAt == Some(this,vy))
      // TODO optimization: would need to traverse only functions computed at or after vy
      // (the whole computeAt story is a bit shaky if a function is used multiple times)

      def computeAllBounds(x0: Int, y0: Int, x1: Int, y1: Int) = {
        var bounds = Map(impl.x.s -> (x0,x1), impl.y.s -> (y0,y1))
        for ((s,f) <- computeBounds)
          bounds += (s -> f(bounds))
        bounds
      }

      def computeAllVars(env: Env, bounds: BEnv) = {
        var env1 = env
        for ((s,f) <- computeVar)
          env1 += (s -> f(env1,bounds))
        env1
      }

      // compute the window of x,y values spanned by loop vars vs, given current env
      // TODO: optimize (exponential)
      def hilo(vs: List[String], env: Env, bounds: BEnv): (Stencil, Stencil) = vs match {
        case Nil =>
          val env1 = computeAllVars(env, bounds)
          val i = env1(impl.x.s)
          val j = env1(impl.y.s)
          ((i,i), (j,j))

        case x::vs => 
          assert(bounds(x)._1 < bounds(x)._2)
          val (lsx,lsy) = hilo(vs, env + (x -> bounds(x)._1), bounds)
          val (hsx,hsy) = hilo(vs, env + (x -> (bounds(x)._2-1)), bounds)
          (merge(lsx,hsx), merge(lsy,hsy))
      }
      // compute the window of x,y values spanned by loop vars vs after y, given current env
      def hiloPrefix(vs: List[String], y: Var, env: Env, bounds: BEnv): (Stencil, Stencil) = vs match {
        case Nil =>
          ((0,0), (0,0))
        case x::vs => 
          assert(bounds(x)._1 < bounds(x)._2)
          if (x == y.s) hilo(vs, env + (x -> bounds(x)._1), bounds)
          else hiloPrefix(vs, y, env + (x -> bounds(x)._1), bounds)
      }

      def realize[T:Type](w: Int, h: Int): Buffer[T] = {
        implicit var fenv = Map[String, Buffer[_]]()
        for ((f,(lx,hx),(ly,hy)) <- getRootStages) {
          if (f.storeRoot) {
              // get x,y rectangle at point of first computation (+ stencil for f)
              val (ux,uy) = widen2(hiloPrefix(order, f.computeAt.get._2, Map(), computeAllBounds(0,0,w,h)), ((lx,hx),(ly,hy)))
              // compute internal size (may be smaller, circular)
              val (ww,hh) = (dim(ux)+1,dim(uy)+1)
              // alloc buffer
              if (trace >= 2) println(s"--- root --- ${f.s} --- alloc buffer  "+((0+lx, 0+ly), (w+hx+1, h+hx+1))+" size "+(ww,hh))
              val buf = new Buffer[T](0+lx, 0+ly, w+hx, h+hy, ww, hh)
              fenv += (f.s -> buf)
          } else {
            fenv += (f.s -> f.realize_internal[T](0+lx, 0+ly, w+hx, h+hy))
          }
        }
        realize_internal[T](0, 0, w, h)
      }

      def realize_internal[T:Type](x0: Int, y0: Int, x1: Int, y1: Int)(implicit fenv: FEnv): Buffer[T] = {
        val buf = new Buffer[T](x0,y0,x1,y1)
        realize_internal(x0,y0,x1,y1,buf)
        buf
      }

      def realize_internal[T:Type](x0: Int, y0: Int, x1: Int, y1: Int, buf: Buffer[T])(implicit fenv: FEnv): Unit = {
        val bounds = computeAllBounds(x0,y0,x1,y1)

        def loop(vs: List[String], env: Env, fenv: FEnv): Unit = vs match {
          case Nil => 
            // inside innermost loop, evaluate body
            implicit val env1 = computeAllVars(env,bounds)
            implicit val fenv1 = fenv
            val i = env1(impl.x.s)
            val j = env1(impl.y.s)
            val res = eval[T](impl.body)
            //if (trace >= 3) println(s"$s $i,$j = $res")
            if (trace >= 3) println(s"$s $i,$j")
            buf(i,j) = res

          case x::vs => 
            for (i <- bounds(x)._1 until bounds(x)._2) {
              val env1 = env + (x -> i)
              implicit var fenv1 = fenv
              for ((f,(lx,hx),(ly,hy)) <- getInternalStages(Var(x))) {
                // get current x,y rectangle (+ stencil for f)
                val ((lx2,hx2),(ly2,hy2)) = widen2(hilo(vs, env1, bounds), ((lx,hx),(ly,hy)))

                if (!(fenv1 contains f.s)) { // create a new buffer
                  // store_at or compute_at without previous store_root or store_at

                  if (f.storeAt == Some((this, Var(x)))) {
                    // get x,y rectangle at point of first computation (+ stencil for f)
                    val (ux,uy) = widen2(hiloPrefix(vs, f.computeAt.get._2, env1, bounds), ((lx,hx),(ly,hy)))
                    // compute internal size (may be smaller, circular)
                    val (w,h) = (dim(ux)+1,dim(uy)+1)

                    if (trace >= 2) println(s"--- $x --- ${f.s} --- alloc buffer  "+((lx2, ly2), (hx2+1, hy2+1))+" size "+(w,h))
                    if (trace >= 2) if (hy2+1-ly2 > h) println("sliding in y")
                    if (trace >= 2) if (hx2+1-lx2 > w) println("sliding in x")

                    fenv1 += (f.s -> new Buffer[T](lx2, ly2, hx2+1, hy2+1, w, h))
                  } else {
                    assert(f.computeAt == Some((this, Var(x))))
                    if (trace >= 2) println(s"--- $x --- ${f.s} --- alloc+fill buffer  "+((lx2, ly2), (hx2+1, hy2+1)))
                    fenv1 += (f.s -> f.realize_internal[T](lx2, ly2, hx2+1, hy2+1))
                  }
                } else { // already have a buffer
                  assert(f.computeAt == Some((this, Var(x)))) // compute_at with previous store_root or store_at
                  val buf = fenv1(f.s).asInstanceOf[Buffer[T]]

                  // SCANLINE OPT:
                  // we're computing only the last scanline (per dimension)
                  // if we have pre-allocated storage for a larger window,
                  // and we're not at the 0 coordinate in that window
                  // BIG ASSUMPTION:
                  // we're iterating low to high in increments of 1 !!
                  val (xAtStart, yAtStart) = (buf.x0 == lx2, buf.y0 == ly2)
                  val (lx3, ly3) = (if (xAtStart) lx2 else hx2, if (yAtStart) ly2 else hy2)

                  if (trace >= 2) println(s"--- $x --- ${f.s} --- fill buffer "+((lx3, ly3), (hx2+1, hy2+1))+s" ${if(xAtStart)""else"!"}${if(yAtStart)""else"!"}")

                  f.realize_internal[T](lx3, ly3, hx2+1, hy2+1, buf)
                }
              }

              loop(vs, env1, fenv1)
            }
        }

        // todo: vectorize/unroll/parallel are currently no-ops for execution

        loop(order, Map(),fenv)
      }

      def print_loop_nest(): Unit = {
        val fs = getRootStages:+(this,(0,0),(0,0))
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
            if (p._1.computeAt == Some((this, Var(x)))) {
              println(s"sub $p {")
              p._1.print_loop_nest()
              println("}")
            } else println(s"alloc $p")
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

    class Buffer[T:Type](val x0: Int, val y0: Int, val x1: Int, val y1: Int, val w: Int, val h: Int) {
      def this(x0: Int, y0: Int, x1: Int, y1: Int) = 
        this(x0,y0,x1,y1,x1-x0,y1-y0)
      val data: Array[Array[T]] = Array.ofDim[T](h, w)
      def apply(x: Int, y: Int) = data((y-y0)%h)((x-x0)%w)
      def update(x: Int, y: Int, v: T) = data((y-y0)%h)((x-x0)%w) = v
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



}