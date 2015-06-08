/*
 * Copyright (c) 2011-15 Peter Schmitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.petomat
import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.collection.breakOut

package object geom {

  private trait =!=[A, B]
  private object =!= {
    implicit def neq[A, B]: A =!= B = null
    // This pair excludes the A =:= B case
    implicit def neqAmbig1[A]: A =!= A = null
    implicit def neqAmbig2[A]: A =!= A = null
  }

  trait DefaultsTo[T, S]
  trait DefaultsToLowPriority {
    implicit def fallback[T, S]: DefaultsTo[T, S] = null
  }
  object DefaultsTo extends DefaultsToLowPriority {
    implicit def defaultDefaultsTo[T]: DefaultsTo[T, T] = null
  }

  // TODO range is limited, e.g. 16 bit for coordinates, that means a max value of only 32767. IDEA: 
  //       // see https://www.securecoding.cert.org/confluence/display/java/NUM00-J.+Detect+or+prevent+integer+overflow
  //       private[this] def safeAdd(a: Long, b: Long): Long = {
  //         if ((b > 0) && (a > Long.MaxValue - b) ||
  //           (b < 0) && (a < Long.MinValue - b)) throw new IllegalArgumentException("integer overflow")
  //         a + b
  //       }

  final type SHORTAT[S <: Spatial, +L <: Location] = Short { type Spatial = S; type Location <: L }
  implicit final def orderingShortAt[S <: Spatial, L <: Location]: Ordering[SHORTAT[S, L]] = Ordering by (_.asInstanceOf[Short])
  implicit final def autoShortAtInt[S <: Spatial, L <: Location](i: Int): SHORTAT[S, L] = i.toShort.asInstanceOf[SHORTAT[S, L]]

  final type Bounds = Rectangle
  final type Position = Point
  final type Pnt[L0 <: Location] = Point { type L = L0 }
  final type Pos[L <: Location] = Pnt[L]

  private final def halfShort(s: Short): Short = (s >> 1).toShort // double2Short(s / 2D)
  final def double2Short(d: Double): Short = {
    require(d >= Short.MinValue && d <= Short.MaxValue)
    d.toShort // d.round.toShort ?
  }

  // http://stackoverflow.com/questions/7337526/how-to-tell-if-a-32-bit-int-can-fit-in-a-16-bit-short
  private final def intFitsShort(i: Int): Boolean = (((i & 0xffff8000) + 0x8000) & 0xffff7fff) == 0

  sealed trait Spatial
  object Spatial {
    sealed trait Coord extends Spatial
    object Coord {
      trait X extends Coord
      trait Y extends Coord
    }
    sealed trait Delta extends Spatial
    object Delta {
      trait DX extends Delta
      trait DY extends Delta
    }
    type X = SHORTAT[Coord.X, Nothing]
    type Y = SHORTAT[Coord.Y, Nothing]
    type DX = SHORTAT[Delta.DX, Nothing]
    type DY = SHORTAT[Delta.DY, Nothing]
  }

  sealed trait Location {
    def normalizedOffsetFromCenter: Size
  }
  sealed abstract class LocationImpl(val normalizedOffsetFromCenter: Size) { _: Location => }
  object Location {
    trait Center extends Location
    sealed trait Corner extends Location
    implicit case object Center extends LocationImpl(Size.zero) with Center
    trait UpperLeft extends Corner
    trait LowerLeft extends Corner
    trait UpperRight extends Corner
    trait LowerRight extends Corner
    implicit case object UpperLeft extends LocationImpl(Size(-1, -1)) with UpperLeft
    implicit case object LowerLeft extends LocationImpl(Size(-1, +1)) with LowerLeft
    implicit case object UpperRight extends LocationImpl(Size(+1, -1)) with UpperRight
    implicit case object LowerRight extends LocationImpl(Size(+1, +1)) with LowerRight
    val corners = Seq[Corner](UpperLeft, UpperRight, LowerRight, LowerLeft)
    sealed trait Side extends Location
    trait Top extends Side
    trait Right extends Side
    trait Bottom extends Side
    trait Left extends Side
    implicit case object Top extends LocationImpl(Size(0, -1)) with Top
    implicit case object Right extends LocationImpl(Size(+1, 0)) with Right
    implicit case object Bottom extends LocationImpl(Size(0, +1)) with Bottom
    implicit case object Left extends LocationImpl(Size(-1, 0)) with Left
    val sides = Seq[Side](Top, Right, Bottom, Left)
  }

  import Location._

  // ======== SIZE ================================================

  final class Size(private[geom] val i: Int) extends AnyVal {
    def dx: Spatial.DX = i >> 16
    def dy: Spatial.DY = i
    def projectX: Size = Size(dx, 0)
    def projectY: Size = Size(0, dy)
    def +(size: Size): Size = Size(dx + size.dx, dy + size.dy)
    def -(size: Size): Size = Size(dx - size.dx, dy - size.dy)
    def *(factor: Double): Size = Size(dx * factor, dy * factor)
    def *(factorX: Double, factorY: Double): Size = Size(dx * factorX, dy * factorY)
    def *(size: Size): Size = Size(dx * size.dx, dy * size.dy)
    def /(factor: Double): Size = Size(dx / factor, dy / factor)
    def /(other: Size): (Double, Double) = (dx.toDouble / other.dx.toDouble, dy.toDouble / other.dy.toDouble)
    def >(other: Size): Boolean = dx > other.dx && dy > other.dy
    def >=(other: Size): Boolean = dx >= other.dx && dy >= other.dy
    def <(other: Size): Boolean = dx < other.dx && dy < other.dy
    def <=(other: Size): Boolean = dx <= other.dx && dy <= other.dy
    def length: Double = math.sqrt(dx.toDouble * dx.toDouble + dy.toDouble * dy.toDouble)
    def unary_- : Size = Size(-dx, -dy)
    def abs: Size = Size(dx.abs, dy.abs)
    def signum: Size = Size(dx.signum, dy.signum)
    def minComponentwise(other: Size) = Size(dx min other.dx, dy min other.dy)
    def maxComponentwise(other: Size) = Size(dx max other.dx, dy max other.dy)
    def half: Size = Size(halfShort(dx), halfShort(dy))
    def swap: Size = Size(dy: Short, dx: Short)
    def toRectangle: Rectangle = Rectangle(Point.zero, this)
    def asPoint[L <: Location]: Pnt[L] = Point(i).at[L] // for performance we reuse the underlying int
    def isZero = i == 0 // this == Size.zero
    def area: Size.Area = dx * dy
    override def toString = s"Size($dx,$dy)"
  }

  object Size {
    type Area = Int
    private[geom] def apply(i: Int): Size = new Size(i)
    private def applyChecked(dx: Int, dy: Int) = Size((dx << 16) | (dy.toShort & 0xffff))
    // originally we had type short for dx and dy but operating on two shorts results in an int
    // and then the double-apply method is called which results in unnecessary rounding/converting
    // (same for equi)
    // so we are now using type int for dx and dy:
    def apply(dx: Int, dy: Int): Size = {
      require(intFitsShort(dx) && intFitsShort(dy))
      applyChecked(dx, dy)
    }
    def apply(dx: Double, dy: Double): Size = applyChecked(double2Short(dx), double2Short(dy))
    def unapply(s: Size): Option[(Spatial.DX, Spatial.DY)] = Some((s.dx, s.dy))
    def equi(s: Int): Size = Size(s, s)
    def equi(d: Double): Size = Size(d, d)
    def dx(dx: Short): Size = Size(dx, 0)
    def dy(dy: Short): Size = Size(0, dy)
    val zero: Size = Size(0, 0)
    implicit def intTuple2Size(t: (Int, Int)): Size = Size(t._1, t._2)
    implicit def doubleTuple2Size(t: (Double, Double)): Size = Size(t._1, t._2)
  }

  // ======== POINT ================================================

  final class Point(private[geom] val i: Int) extends AnyVal {
    type L <: Location
    def at[L <: Location]: Pnt[L] = asInstanceOf[Pnt[L]]
    def x: Spatial.X = (i >> 16)
    def y: Spatial.Y = i
    def +(s: Size): Pnt[L] = Point[L]((x + s.dx), (y + s.dy))
    def -(s: Size): Pnt[L] = Point[L]((x - s.dx), (y - s.dy))
    def *(factor: Double): Pnt[L] = Point[L](double2Short(x * factor), double2Short(y * factor))
    def /(factor: Double): Pnt[L] = Point[L](double2Short(x / factor), double2Short(y / factor))
    def >=(other: Point): Boolean = x >= other.x && y >= other.y
    def <=(other: Point): Boolean = x <= other.x && y <= other.y
    def >(other: Point): Boolean = x > other.x && y > other.y
    def <(other: Point): Boolean = x < other.x && y < other.y
    def distanceTo(p: Point): Double = (this.asSize - p.asSize).length
    def swap: Point = Point(y: Short, x: Short)
    def asSize: Size = Size(i) // for performance we reuse the underlying int
    def moveInto(r: Rectangle): Pnt[L] = {
      val moveDownRight: Point => Point = p => (r.pointAt[UpperLeft] :: p :: Nil).maxPointComponentwise
      val moveUpLeft: Point => Point = p => (r.pointAt[LowerRight] :: p :: Nil).minPointComponentwise
      (moveDownRight andThen moveUpLeft apply this).at[L]
    }
    def to(other: Point): Line = Line(this, other)
    override def toString = s"Point($x,$y)"
  }

  object Point {
    private[geom] def apply(i: Int) = new Point(i)
    private def applyChecked[L <: Location](x: Spatial.X, y: Spatial.Y): Pnt[L] = {
      Point((x.toInt << 16) | (y & 0xffff)).at[L]
    }
    // originally we had type short for x and y but operating on two shorts results in an int
    // and then the double-apply method is called which results in unnecessary rounding/converting
    // (same for equi)
    // so we are now using type int for x and y:
    def apply[L <: Location](x: Int, y: Int)(implicit defaults: L DefaultsTo Location.UpperLeft): Pnt[L] = {
      require(intFitsShort(x) && intFitsShort(y))
      applyChecked(x, y)
    }
    def apply[L <: Location](x: Double, y: Double)(implicit defaults: L DefaultsTo Location.UpperLeft): Pnt[L] = {
      applyChecked(double2Short(x), double2Short(y))
    }
    def unapply(p: Point): Option[(Spatial.X, Spatial.Y)] = Some((p.x, p.y))
    def zero[L <: Location](implicit defaults: L DefaultsTo Location.UpperLeft) = applyChecked[L](0, 0)
    implicit def tuple2Point(t: (Int, Int)): Point = Point(t._1, t._2)
  }

  final implicit class PointCol[Col <: Traversable[Point]](private val col: Col) extends AnyVal {
    def minPointComponentwise: Point = Point(col.minBy(_.x: Short).x, col.minBy(_.y: Short).y)
    def maxPointComponentwise: Point = Point(col.maxBy(_.x: Short).x, col.maxBy(_.y: Short).y)
  }

  // ======== LINE ================================================

  final class Line(private val l: Long) extends AnyVal {
    def from: Point = Point((l >> 32).toInt)
    def to: Point = Point(l.toInt)
    def delta: Size = to.asSize - from.asSize
    def +(s: Size): Line = Line(from + s, to + s)
    def length: Double = (to.asSize - from.asSize).length
    override def toString = s"Line($from,$to)"
  }

  object Line {
    def apply(from: Point, to: Point): Line = new Line((from.i.toLong << 32) | (to.i & 0xffffffffL))
    def apply(fromTo: (Point, Point)): Line = Line(fromTo._1, fromTo._2)
    def unapply(l: Line): Option[(Point, Point)] = Some((l.from, l.to))
  }

  // ======== SHORTINTERVAL ================================================

  final class ShortInterval(private val i: Int) extends AnyVal {
    def fromInclusive: Short = (i >> 16).toShort
    def toExclusive: Short = i.toShort
    private[this] def thisIsToTheLeftOf(i: ShortInterval): Boolean = fromInclusive < i.fromInclusive && toExclusive <= i.fromInclusive
    private[this] def thisIsToTheRightOf(i: ShortInterval): Boolean = fromInclusive >= i.toExclusive && toExclusive > i.toExclusive
    def -(i: ShortInterval): Seq[ShortInterval] = {
      if (i.size <= 0 || thisIsToTheLeftOf(i) || thisIsToTheRightOf(i)) Seq(this)
      else {
        def leftResultInterval = ShortInterval(fromInclusive, i.fromInclusive)
        def rightResultInterval = ShortInterval(i.toExclusive, toExclusive)
        val leftResultIntervalSizeGreaterZero = i.fromInclusive > fromInclusive
        val rightResultIntervalSizeGreaterZero = toExclusive > i.toExclusive
        if (leftResultIntervalSizeGreaterZero && rightResultIntervalSizeGreaterZero) Seq(leftResultInterval, rightResultInterval)
        else {
          if (leftResultIntervalSizeGreaterZero) Seq(leftResultInterval)
          else {
            if (rightResultIntervalSizeGreaterZero) Seq(rightResultInterval) else Seq()
          }
        }
      }
    }
    def intersectionOpt(i: ShortInterval): Option[ShortInterval] = {
      if (thisIsToTheLeftOf(i) || thisIsToTheRightOf(i)) None else Some(ShortInterval(i.fromInclusive max fromInclusive, toExclusive min i.toExclusive))
    }
    def isDisjunctWith(i: ShortInterval): Boolean = intersectionOpt(i).isEmpty
    def size: Short = (toExclusive - fromInclusive).toShort
    def contains(s: Short): Boolean = fromInclusive <= s && s < toExclusive
    override def toString = s"ShortInterval(fromInclusive=$fromInclusive,toExclusive=$toExclusive)"
  }

  object ShortInterval {
    def apply(fromInclusive: Short, toExclusive: Short): ShortInterval = {
      require(fromInclusive <= toExclusive)
      new ShortInterval((fromInclusive.toInt << 16) | (toExclusive & 0xffff))
    }
    def unapply(si: ShortInterval): Option[(Short, Short)] = Some((si.fromInclusive, si.toExclusive))
  }

  // ======== RECTANGLE ================================================

  final class Rectangle(private val l: Long) extends AnyVal {
    // BY CENTER: private[this] def center: Pnt[Center] = Point((l >> 32).toInt).at[Center]
    // BY CENTER: def pointAt[L <: Location](implicit l: L): Pnt[L] = (center + ((l.normalizedOffsetToCenter * size).half)).at[L]
    private[this] def upperLeft: Pnt[UpperLeft] = Point((l >> 32).toInt).at[UpperLeft]
    def size: Size = Size(l.toInt)
    def pointAt[L <: Location](implicit l: L): Pnt[L] = {
      // TODO more cases for performance reasons?
      l match {
        case UpperLeft  => upperLeft.at[L]
        case LowerRight => (upperLeft + size).at[L]
        case _          => (upperLeft + (size * (l.normalizedOffsetFromCenter - UpperLeft.normalizedOffsetFromCenter)).half).at[L]
      }
    }
    def x: Spatial.X = upperLeft.x
    def y: Spatial.Y = upperLeft.y
    def dx: Spatial.DX = size.dx
    def dy: Spatial.DY = size.dy
    def isEmpty: Boolean = size.isZero
    def area: Size.Area = size.area
    def lineAt[S <: Side](implicit side: S): Line = {
      side match {
        case Top    => Line(pointAt[UpperLeft], pointAt[UpperRight])
        case Right  => Line(pointAt[UpperRight], pointAt[LowerRight])
        case Bottom => Line(pointAt[LowerLeft], pointAt[LowerRight])
        case Left   => Line(pointAt[UpperLeft], pointAt[LowerLeft])
      }
    }
    def width: Spatial.DX = size.dx
    def height: Spatial.DY = size.dy
    def containsDirect(p: Point, ul: Pnt[UpperLeft] = pointAt[UpperLeft], lr: Pnt[LowerRight] = pointAt[LowerRight]): Boolean = {
      p >= pointAt[UpperLeft] && p < pointAt[LowerRight] // left and bottom border are not included !!!
    }
    def movedBy(delta: Size) = Rectangle(pointAt[UpperLeft] + delta, size)
    def moveInto(r: Rectangle): Rectangle = {
      val cropped = Rectangle(r.pointAt[UpperLeft], (r.size - this.size) maxComponentwise Size.zero)
      val c = pointAt[Center] moveInto cropped
      val s = r.size minComponentwise this.size
      Rectangle(c, s)
    }
    def resizeWithFixedCenter(factor: Double): Rectangle = Rectangle(pointAt[Center], size * factor)
    def resizeWithFixedCenter(s: Size): Rectangle = Rectangle(pointAt[Center], size + s)
    def makeFittingInSquare: Rectangle = {
      val s = size
      val dx = s.dx
      val dy = s.dy
      val offset: Size = {
        if (dx > dy) Size.dy((dx - dy) / 2)
        else if (dx < dy) Size.dx((dy - dx) / 2)
        else Size.zero
      }
      val sideLength = dx min dy
      Rectangle(pointAt[UpperLeft] + offset, Size(sideLength, sideLength))
    }
    def contains(p: Point): Boolean = size > Size.zero && containsDirect(p)
    def contains(r: Rectangle): Boolean = {
      size > Size.zero && {
        val ul = pointAt[UpperLeft]
        val lr = pointAt[LowerRight]
        containsDirect(r.pointAt[UpperLeft], ul, lr) && containsDirect(r.pointAt[LowerRight], ul, lr)
      }
    }
    def intersects(r: Rectangle): Boolean = { // see http://stackoverflow.com/a/306332
      val ulThis = pointAt[UpperLeft]
      val lrThat = r.pointAt[LowerRight]
      if (ulThis.x < lrThat.x) {
        // for performance reasons, create the following points only if necessary:
        val ulThat = r.pointAt[UpperLeft]
        val lrThis = pointAt[LowerRight]
        lrThis.x > ulThat.x && ulThis.y < lrThat.y && lrThis.y > ulThat.y
      } else false
    }
    def intersectionOpt(r: Rectangle): Option[Rectangle] = { // see http://stackoverflow.com/questions/19753134/get-the-points-of-intersection-from-2-rectangles
      if (this.isEmpty || r.isEmpty) None
      else {
        val ulThis = pointAt[UpperLeft]
        val lrThat = r.pointAt[LowerRight]
        if (ulThis.x < lrThat.x) {
          // for performance reasons, create the following points only if necessary:
          val lrThis = pointAt[LowerRight]
          val ulThat = r.pointAt[UpperLeft]
          val ul = Point[UpperLeft](ulThis.x max ulThat.x, ulThis.y max ulThat.y)
          val lr = Point[LowerRight](lrThis.x min lrThat.x, lrThis.y min lrThat.y)
          val size = lr.asSize - ul.asSize
          if (size.dx <= 0 || size.dy <= 0) None else Some(Rectangle(ul, size)) // Achtung: <= (kleiner gleich)
        } else None
      }
    }
    def intersection(r: Rectangle): Rectangle = intersectionOpt(r) getOrElse Rectangle.empty
    def -(r: Rectangle): Set[Rectangle] = { // zero to eight result rectangles
      def toSet(intervalOpt: Option[ShortInterval]): Set[ShortInterval] = if (intervalOpt.isDefined) Set(intervalOpt.get) else Set()
      val ul = pointAt[UpperLeft]
      val lr = pointAt[LowerRight]
      val ulR = r.pointAt[UpperLeft]
      val lrR = r.pointAt[LowerRight]
      val dX1 = ShortInterval(ul.x, lr.x)
      val dY1 = ShortInterval(ul.y, lr.y)
      val dX2 = ShortInterval(ulR.x, lrR.x)
      val dY2 = ShortInterval(ulR.y, lrR.y)
      val subtractsX: Seq[ShortInterval] = dX1 - dX2
      val subtractsY: Seq[ShortInterval] = dY1 - dY2
      val corners: Set[Rectangle] = {
        subtractsX.flatMap { sX =>
          subtractsY map { sY =>
            Rectangle(Point(sX.fromInclusive, sY.fromInclusive), Size(sX.size, sY.size))
          }
        }(breakOut)
      }
      val topAndBottom: Set[Rectangle] = {
        for {
          i <- toSet(dX1 intersectionOpt dX2)
          sY <- subtractsY
        } yield Rectangle(Point(i.fromInclusive, sY.fromInclusive), Size(i.size, sY.size))
      }
      val leftAndRight: Set[Rectangle] = {
        for {
          i <- toSet(dY1 intersectionOpt dY2)
          sX <- subtractsX
        } yield Rectangle(Point(sX.fromInclusive, i.fromInclusive), Size(sX.size, i.size))
      }
      corners ++ topAndBottom ++ leftAndRight
    }
    def move[L <: Location](delta: Size)(implicit location: L, ev1: L =!= Nothing, ev2: L =!= Center): Rectangle = { // moves the point at location L, but the point at the opposite site stays fixed
      val triedSizeChange = delta * location.normalizedOffsetFromCenter
      val newSize = (size + triedSizeChange) maxComponentwise Size.zero
      val offset = location.normalizedOffsetFromCenter.signum * (newSize - size)
      Rectangle[L](pointAt[L] + offset, newSize) // newSize >= Size.zero
    }
    def resizeBy(f: Size => Size): Rectangle = Rectangle(upperLeft, f(size))
    override def toString = toStrFixed
    //  def toStr = s"[(${pointAt[UpperLeft].x},${pointAt[UpperLeft].y})-(${size.dx},${size.dy})-(${pointAt[LowerRight].x},${pointAt[LowerRight].y})]"
    def toStr = s"[(${pointAt[UpperLeft].x},${pointAt[UpperLeft].y})-(${pointAt[LowerRight].x},${pointAt[LowerRight].y})]"
    def toStrFull = s"Rectangle((${pointAt[UpperLeft].x},${pointAt[UpperLeft].y})|--(${size.dx},${size.dy})--|(${pointAt[LowerRight].x},${pointAt[LowerRight].y}))"
    def toStrFixed = f"[(${pointAt[UpperLeft].x}%4d,${pointAt[UpperLeft].y}%4d) - (${pointAt[LowerRight].x}%4d,${pointAt[LowerRight].y}%4d)]"
  }

  object Rectangle {
    private def byUpperLeft(upperLeft: Pnt[UpperLeft], size: Size): Rectangle = {
      val s = if (size >= Size.zero) size else {
        System.err.println(s"[WARNING]: Rectangle.size = $size") // TODO eliminate cases of occurrence
        size maxComponentwise Size.zero
      }
      new Rectangle((upperLeft.i.toLong << 32) | (s.i & 0xffffffffL))
    }
    def apply[L <: Location](point: Pnt[L], size: Size)(implicit defaults: L DefaultsTo Location.UpperLeft, l: L): Rectangle = {
      // TODO more cases for performance reasons?
      val p: Point = {
        l match {
          case UpperLeft  => point
          case LowerRight => point - size
          case _          => point - ((l.normalizedOffsetFromCenter - UpperLeft.normalizedOffsetFromCenter) * size).half
        }
      }
      byUpperLeft(p.at[UpperLeft], size)
    }
    def fromTo(from: Pnt[UpperLeft], to: Pnt[LowerRight]): Rectangle = apply(from, to.asSize - from.asSize)
    // def apply[L1 <: Location, L2 <: Location](from: Pnt[L1], to: Pnt[L2])(implicit ev: L1 =!= L2, l1: L1, l2: L2): Rectangle = ???
    val empty: Rectangle = apply(Point.zero[UpperLeft], Size.zero)
    def empty[L <: Location](p: Pnt[L])(implicit defaults: L DefaultsTo Location.UpperLeft, location: L): Rectangle = apply(p, Size.zero)
  }

  final implicit class RectangleCol[Col <: Traversable[Rectangle]](private val rectangles: Col) extends AnyVal {
    def deadSpaceArea(boundingBoxOpt: Option[Rectangle] = None): Size.Area = {
      val bb = boundingBoxOpt getOrElse rectangles.boundingBox
      @tailrec def rec(deadArea: Size.Area, processed: Seq[Rectangle], toProcess: Traversable[Rectangle]): Size.Area = {
        if (deadArea <= 0 || toProcess.isEmpty) deadArea max 0
        else {
          val current: Rectangle = if (boundingBoxOpt.isDefined) toProcess.head intersection bb else toProcess.head // intersect with boundingBoxOpt because boundingBoxOpt may be smaller than toProcess.head
          val newDeadArea = deadArea - current.size.area + processed.view.map(_.intersection(current).size.area).sum
          rec(newDeadArea, processed :+ current, toProcess.tail)
        }
      }
      rec(bb.size.area, Seq.empty, rectangles)
    }
    def boundingBoxOpt: Option[Rectangle] = {
      if (rectangles.isEmpty) None else { // TODO this can be done faster, e.g. dont traverse reectangles twice
        Some(
          Rectangle.fromTo(
            rectangles.map(_.pointAt[UpperLeft]).minPointComponentwise.at[UpperLeft],
            rectangles.map(_.pointAt[LowerRight]).maxPointComponentwise.at[LowerRight]
          )
        )
      }
    }
    def boundingBox: Rectangle = boundingBoxOpt getOrElse Rectangle.empty
  }

}

