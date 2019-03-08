package ch.epfl.lara.engine

import java.time.{Duration, Instant}

/**
  * This package contains some basic geometrical classes to represents points and intervals in time and space
  *
  * @author Louis Vialar
  */
package object geometry {

  case class SpatialCoordinates(latitude: BigDecimal, longitude: BigDecimal, altitude: BigDecimal) {
    def -(other: SpatialCoordinates): (BigDecimal, BigDecimal, BigDecimal) = (latitude - other.latitude, longitude - other.longitude, altitude - other.altitude)
  }

  /**
    * A point in time and space
    *
    * @param space the spatial coordinates of the point
    * @param time  the time coordinate of the point
    */
  case class Point(space: SpatialCoordinates, time: Instant)

  /**
    * An interval in time and space
    */
  trait Interval {
    def isInside(point: Point): Boolean

    /**
      * Compute the union of this interval with an other interval
      *
      * @param other the other interval to union with the current interval
      * @return an interval containing all points either in the current interval or in the `other` interval
      */
    def union(other: Interval): Interval = Union(this, other)

    /**
      * Compute the intersection of this interval with an other interval
      *
      * @param other the other interval to intersect with the current interval
      * @return an interval containing all points that are in both the current interval and the `other` interval
      */
    def intersection(other: Interval): Interval = Intersection(this, other)
  }

  /**
    * An interval in time and space built from two corners
    *
    * @param c1 the "bottom left" corner of the interval (lowest x,y,z,t)
    * @param c2 the "top right" corner of the interval (highest x,y,z,t)
    */
  case class Cuboid(c1: Point, c2: Point) extends Interval {
    def duration: Duration = Duration.between(c1.time, c2.time)

    /**
      * Compute the space covered by this cuboid
      *
      * @return a triple (delta-latitude, delta-longitude, delta-altitude)
      */
    def geodesicSpace: (BigDecimal, BigDecimal, BigDecimal) = c2.space - c1.space

    override def isInside(point: Point): Boolean = {
      def inInterval[T <: Ordered[T]](getter: Point => T): Boolean =
        getter(point) >= getter(c1) && getter(point) <= getter(c2)

      inInterval(_.space.longitude) && inInterval(_.space.latitude) && inInterval(_.space.altitude) //&& inInterval(_.time)
    }
  }

  private case class Union(i1: Interval, i2: Interval) extends Interval {
    override def isInside(point: Point): Boolean = i1.isInside(point) || i2.isInside(point)
  }

  private case class Intersection(i1: Interval, i2: Interval) extends Interval {
    override def isInside(point: Point): Boolean = i1.isInside(point) && i2.isInside(point)
  }

}
