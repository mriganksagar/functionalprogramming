package barneshut

import java.awt.*
import java.awt.event.*
import javax.swing.*
import javax.swing.event.*
import scala.{collection => coll}
import scala.collection.parallel.{TaskSupport, Combiner}
import scala.collection.parallel.mutable.ParHashSet
import scala.collection.parallel.CollectionConverters.*

class Simulator(val taskSupport: TaskSupport, val timeStats: TimeStatistics):

  def updateBoundaries(boundaries: Boundaries, body: Body): Boundaries =
    val updatedBoundary = new Boundaries
    updatedBoundary.minX = math.min(boundaries.minX, body.x)
    updatedBoundary.minY = math.min(boundaries.minY, body.y)
    updatedBoundary.maxX = math.max(boundaries.maxX, body.x)
    updatedBoundary.maxY = math.max(boundaries.maxY, body.y)
    updatedBoundary
  end updateBoundaries

  def mergeBoundaries(a: Boundaries, b: Boundaries): Boundaries =
    val mergedBoundary = new Boundaries
    mergedBoundary.minX = math.min(a.minX, b.minX)
    mergedBoundary.minY = math.min(a.minY, b.minY)
    mergedBoundary.maxX = math.max(a.maxX, b.maxX)
    mergedBoundary.maxY = math.max(a.maxY, b.maxY)
    mergedBoundary
  end mergeBoundaries

  def computeBoundaries(bodies: coll.Seq[Body]): Boundaries = timeStats.timed("boundaries") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.aggregate(Boundaries())(updateBoundaries, mergeBoundaries)
  }

  def computeSectorMatrix(bodies: coll.Seq[Body], boundaries: Boundaries): SectorMatrix = timeStats.timed("matrix") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    bodies.aggregate(new SectorMatrix(boundaries, SECTOR_PRECISION))(
      (sectorMatrix, body) => sectorMatrix += body,
      (sm1, sm2) => sm1.combine(sm2)
    )           
  }

  def computeQuad(sectorMatrix: SectorMatrix): Quad = timeStats.timed("quad") {
    sectorMatrix.toQuad(taskSupport.parallelismLevel)
  }

  def updateBodies(bodies: coll.Seq[Body], quad: Quad): coll.Seq[Body] = timeStats.timed("update") {
    val parBodies = bodies.par
    parBodies.tasksupport = taskSupport
    parBodies.map(_.updated(quad)).seq
  }

  def eliminateOutliers(bodies: coll.Seq[Body], sectorMatrix: SectorMatrix, quad: Quad): coll.Seq[Body] = timeStats.timed("eliminate") {
    def isOutlier(b: Body): Boolean =
      val dx = quad.massX - b.x
      val dy = quad.massY - b.y
      val d = math.sqrt(dx * dx + dy * dy)
      // object is far away from the center of the mass
      if d > eliminationThreshold * sectorMatrix.boundaries.size then
        val nx = dx / d
        val ny = dy / d
        val relativeSpeed = b.xspeed * nx + b.yspeed * ny
        // object is moving away from the center of the mass
        if relativeSpeed < 0 then
          val escapeSpeed = math.sqrt(2 * gee * quad.mass / d)
          // object has the espace velocity
          -relativeSpeed > 2 * escapeSpeed
        else false
      else false

    def outliersInSector(x: Int, y: Int): Combiner[Body, ParHashSet[Body]] =
      val combiner = ParHashSet.newCombiner[Body]
      combiner ++= sectorMatrix(x, y).filter(isOutlier)
      combiner

    val sectorPrecision = sectorMatrix.sectorPrecision
    val horizontalBorder = for x <- 0 until sectorPrecision; y <- Seq(0, sectorPrecision - 1) yield (x, y)
    val verticalBorder = for y <- 1 until sectorPrecision - 1; x <- Seq(0, sectorPrecision - 1) yield (x, y)
    val borderSectors = horizontalBorder ++ verticalBorder

    // compute the set of outliers
    val parBorderSectors = borderSectors.par
    parBorderSectors.tasksupport = taskSupport
    val outliers = parBorderSectors.map({ case (x, y) => outliersInSector(x, y) }).reduce(_ combine _).result()

    // filter the bodies that are outliers
    val parBodies = bodies.par
    parBodies.filter(!outliers(_)).seq
  }

  def step(bodies: coll.Seq[Body]): (coll.Seq[Body], Quad) =
    // 1. compute boundaries
    val boundaries = computeBoundaries(bodies)

    // 2. compute sector matrix
    val sectorMatrix = computeSectorMatrix(bodies, boundaries)

    // 3. compute quad tree
    val quad = computeQuad(sectorMatrix)

    // 4. eliminate outliers
    val filteredBodies = eliminateOutliers(bodies, sectorMatrix, quad)

    // 5. update body velocities and positions
    val newBodies = updateBodies(filteredBodies, quad)

    (newBodies, quad)

