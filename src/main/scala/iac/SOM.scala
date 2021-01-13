package iac


import breeze.linalg.{DenseMatrix, DenseVector, argmax}

case class Element(var name: String, var activation: Double)

object CompetitiveLearningFunctions {
  def connect(srce: CLPool, trgt: CLPool): Unit = {
    srce.next = trgt
    trgt.prev = srce
    srce.clusters.foreach {c => connectToUpperLayer(c,trgt)}
  }

  def connectToUpperLayer(cl: Cluster, pool: CLPool): Unit = {
    pool.clusters.foreach {i =>
      cl.excitation = new Projector {
        var src : Cluster = cl
        var tgt : Cluster = i
        var xDim : Int =src.units.length
        var yDim : Int = tgt.units.length
        var connections : DenseMatrix[Double] = DenseMatrix.zeros[Double](xDim,yDim)
        for (j <- 0 until xDim)
          for (i<- 0 until yDim)
            connections(j,i) = 1.0/cl.container.size
      } :: cl.excitation
    }
  }
}


abstract class CLNetwork {
  var pools: List[CLPool]
  var inputPool: CLPool

  def subjectTo(projs: Unit*): CLNetwork = this

  def report(): Unit = {
    pools foreach {p => p.report}
  }

  def setPattern(v: DenseVector[Double]): Unit = {
    inputPool.clusters.head.units = v
  }

  def update: Unit = {
    var p: CLPool = inputPool
    p.activeCount()
    do {
      p.feedForward()
      p = p.next
      p.selectUnits()
    } while (p.next != null)

  }

  def train(data: List[Array[Double]]): Unit = {

  }

}

trait CompetitiveLearning {
  val DEFAULT = 0.001
  val epsilon = 0.2
  val decay : Double  = 0.01
  val min : Double = -1.0
  val max : Double = 1.0
  val rest : Double = 0.0
  val maxIterations : Int = 200
  var N : Int = 0
  var delta : Double = 0.02
  val deltaThreshold = 0.01
  var r = scala.util.Random
}

abstract class BasePool {
  def report : Unit
  var activeCounter : Int = 0
}

abstract class Projector {
  var connections : DenseMatrix[Double]
  var src: Cluster
  var tgt: Cluster

}

object Cluster {
  def apply(p: CLPool,v: DenseVector[Double]): Cluster = {
    new Cluster(p,v)
  }
}

class Cluster(p: CLPool, v: DenseVector[Double]) extends BasePool
  with CompetitiveLearning {self =>
  var container: CLPool = p
  var units : DenseVector[Double] = v
  var excitation : List[Projector] = List[Projector]()
  var inhibition : Projector = new Projector {
    var src: Cluster = self
    var tgt: Cluster = self
    var connections: DenseMatrix[Double] =
      DenseMatrix.tabulate[Double](units.length, units.length) {
        case (i, j) if (i == j) => 0
        case (i, j) if (i != j) => -1
      }
  }

  def feedForward(): Unit = {
    if (!excitation.isEmpty) {
      excitation foreach { c =>
        (0 until c.tgt.units.length) foreach { u =>
          var net = (c.connections(::, u) *:* c.src.units) reduce (_ + _)
          c.tgt.units(u) += net
        }
      }
    }
  }

  def selectUnit(): Unit = {
    var max : Int = argmax(units)
    (0 until units.length) foreach {i => units(i) = 0.0}
    units(max) = 1.0
    container.activeCounter += 1
    updateConnections(max)
  }

  def updateConnections(m: Int): Unit = {
    container.prev.clusters foreach {c =>
      c.excitation foreach {p=>
        if (p.tgt == this) {
          (0 until p.connections.rows) foreach {r =>
            var total  = p.src.container.activeCounter
            if (p.src.units(r) > 0)
              p.connections(r,m) += (epsilon/total)-(epsilon*p.connections(r,m))
            else
              p.connections(r,m) += -(epsilon*p.connections(r,m))
          }
        }
      }
    }
  }

  override def report(): Unit = {
    println(units)
    println(inhibition.connections)
    if (excitation!=null) {
      excitation.foreach(x => println(x.connections))
    }
  }
}



abstract class CLPool extends BasePool with CompetitiveLearning {
  var next : CLPool = null
  var prev : CLPool = null
  def count = clusters.length
  def size : Int = {
    var unitCount = 0
    clusters foreach(c => unitCount += c.units.length)
    unitCount
  }

  var clusters : List[Cluster] = null
  def add(c: Cluster): Unit = {
    clusters = c :: clusters
  }

  def reset(): Unit = {
    activeCounter = 0
  }


  def feedForward(): Unit = {
    clusters.foreach {c => c.feedForward()}
  }

  def selectUnits(): Unit = {
    clusters.foreach {c => c.selectUnit()}
  }

  def activeCount(): Unit = {
    clusters foreach { c =>
      c.units.foreach {u =>
        if (u > 0.0) activeCounter += 1}
    }
  }

  def report: Unit = {
    clusters foreach {c => c.report}

  }
}


object CLTest {
  def apply(): Unit = {
    import CompetitiveLearningFunctions._
    var input = new CLPool {self=>
      clusters = List[Cluster](
        Cluster(self,DenseVector(0.0, 0.0, 0.2, 0.0, 0.2)))
    }

    var layer1 = new CLPool {self=>
      clusters = List[Cluster](
        Cluster(self,DenseVector(0.0, 0.0, 0.0)),
        Cluster(self,DenseVector(0.0, 0.0, 0.0)),
        Cluster(self,DenseVector(0.0, 0.0, 0.0)))
    }

    var out = new CLPool {self=>
      clusters  = List[Cluster](
        Cluster(self,DenseVector(0.0, 0.0, 0.0, 0.0, 0.0)))
    }



    var clNet = new CLNetwork {
      var pools : List[CLPool] = List(input,layer1,out)
      var inputPool : CLPool = input
    } subjectTo(
      connect(input, layer1),
      connect(layer1, out))

    clNet.report()
    clNet.update
    clNet.report()
  }
}

class SOM {

}