package iac

import breeze.linalg.{DenseMatrix, DenseVector, shuffle}
import iac.IACModelState._

case class Prop(d: String, a: Double) extends IANode
case class Link(p1: (Pool[IANode,Link],String),
                p2: (Pool[IANode,Link],String), w: Double) extends IAEdge



trait InteractiveActivationCompetition[Node,Edge] {
  val DEFAULT = 0.001
  val decay : Double  = 0.01
  val min : Double = -1.0
  val max : Double = 1.0
  val rest : Double = 0.0
  val maxIterations : Int = 200
  var N : Int = 0
  var delta : Double = 0.02
  val deltaThreshold = 0.01
  var r = scala.util.Random
  type InputVector = DenseVector[Double]
  type ActivationVector = DenseVector[Double]
  def update : Int => ActivationVector
}


object IACModelState {
  var poolId: Int = 0
  var poolList: List[Pool[IANode,Link]] = List[Pool[IANode,Link]]()
  var poolMap: Map[String,Pool[IANode,Link]] = Map[String,Pool[IANode,Link]]()
  var poolNodes: List[IANode] = List[IANode]()
  var narrativeSpec : List[NarrativeData] = null

  def reset(): Unit = {
    poolId = 0
    poolList = List[Pool[IANode,Link]]()
    poolMap = Map[String,Pool[IANode,Link]]()
    poolNodes = List[IANode]()
  }

  def proposition(s: String, a: Double): Prop = {
    var node : Prop = Prop(s,a)
    poolNodes = node :: poolNodes
    node
  }

  def unit(s: String, a: Double): Prop = {
    var node : Prop = Prop(s,a)
    poolNodes = node :: poolNodes
    node
  }

  def projection(p1Name: String, p2Name: String)(lks : List[Link]) : Unit = {
    var p1 = poolMap(p1Name)
    var p2 = poolMap(p2Name)
    p1.projections += (p2 -> new Projection[IANode,Link] {
      var src : Pool[IANode,Link] = p1
      var tgt : Pool[IANode,Link] = p2
      var connections : DenseMatrix[Double] =
        DenseMatrix.zeros[Double](src.nodes.length,tgt.nodes.length)
    })

    p2.projections += (p1 -> new Projection[IANode,Link] {
      var src : Pool[IANode,Link] = p2
      var tgt : Pool[IANode,Link] = p1
      var connections : DenseMatrix[Double] =
        DenseMatrix.zeros[Double](src.nodes.length,tgt.nodes.length)
    })

    for (l <- lks) {
      l match {
        case Link(s,t,w) => {
          var row = s._1.nodeId(s._2)
          var column = t._1.nodeId(t._2)
          s._1.projections(t._1).connections.update(row,column,w)

          row = t._1.nodeMap(t._2)._2
          column = s._1.nodeId(s._2)
          t._1.projections(s._1).connections.update(row,column,w)
        }
        case _ =>
      }
    }
  }

  def projection(p1: Pool[IANode,Link], p2: Pool[IANode,Link])(lks : List[Link]) : Unit = {
    p1.projections += (p2 -> new Projection[IANode,Link] {
      var src : Pool[IANode,Link] = p1
      var tgt : Pool[IANode,Link] = p2
      var connections : DenseMatrix[Double] =
        DenseMatrix.zeros[Double](src.nodes.length,tgt.nodes.length)
    })



    p2.projections += (p1 -> new Projection[IANode,Link] {
      var src : Pool[IANode,Link] = p2
      var tgt : Pool[IANode,Link] = p1
      var connections : DenseMatrix[Double] =
        DenseMatrix.zeros[Double](src.nodes.length,tgt.nodes.length)
    })

    for (l <- lks) {
      l match {
        case Link(s,t,w) => {
          var row = s._1.nodeId(s._2)
          var column = t._1.nodeId(t._2)
          s._1.projections(t._1).connections.update(row,column,w)
          row = t._1.nodeMap(t._2)._2
          column = s._1.nodeId(s._2)
          t._1.projections(s._1).connections.update(row,column,w)
        }
        case _ =>
      }
    }
  }
}


abstract class IACModel[Node,Edge] {
  val maxIterations: Int = 200
  var delta: Double = 0.02
  val deltaThreshold = 0.01
  var pools : List[Pool[Node, Edge]] = List[Pool[Node, Edge]]()
  var hidden : List[Pool[Node, Edge]] = null
  var visible : List[Pool[Node, Edge]] = null

  def this(pls : List[Pool[Node, Edge]]) {
    this
    pools = pls
  }

  def report(): Unit = {
    pools foreach {p => p.report()}
  }

  def reportActivations(): Unit = {
    pools foreach {p => p.reportActivations()}
  }

  def setActivation(pname : String, nodename : String, value : Double): Unit = {
    poolMap(pname).setNodeActivation(nodename,value)
  }

  def subjectTo(projs: Unit*): IACModel[Node,Edge] = {this}

  def subjectTo(projector : List[NarrativeData] => Unit) : IACModel[Node,Edge] = {
    projector(narrativeSpec)
    this
  }



  def simulate(): Unit = {
    var iterations: Int = 0
    var currentDelta: Double = 0.0
    while ((iterations < maxIterations) && (delta > deltaThreshold)) {
      pools foreach { p => {
        p.updateActivations()
        currentDelta = Math.max(currentDelta, p.computeDelta())
      }
        delta = Math.min(delta, currentDelta)
        iterations = iterations + 1
      }
    }
  }
}


abstract class Pool[Node,Edge](identifier: Int) extends
  InteractiveActivationCompetition[Node,Edge] {
  var id : Int
  var name: String
  var nodes : Vector[Node]
  var nodeMap : Map[String,Tuple2[Node,Int]]
  var previousActivations : DenseVector[Double]
  var activations : DenseVector[Double]
  var projections : Map[Pool[Node,Edge],Projection[Node,Edge]]

  def nodeId(s: String) : Int = nodeMap(s)._2

  def getNetInput : Int => Double = {id => {
    var size : Int = projections.size
    var net : Double = 0
    projections.keys foreach {p => {
      var edgeMatrix = p.projections(this).connections
      var input = p.activations
      net += (edgeMatrix(::,id) *:* input) reduce (_ + _)
    }}
    net
  }}

  def update : Int => ActivationVector = {id => {
    var net : Double = getNetInput(id)
    var adelta : Double = 0.0
    if (net > 0) {
      adelta = (max - activations(id))*net - (decay*(activations(id)-rest))
      activations(id)=Math.min(1,activations(id)+adelta)
    }
    else {
      adelta = (activations(id)-min)*net - (decay*(activations(id)-rest))
      activations(id) = Math.max(-1, activations(id)+adelta)
    }
    activations
  }}

  def updateActivations() :  ActivationVector = {
    N = nodes.length
    (0 until activations.length) foreach {i => previousActivations(i) = activations(i)}
    val A = shuffle(DenseVector.tabulate[Int](N){i => i})
    (0 until activations.length)
      .foreach(i => update(A(i)))
    activations
  }

  def computeDelta() : Double = {
    delta = Math.abs(activations(0)-previousActivations(0))
    for(i <- 1 until activations.length) {
      delta = Math.max(delta,Math.abs(activations(i)-previousActivations(i)))
    }
    delta
  }

  def setNodeActivation(n: String, newActivation: Double): Unit

  def reportActivations(): Unit = {
    nodes.indices foreach {i =>
    {
      println(this.nodes(i).asInstanceOf[Prop].d + " " +
        this.activations(i))
    }}
  }

  def getActivations() : List[Tuple2[String,Double]] = {
    var nodeList = List[Tuple2[String,Double]]()
    nodes.indices foreach {i =>
      nodeList = (this.nodes(i).asInstanceOf[Prop].d,this.activations(i)) :: nodeList
    }
    nodeList
  }

  def getMaxActivation() : Tuple2[String,Double] = {
    var nList = getActivations()
    var currentMax : Tuple2[String,Double] = nList.head
    nList foreach {n =>
      if (n._2 > currentMax._2) currentMax = n
    }
    currentMax
  }

  def report(): Unit = {
    nodes.indices foreach {i =>
    {
      println(this.nodes(i).asInstanceOf[Prop].d + " " +
        this.activations(i))
    }}

    println("Pool id: " + this.id)
    this.projections foreach {case (i,j) => {
      println("Projected to " + i.id)
      println(j.connections)
    }}
  }

}

abstract class Projection[Node,Edge] {
  var connections : DenseMatrix[Double]
  var src: Pool[Node,Edge]
  var tgt: Pool[Node,Edge]

}


object Pool {

  def apply(poolName: String, pNodes: IANode*) : Pool[IANode,Link] = {
    import IACModelState._
    poolList = new Pool[IANode,Link](poolId) {self=>
      var name : String = poolName
      var id: Int = poolId
      poolId += 1
      poolMap += (name -> self)
      var nodes : Vector[IANode] = poolNodes.reverse.toVector
      var activations: DenseVector[Double] = new DenseVector[Double](
        (for (n <- nodes.indices) yield nodes(n).asInstanceOf[Prop].a).toArray)
      var previousActivations : DenseVector[Double] =
        DenseVector.tabulate[Double](nodes.length){i => activations(i)}
      poolNodes = List[IANode]()
      var nodeMap: Map[String, Tuple2[IANode, Int]] = Map[String, Tuple2[IANode, Int]]()
      nodes.indices foreach {n =>
        nodeMap += (nodes(n).asInstanceOf[Prop].d -> (nodes(n),n))
      }
      var projections: Map[Pool[IANode, Link], Projection[IANode, Link]] =
        Map(self -> new Projection[IANode, Link] {
          var src: Pool[IANode, Link] = self
          var tgt: Pool[IANode, Link] = self
          var connections: DenseMatrix[Double] = DenseMatrix.tabulate[Double](nodes.length, nodes.length) {
            case (i, j) if (i == j) => 0
            case (i, j) if (i != j) => -1
          }
        })

      def setNodeActivation(n: String, newActivation: Double): Unit = {
        var np = Prop(n,newActivation)
        var index = nodeMap(n)._2
        nodeMap += (n -> (np,index))
        nodes.updated(index,np)
        activations(index) = newActivation
        previousActivations(index) = newActivation
      }

    } :: poolList
    poolList.head
  }


  def apply(poolName: String, pNodes: Set[String]) : Pool[IANode,Link] = {
    poolList = new Pool[IANode,Link](poolId) {self=>
      var name : String = poolName
      var id: Int = poolId
      poolId += 1
      poolMap += (name -> self)
      pNodes foreach {p =>
        proposition(p,DEFAULT)
      }

      var nodes : Vector[IANode] = poolNodes.reverse.toVector
      var activations: DenseVector[Double] = new DenseVector[Double](
        (for (n <- nodes.indices) yield nodes(n).asInstanceOf[Prop].a).toArray)
      var previousActivations : DenseVector[Double] =
        DenseVector.tabulate[Double](nodes.length){i => activations(i)}
      poolNodes = List[IANode]()
      var nodeMap: Map[String, Tuple2[IANode, Int]] = Map[String, Tuple2[IANode, Int]]()
      nodes.indices foreach {n =>
        nodeMap += (nodes(n).asInstanceOf[Prop].d -> (nodes(n),n))
      }
      var projections: Map[Pool[IANode, Link], Projection[IANode, Link]] =
        Map(self -> new Projection[IANode, Link] {
          var src: Pool[IANode, Link] = self
          var tgt: Pool[IANode, Link] = self
          var connections: DenseMatrix[Double] = DenseMatrix.tabulate[Double](nodes.length, nodes.length) {
            case (i, j) if i == j => 0
            case (i, j) if i != j => -1
          }
        })

      def setNodeActivation(n: String, newActivation: Double): Unit = {
        var np = Prop(n,newActivation)
        var index = nodeMap(n)._2
        nodeMap += (n -> (np,index))
        nodes.updated(index,np)
        activations(index) = newActivation
        previousActivations(index) = newActivation
      }

    } :: poolList
    poolList.head
  }
}