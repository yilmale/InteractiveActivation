package iac

import scalAgent._
import breeze.linalg._



class Constraint(val p1: Proposition = null, val p2: Proposition =null, val w: Double = 0.0) extends IAEdge
case class Explanation(override val p1: Proposition, override val
p2: Proposition, override val w: Double) extends Constraint
case class Compatible(override val p1: Proposition, override val
p2: Proposition, override val w: Double) extends Constraint
case class Contradict(override val p1: Proposition, override val
p2: Proposition, override val w: Double) extends Constraint


class Proposition(val prop : String = null, val weight : Double = 1.0) extends IANode
case class Belief(override val prop : String, override val weight : Double = 1.0) extends Proposition
case class Evidence(override val prop : String, override val weight : Double = 1.0) extends Proposition
case class Goal(override val prop : String, override val weight : Double = 1.0) extends Proposition
case class Effect(override val prop : String, override val weight : Double = 0.0, activity : Activity = null) extends Proposition


object Declarations {
  var CM : Narrative = null

  def addConstraint(s: String, t: String, w: Double) : Constraint = {
    var p1 = CM.propositions(s)
    var p2 = CM.propositions(t)
    var c : Constraint = null
    if (w >= 0) c = Explanation(p1,p2,w)
    else c = Contradict(p1,p2,w)
    CM.constraints = c :: CM.constraints
    c
  }

  def addCompatibilityConstraint(s: String, t: String, w: Double) : Constraint = {
    require(w > 0)
    var p1 = CM.propositions(s)
    var p2 = CM.propositions(t)
    var c = Compatible(p1,p2,w)
    CM.constraints = c :: CM.constraints
    c
  }

  def addProposition(s: String, p: Proposition): Unit = {
    CM.propositions += (s -> p)
    CM.nodes += (CM.pCount -> p )
    CM.propId += (p -> CM.pCount)
    CM.pCount+=1
  }

  def explain(s: String, t: String, w: Double): Constraint = addConstraint(s,t,w)

  def explain(p1: Proposition, p2: Proposition, w: Double): Constraint = addConstraint(p1.prop,p2.prop,w)

  def explain(s: List[String], t: String, w: Double) : Constraint = {
    var weight : Double = w/s.length
    s.foreach(i => addConstraint(i,t,weight))
    pairs(s) foreach (p => addCompatibilityConstraint(p._1,p._2,weight))
    CM.constraints.head
  }

  def explain(s: List[Proposition], t: Proposition, w: Double) : Constraint = {
    var weight : Double = w/s.length
    s.foreach(i => addConstraint(i.prop,t.prop,weight))
    var contents : List[String] = List[String]()
    s foreach {p => contents = p.prop :: contents }
    pairs(contents) foreach (p => addCompatibilityConstraint(p._1,p._2,weight))
    CM.constraints.head
  }

  def facilitate(s: String, t: String, w: Double): Constraint = addConstraint(s,t,w)

  def facilitate(p1: Proposition, p2: Proposition, w: Double): Constraint = addConstraint(p1.prop,p2.prop,w)

  def facilitate(s: List[String], t: String, w: Double) : Constraint = {
    var weight : Double = w/s.length
    s.foreach(i => addConstraint(i,t,weight))
    pairs(s) foreach (p => addCompatibilityConstraint(p._1,p._2,weight))
    CM.constraints.head
  }

  def deduce(s: String, t: String, w: Double): Constraint = addConstraint(s,t,w)

  def deduce(p1: Proposition, p2: Proposition, w: Double): Constraint = addConstraint(p1.prop,p2.prop,w)

  def deduce(s: List[String], t: String, w: Double) : Constraint = {
    var weight : Double = w/s.length
    s.foreach(i => addConstraint(i,t,weight))
    pairs(s) foreach (p => addCompatibilityConstraint(p._1,p._2,weight))
    CM.constraints.head
  }

  def contradict(s: String, t: String, w: Double): Constraint = addConstraint(s,t,-w)
  def contradict(p1: Proposition, p2: Proposition, w: Double): Constraint = addConstraint(p1.prop,p2.prop,-w)

  def evidence(s: String, w: Double) : Unit = addProposition(s,Evidence(s,w))

  def belief(s: String, w: Double) : Unit = addProposition(s,Belief(s,w))

  def goal(s: String, w: Double) : Unit = addProposition(s,Goal(s,w))

  def effect(s: String, w: Double, a : Activity = null) : Unit = addProposition(s,Effect(s,w,a))

  def pairs(L : List[String]) : List[Tuple2[String,String]] = {
    L match {
      case x :: y  if y.length == 1 => List((x,y.head))
      case x :: y  if y.length > 1  => pairs(y) ::: (for (f <- y) yield (x,f) )
      case _ => List()
    }
  }
}


object Narrative {

  def apply(body: => Unit): Narrative = {
    new Narrative {
      body
    }
  }
}


class Narrative {self =>
  import Declarations._
  var IACModel : ConstraintSatisfactionModel = null
  var pCount :Int = 0
  var propositions = scala.collection.mutable.Map[String,Proposition]()
  var nodes  = scala.collection.mutable.Map[Int,Proposition]()
  var propId = scala.collection.mutable.Map[Proposition,Int]()
  var constraints = List[Constraint]()
  CM = this

  def add_Evidence(s: String, w: Double): Unit = {
    evidence(s,w)
  }

  def add_Belief(s: String, w: Double): Unit = {
    belief(s,w)
  }

  def add_Goal(s: String, w: Double): Unit = {
    goal(s,w)
  }

  def add_Action(s: String, w: Double): Unit = {
    effect(s,w)
  }

  def add_Explanation(s: String, t: String, w: Double): Unit = {
    explain(s,t,w)
  }

  def add_Facilitation(s: String, t: String, w: Double): Unit = {
    facilitate(s,t,w)
  }

  def add_Deduction(s: String, t: String, w: Double): Unit = {
    deduce(s,t,w)
  }

  def add_Contradiction(s: String, t: String, w: Double): Unit = {
    contradict(s,t,w)
  }

  def generateConstraintModel(): ConstraintSatisfactionModel = {
    IACModel = new ConstraintSatisfactionModel() {
      var narrative: Narrative = self
      var N : Int = pCount
      var activations : ActivationVector = DenseVector.tabulate[Double](N) {i => nodes(i).weight}
      var initialState: ActivationVector = DenseVector.tabulate[Double](N) {i => nodes(i).weight}
      var constraintMatrix : ConstraintMatrix = DenseMatrix.zeros[Double](N,N)

      for (c <- constraints) {
        val node1 : Int = propId(c.p1)
        val node2 : Int = propId(c.p2)
        constraintMatrix(node1,node2) = c.w
        constraintMatrix(node2,node1) = c.w
      }
    }
    IACModel
  }

  def subjectTo(cnts: Constraint*): ConstraintSatisfactionModel = {
    IACModel = new ConstraintSatisfactionModel() {
      var narrative: Narrative = self
      var N : Int = pCount
      var activations : ActivationVector = DenseVector.tabulate[Double](N) {i => nodes(i).weight}
      var initialState: ActivationVector = DenseVector.tabulate[Double](N) {i => nodes(i).weight}
      var constraintMatrix : ConstraintMatrix = DenseMatrix.zeros[Double](N,N)

      for (c <- constraints) {
        val node1 : Int = propId(c.p1)
        val node2 : Int = propId(c.p2)
        constraintMatrix(node1,node2) = c.w
        constraintMatrix(node2,node1) = c.w
      }
    }
    IACModel
  }

}