package iac

import iac.IACModelState._
import iac.IACImplicitConversions._


case class Conflict(name: String)
case class Desire(name: String)
case class Location(name: String)
case class Event(name: String)
case class Action(name: String)
case class Participant(name: String)
case class Resolution(name: String)

case class NarrativeData(id: String, conflict: Conflict, goal: Desire,
                         location: Location, event: Event, participant: Participant, action: Action, resolution: Resolution)

class LinkGenerator(s: Tuple2[Pool[IANode,Link],String]) {
  def <--> (t: Tuple2[Tuple2[String,String],Double]) : Link = {
    Link((s._1, s._2), (poolMap(t._1._1),t._1._2), t._2)
  }
}

object IACImplicitConversions {
  implicit def str2LinkGen(s: Tuple2[Pool[IANode,Link],String]): LinkGenerator =
    new LinkGenerator(s)

  implicit def str2LinkGen1(s: Tuple2[String,String]): LinkGenerator = {
    var transformed : Tuple2[Pool[IANode,Link],String] = (poolMap(s._1),s._2)
    new LinkGenerator(transformed)
  }
}



object IACUtilities {
  val DEFAULT_EXCITATION = 1.0

  def ParseData(specs : List[NarrativeData]) :  Map[String, Set[String]] = {
    narrativeSpec = specs
    var poolMap = Map[String, Set[String]]()

    poolMap += ("narratives" -> Set())
    poolMap += ("hidden" -> Set())
    poolMap += ("conflict" -> Set())
    poolMap += ("goal" -> Set())
    poolMap += ("location" -> Set())
    poolMap += ("event" -> Set())
    poolMap += ("participant" -> Set())
    poolMap += ("action" -> Set())
    poolMap += ("resolution" -> Set())

    specs foreach { n => {
      n match {
        case NarrativeData(id, c, g, l, e, p, a, r) =>
          if (id != "") {
            poolMap += ("narratives" -> (poolMap("narratives") + id))
            poolMap += ("hidden" -> (poolMap("hidden") + id))
          }
          if (c.name != "") poolMap += ("conflict" -> (poolMap("conflict") + c.name))
          if (g.name != "") poolMap += ("goal" -> (poolMap("goal") + g.name))
          if (l.name != "") poolMap += ("location" -> (poolMap("location") + l.name))

        case _ =>
      }
    }}
    poolMap
  }

  def initPools(poolNames : List[String], poolMap: Map[String, Set[String]]) : List[Pool[IANode, Link]] = {
    var poolList = List[Pool[IANode, Link]]()
    poolNames foreach {pn =>
      poolList = Pool(pn,poolMap(pn)) :: poolList
    }
    poolList
  }

  def setProjections(narratives : List[NarrativeData]): Unit = {
    var projectionList = Map[Tuple2[String,String],List[Tuple2[String,String]]](
      ("hidden","narratives") -> List(),
      ("hidden","conflict") -> List(),
      ("hidden","goal") -> List(),
      ("hidden","location") -> List()
    )

    narratives foreach { n => {
      n match {
        case NarrativeData(id, c, g, l, e, p, a, r) => {
          projectionList += (("hidden", "narratives") -> ((id, id) :: projectionList(("hidden", "narratives"))))
          projectionList += (("hidden", "conflict") -> ((id, c.name) :: projectionList(("hidden", "conflict"))))
          projectionList += (("hidden", "goal") -> ((id, g.name) :: projectionList(("hidden", "goal"))))
          projectionList += (("hidden", "location") -> ((id, l.name) :: projectionList(("hidden", "location"))))
        }
        case _ =>
      }
    }}


    projectionList foreach {pr => {
      pr._1 match {
        case (x,y) => {
          var links = List[Link]()
          pr._2 foreach {m => links = ((x, m._1) <--> ((y, m._2), DEFAULT_EXCITATION)) :: links}
          projection(x,y)(links)
        }
        case _ =>
      }

    }}
  }

  def apply(): Unit = {
    var narratives: List[NarrativeData] = List(
      NarrativeData("HumanitarianCrisis", Conflict("CivilWar"), Desire("ProtectCivillians"), Location("Syria"), Event("Gas"),
        Participant(""), Action("CeaseFire"), Resolution("Settlement")),
      NarrativeData("Assad", Conflict("Revolution"), Desire("RegimeSurvival"), Location("Syria"), Event("ArabSpring"),
        Participant("Russia"), Action("AttackRebels"), Resolution("MilitaryVictory")),
      NarrativeData("ISIS", Conflict("ThreatToUmmah"), Desire("ProtectIslam"), Location("Syria"), Event("ArabSpring"),
        Participant(""), Action("DefendMosul"), Resolution("Caliphate")),
      NarrativeData("US(GWOT)", Conflict("Terrorism"), Desire("ProtectUSInterest"), Location("Iraq"), Event(""),
        Participant("US"), Action("AssistIraqiArmy"), Resolution("DefeatISIS"))
    )


    var iac = new IACModel[IANode, Link](
      initPools(List(
        "narratives",
        "hidden",
        "conflict",
        "goal",
        "location"),
        ParseData(narratives))
    )
    {
      setActivation("narratives","HumanitarianCrisis",1.0)
      setActivation("conflict","CivilWar",1.0)
      setActivation("goal","ProtectCivillians",1.0)
      setActivation("location","Syria",1.0)
    } subjectTo(
      narratives => setProjections(narratives)
      )

    iac.report()

    iac.simulate()
    iac.reportActivations()


  }
}


object IACDSLTest {
  def apply() {
    var iac = new IACModel[IANode,Link] {
      pools = List(
        Pool("P3",
          proposition("A",1.00),
          proposition("B",0.01),
          proposition("C",0.04)
        ),
        Pool("P4",
          proposition("D",0.03),
          proposition("E",0.03)
        ),
        Pool("P5",
          proposition("X",0.01),
          proposition("Y",0.01),
          proposition("Z",0.01),
        )
      )
    } subjectTo(
      projection("P5", "P3") {List(
        ("P5", "X") <--> (("P3", "A"), 1.00),
        ("P5", "Y") <--> (("P3", "B"), 1.00),
        ("P5", "Z") <--> (("P3", "C"), 1.00)
      )}, projection("P5", "P4") {List(
      ("P5", "X") <--> (("P4","D"), 1.00),
      ("P5", "Y") <--> (("P4", "E"), 1.00),
      ("P5", "Z") <--> (("P4", "E"), 1.00)
    )}
    )
  }
}