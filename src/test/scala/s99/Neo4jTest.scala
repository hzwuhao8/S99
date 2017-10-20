package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

import org.neo4j.test.TestGraphDatabaseFactory
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.PathExpanders
import org.neo4j.graphdb.Direction

import scala.collection.convert.ImplicitConversionsToScala._
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory

case class DIS(name: String) extends RelationshipType {

}
class Neo4jTest extends FunSuite with Checkers with Log {
  val mytype = DIS("PATH")
  test("example 11") {
    val graphDb = new TestGraphDatabaseFactory().newImpermanentDatabase();

    val tx = graphDb.beginTx()
    val nOpt = try {
      val n = graphDb.createNode();
      n.setProperty("name", "Nancy");
      tx.success();
      debug(s"n=${n}")
      Some(n)
    } catch {
      case ex: java.lang.Exception => None
    } finally {
      tx.close()
    }
    assert(nOpt != None)
    assert(nOpt.get.getId > -1L)

    graphDb.shutdown()
  }

  test("Neo4j P81") {
    implicit val graphDb = new TestGraphDatabaseFactory().newImpermanentDatabase();
    val tx = graphDb.beginTx()

    val (nodeList, edgesList) = fromString("[p>q/9, m>q/7, k, p>m/5]")
    val nMap = create(nodeList, edgesList)
    val p = nMap("p")

    val finder = GraphAlgoFactory.allPaths(
      PathExpanders.forTypeAndDirection(mytype, Direction.OUTGOING), 15);
    val paths = finder.findAllPaths(nMap("p"), nMap("q")).toList
    val pp = paths.map { path => path.nodes().toList.map { n => n.getProperty("name") } }
    debug(s"paths=${pp.mkString("\n")}")

    tx.close()
    graphDb.shutdown()

    assert(pp.size == 2)
    assert(pp.contains(List("p", "q")))
  }

  test("P84") {
    implicit val graphDb = new TestGraphDatabaseFactory().newImpermanentDatabase();
    val tx = graphDb.beginTx()
    val (nodeList, edgesList) = (
      List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
      List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
        ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
        ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)))

    val nMap = create(nodeList, edgesList)
  
    tx.close()
    graphDb.shutdown()

  }
  def fromString(str: String) = {
    val str2 = str.drop(1).dropRight(1).mkString
    val arr1 = str2.split(",");
    val arr2 = arr1.map { s =>
      s.trim.toList match {
        case List(a)                          => List(a)
        case p :: '>' :: q :: '/' :: v :: Nil => List(p, q, v)
      }
    }
    val nodeList = arr2.flatMap { x =>
      x match {
        case List(a)       => List(a.toString)
        case List(a, b, v) => List(a.toString, b.toString)
      }
    }.toSet.toList
    val edgesList = arr2.filter(_.size == 3).map { arr =>
      (arr(0).toString, arr(1).toString, arr(2).toString.toInt)
    }.toList
    (nodeList, edgesList)
  }

  def create[T, U](nodeList: List[T], edgesList: List[(T, T, U)])(implicit graphDb: GraphDatabaseService) = {
    val nList = nodeList.map { name =>
      val n = graphDb.createNode()
      n.setProperty("name", name)
      (name, n)
    }
    val nMap = nList.toMap
    edgesList.map {
      case (name1, name2, value) =>
        val n1 = nMap(name1)
        val n2 = nMap(name2)
        n1.createRelationshipTo(n2, mytype)
    }

    nList.toMap
  }
}