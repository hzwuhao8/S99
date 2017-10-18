package s99

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[T, U] => ((nodes.keys.toSet -- g.nodes.keys.toSet).isEmpty &&
      (edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet).isEmpty)
    case _ => false
  }

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }
  def toTermForm(): (List[T], List[(T,T,U)]) ={
    (nodes.keys.toSet.toList, edges.map{ _.toTuple} )
  }
}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[T, U] => super.equals(g)
    case _              => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }
}

object Graph {

  def term2[T](nodeList: List[T], edgeList: List[(T, T)]) = {
    val tmp = edgeList.map { x => (x._1, x._2, ()) }
    term(nodeList, tmp)
  }

  def term[T, U](nodeList: List[T], edgeList: List[(T, T, U)]) = {
    val g = new Graph[T, U]
    nodeList.map { n => g.addNode(n) }
    nodeList.map { n =>
      {
        val subList = edgeList.filter(_._1 == n)
        subList.foreach(x => g.addEdge(x._1, x._2, x._3))
      }
    }
    g
  }
  def adjacent2[T](edgesList: List[(T, List[T])]) = {
    val tmp = edgesList.map { e => (e._1, e._2.map { n => (n, ()) }) }
    adjacent(tmp)
  }
  def adjacent[T, U](edgesList: List[(T, List[(T, U)])]) = {
    val g = new Graph[T, U]
    edgesList.foreach { e => g.addNode(e._1) }
    edgesList.map { e =>
      g.addNode(e._1)
      e._2.foreach { n =>
        g.addEdge(e._1, n._1, n._2)
      }
    }
    g
  }

  def fromString(str: String): Graph[String, Unit] = {
    val str2 = str.drop(1).dropRight(1).mkString
    val arr1 = str2.split(",");
    val arr2 = arr1.map { s =>
      val arr3 = s.split("-")
      arr3
    }
    val nodeList = arr2.flatten.toSet.toList
    val edgesList = arr2.filter(_.size == 2).map { arr => (arr(0), arr(1)) }.toList
    term2(nodeList, edgesList)

  }

  
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[T, U] => super.equals(g)
    case _                => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }
}

object Digraph {
  def termLabel[T, U](nodeList: List[T], edgeList: List[(T, T, U)]) = {
    val g = new Digraph[T, U]
    nodeList.map { n => g.addNode(n) }
    nodeList.map { n =>
      {
        val subList = edgeList.filter(_._1 == n)
        subList.foreach(x => g.addArc(x._1, x._2, x._3))
      }
    }
    g
  }

  def adjacentLabel[T, U](edgesList: List[(T, List[(T, U)])]) = {
    val g = new Digraph[T, U]
    edgesList.foreach { e => g.addNode(e._1) }
    edgesList.map { e =>
      g.addNode(e._1)
      e._2.foreach { n =>
        g.addArc(e._1, n._1, n._2)
      }
    }
    g
  }
}
