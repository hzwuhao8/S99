package s99

abstract class GraphBase[T, U] extends Log {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    def degree: Int = neighbors.size

  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[T, U] => (
      (nodes.keys.toSet -- g.nodes.keys.toSet).isEmpty &&
      (edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet).isEmpty)
    case _ => false
  }

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }
  def toTermForm(): (List[T], List[(T, T, U)]) = {
    (nodes.keys.toSet.toList, edges.map { _.toTuple })
  }
  def toAdjacentForm(): List[(T, List[(T, U)])] = {
    val tt: List[T] = nodes.keys.toSet.toList
    tt.map { my: T =>
      val e: List[(T, U)] = edges.filter(_.n1.value == my).map { x => (x.n2.value, x.value) }
      (my, e)
    }
  }
  def findPaths(from: T, to: T): List[List[T]] = {
    findPathsIner(from, to, from)
  }
  def findPathsIner(from: T, to: T, orig: T): List[List[T]] = {
    val fromOpt = nodes.get(from)
    val toOpt = nodes.get(to)
    val res: List[List[T]] = (fromOpt, toOpt) match {
      case (None, _)                    => Nil
      case (_, None)                    => Nil
      case (Some(f), Some(t)) if f == t => Nil
      case (Some(f), Some(t)) =>
        val p1 = if (f.neighbors.contains(t)) {
          List(f.value, t.value) :: Nil
        } else {
          Nil
        }
        val neig = f.neighbors.map { _.value }.filter(x => (x != f && x != t && x != orig))
        trace(s"neig=${neig}")
        val p2 = neig.flatMap { x =>
          val pp = findPathsIner(x, to, orig)
          pp.map { f.value :: _ }
        }
        p1 ::: p2
    }
    res
  }

  def removeEdge(e: Edge) = {
    edges.filterNot(_ == e)
    e.n1.adj.filterNot(_ == e)
    e.n2.adj.filterNot(_ == e)

  }

  def isConnected(): Boolean = {
    nodes.forall(kv => !kv._2.neighbors.isEmpty)
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

  def findCycles(a: T): List[List[T]] = {
    val naOpt = nodes.get(a)
    val res = naOpt match {
      case None => Nil
      case Some(na) =>
        na.neighbors.flatMap { n =>
          trace(s"findPath=${n.value} ->  ${a}")
          val paths = findPaths(n.value, a)
          trace(s"paths=${paths}")
          paths.filterNot(_ == Nil).map { a :: _ }
        }
    }
    res.filterNot(_.size <= 3)
  }

  def isTree(): Boolean = {
    edges.forall(e => {
      removeEdge(e)
      val flag = isConnected() == false
      flag
    })
  }

  def edgeConnectsToGraph[T, U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2)) // xor

  def spanningTrees = {
    def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): List[Graph[T, U]] = {
      if (graphNodes == Nil) List(Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple)))
      else if (graphEdges == Nil) Nil
      else graphEdges.filter(edgeConnectsToGraph(_, graphNodes)) flatMap { ge =>
        spanningTreesR(graphEdges.filterNot(_ == ge),
          graphNodes.filter(edgeTarget(ge, _) == None),
          ge :: treeEdges)
      }
    }
    spanningTreesR(edges, nodes.values.toList.tail, Nil).distinct
  }

  def minimalSpanningTree(implicit f: (U) => Ordered[U]) = {
    def minimalSpanningTreeR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): Graph[T, U] =
      if (graphNodes == Nil) Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple))
      else {
        val nextEdge = graphEdges.filter(edgeConnectsToGraph(_, graphNodes)).reduceLeft((r, e) => if (r.value < e.value) r else e)
        minimalSpanningTreeR(graphEdges.filterNot(_ == nextEdge),
          graphNodes.filter(edgeTarget(nextEdge, _) == None),
          nextEdge :: treeEdges)
      }
    minimalSpanningTreeR(edges, nodes.values.toList.tail, Nil)
  }

  /**
   * TODO
   *  f is AnyF    T <=> S
   *  2^(nodes.size-1)
   */
  def isIsomorphicTo[S, V](g: Graph[S, V]): Boolean = {
    val zip = this.nodes.keys.zip(scala.util.Random.shuffle(g.nodes.keys))
    debug(s"zip=${zip}")
    def f(x: T): S = {
      val z = zip.find(_._1 == x)
      z.map(_._2).get
    }
    val flag = (this.nodes.size == g.nodes.size) && (this.edges.size == g.edges.size)
    if (flag) {
      val eA = edges.map { _.toTuple }.map { p => (p._1, p._2) }
      val eB = g.edges.map { _.toTuple }.map { p => (p._1, p._2) }
      val ff = eA.map { p => (f(p._1), f(p._2)) }
      eB.toSet == ff.toSet

    } else {
      false
    }
  }

  def nodesByDegree() = {
    val x: List[(Node, Int)] = nodes.map { kv => (kv._2, kv._2.degree) }.toList
    x.sortBy(_._2).reverse
  }

  def colorNodes() = {

    def run2(color: Int, uncolored: List[(Node, Int)], colored: List[(Node, Int)], adj: Set[Node]): (List[(Node, Int)], List[(Node, Int)]) = {
      uncolored match {
        case Nil => (colored, uncolored)
        case a :: as =>
          val t = (a._1, color)
          debug { s"{$t}" }
          val neighbors = a._1.neighbors
          val nextadj: Set[Node] = adj ++ neighbors + a._1
          val l1 = as.filter(p => nextadj.contains(p._1))
          val l2 = as.filterNot(p => nextadj.contains(p._1))
          debug(s"\nl1=${l1}\nl2=${l2}")
          val (cList, ulist) = run2(color, l2, t :: colored, nextadj)
          (cList, l1 ::: ulist)
      }
    }

    def run(seq: List[(Node, Int)], color: Int, res: List[(Node, Int)]): List[(Node, Int)] = {
      seq match {
        case Nil => res
        case a :: as =>
          val t = (a._1, color)
          debug { s"{$t}" }
          val neighbors = a._1.neighbors
          debug(s"neighbors=${neighbors}")
          val connectedList = as.filter { p => neighbors.contains(p._1) }
          val notconnectedList = as.filterNot { p => neighbors.contains(p._1) }
          debug(s"connectedList=${connectedList}")
          debug(s"notconnectedList=${notconnectedList}")
          val (cList, uList) = run2(color, notconnectedList, Nil, Set(a._1))

          run(connectedList ::: uList, color + 1, t :: cList ::: res)
      }
    }

    run(nodesByDegree, 0, Nil)
  }
}

object Graph {

  def term[T](nodeList: List[T], edgeList: List[(T, T)]) = {
    val tmp = edgeList.map { x => (x._1, x._2, ()) }
    termLabel(nodeList, tmp)
  }

  def termLabel[T, U](nodeList: List[T], edgeList: List[(T, T, U)]) = {
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
      val arr3 = s.trim.split("-")
      arr3
    }
    val nodeList = arr2.flatten.toSet.toList
    val edgesList = arr2.filter(_.size == 2).map { arr => (arr(0), arr(1)) }.toList
    term(nodeList, edgesList)

  }

  def fromStringLabel(str: String): Graph[String, Int] = {
    val str2 = str.drop(1).dropRight(1).mkString
    val arr1 = str2.split(",");
    val arr2 = arr1.map { s =>
      s.trim.toList match {
        case List(a)                          => List(a)
        case p :: '-' :: q :: '/' :: v :: Nil => List(p, q, v)
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
    termLabel(nodeList, edgesList)
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

  def fromStringLabel(str: String): Digraph[String, Int] = {
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
    termLabel(nodeList, edgesList)
  }

}
