
// Exercise Session 6
// For comprehensions and monads

// Question 1.1

type NodeId = Int
type DirectedEdge = (NodeId, NodeId)
type DirectedGraph = List[DirectedEdge]

val graph: DirectedGraph = List((1, 2), (2, 3), (3, 1), (2, 4), (4, 1))

def weakTriangles(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] = 
    val seq = for 
        edge1 <- edges
        edge2 <- edges
        edge3 <- edges
        if edge1._2 == edge2._1 && edge2._2 == edge3._1 && edge3._2 == edge1._1
    yield List(edge1._1, edge2._1, edge3._1).sorted
    (for i <- seq yield (i(0), i(1), i(2))).toSet.toList

def triangles(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] = 
    for 
        e1 <- edges if (e1._1 < e1._2)
        e2 <- edges if (e1._1 < e2._1 && e1._1 < e2._2 && e1._2 == e2._1)
        e3 <- edges if (e1._1 < e3._1 && e2._2 == e3._1 && e3._2 == e1._1)
    yield (e1._1, e2._1, e3._1)


// Question 1.2

// triangels with translated 'for-exprs'
def trianglesMap(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] = 
    edges.filter(e1 => e1._1 < e1._2)
    .flatMap(e1 => 
            edges.filter(e2 => (e1._1 < e2._1 && e1._1 < e2._2 && e1._2 == e2._1))
            .flatMap(e2 => 
                    edges.filter(e3 => (e1._1 < e3._1 && e2._2 == e3._1 && e3._2 == e1._1))
                    .map(e3 => (e1._1, e2._1, e3._1))))


triangles(graph)
trianglesMap(graph)
