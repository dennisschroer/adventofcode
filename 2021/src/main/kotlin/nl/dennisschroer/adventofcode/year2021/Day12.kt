package nl.dennisschroer.adventofcode.year2021

class Day12 {
    data class Graph(val nodes: Set<Node>, val edges: List<Edge>)
    data class Node(val id: String, val edges: MutableList<Edge>) {
        override fun equals(other: Any?): Boolean = if (other is Node) this.id == other.id else false
        override fun hashCode(): Int = id.hashCode()
        override fun toString(): String = id
    }

    data class Edge(val start: Node, val end: Node)
    data class Path(val nodes: List<Node>)

    fun part1(input: List<String>): Int {
        val graph = inputToGraph(input)

        val start = graph.nodes.first { it.id == "start" }
        val end = graph.nodes.first { it.id == "end" }
        val visited = listOf<Node>()

        val routes = findAllRoutes(graph, start, end, visited)
        routes.forEach { println(it) }
        return routes.size
    }

    fun findAllRoutes(graph: Graph, start: Node, end: Node, visited: List<Node>): Set<Path> {
        println("Finding all routes from $start to $end; visited: $visited")

        return start.edges.filter { !visited.contains(it.end) }.map { edge ->
            if (edge.start == end) {
                // Base case
                listOf(Path(listOf(end)))
            } else {
                // Recursive case
                val newVisited = if (start.id.first().isLowerCase()) visited + start else visited
                findAllRoutes(graph, edge.end, end, newVisited).map { Path(listOf(start) + it.nodes) }
            }
        }.flatten().toSet()
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    fun inputToGraph(input: List<String>): Graph {
        val edges = mutableListOf<Edge>()
        val nodes = mutableSetOf<Node>()

        input.forEach { line ->
            val nodeA = line.substringBefore('-').let { id -> nodes.find { it.id == id } ?: Node(id, mutableListOf()) }
            val nodeB = line.substringAfter('-').let { id -> nodes.find { it.id == id } ?: Node(id, mutableListOf()) }
            val edge = Edge(nodeA, nodeB)
            nodes.add(nodeA)
            nodes.add(nodeB)
            nodeA.edges.add(Edge(nodeA, nodeB))
            nodeB.edges.add(Edge(nodeB, nodeA))
            edges.add(edge)
        }

        return Graph(nodes, edges)
    }
}

fun main() {
    println("Part 1: ${Day12().part1(readLines("day12"))}")
    println("Part 2: ${Day12().part2(readLines("day12"))}")
}