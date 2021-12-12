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

        val routes = findAllPaths(start, end, visited)
        routes.forEach { println(it) }
        return routes.size
    }

    /**
     * Find all possible paths from start to end by recursively walking each edge from the start node.
     */
    fun findAllPaths(start: Node, end: Node, visited: List<Node>): Set<Path> {
        println("Finding all paths from $start to $end; visited: $visited")

        return start.edges.filter { !visited.contains(it.end) }.map { edge ->
            if (edge.start == end) {
                // Base case
                listOf(Path(listOf(end)))
            } else {
                // Recursive case
                val newVisited = if (start.id.first().isLowerCase()) visited + start else visited
                findAllPaths(edge.end, end, newVisited).map { Path(listOf(start) + it.nodes) }
            }
        }.flatten().toSet()
    }

    fun findAllPathsWhileVisitingOneSmallCaveTwice(start: Node, end: Node, visited: List<Node>): Set<Path> {
        println("Finding all paths from $start to $end; visited: $visited")


        return start.edges.map { edge ->
            val newVisited = if (start.id.first().isLowerCase()) visited + start else visited

            if (edge.end.id == "start") {
                // Exception to the rule: never visit start again
                listOf()
            } else if (edge.start == end) {
                // Base case
                listOf(Path(listOf(end)))
            } else if (visited.contains(edge.end)) {
                // Go back and continue with default pathfinding
                findAllPaths(edge.end, end, newVisited).map { Path(listOf(start) + it.nodes) }
            } else {
                // Recursive case
                findAllPathsWhileVisitingOneSmallCaveTwice(edge.end, end, newVisited).map { Path(listOf(start) + it.nodes) }
            }
        }.flatten().toSet()


        val defaultRoutes = findAllPathsWhileVisitingOneSmallCaveTwice(start, end, visited)

        // When in here, we can also still opt to visit one small cave for the second time
        val backingRoutes = start.edges.filter { visited.contains(it.end) }.map { edge ->
            val newVisited = if (start.id.first().isLowerCase()) visited + start else visited
            findAllPaths(edge.end, end, newVisited)
        }.flatten().toSet()

        return defaultRoutes + backingRoutes
    }

    fun part2(input: List<String>): Int {
        val graph = inputToGraph(input)

        val start = graph.nodes.first { it.id == "start" }
        val end = graph.nodes.first { it.id == "end" }
        val visited = listOf<Node>()

        val routes = findAllPathsWhileVisitingOneSmallCaveTwice(start, end, visited)
        routes.forEach { println(it) }
        return routes.size
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