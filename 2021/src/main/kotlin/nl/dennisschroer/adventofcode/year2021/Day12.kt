package nl.dennisschroer.adventofcode.year2021

class Day12 {
    data class Graph(val nodes: Set<Node>)
    data class Node(val id: String, val edges: MutableList<Edge>) {
        override fun equals(other: Any?): Boolean = if (other is Node) this.id == other.id else false
        override fun hashCode(): Int = id.hashCode()
        override fun toString(): String = id
    }
    data class Edge(val start: Node, val end: Node)
    data class Path(val nodes: List<Node>)

    fun part1(input: List<String>): Int {
        val graph = inputToGraph(input)
        return findAllPaths(graph.nodes.first { it.id == "start" }, graph.nodes.first { it.id == "end" }, listOf()).size
    }

    /**
     * Find all possible paths from start to end by recursively walking each edge from the start node.
     */
    private fun findAllPaths(start: Node, end: Node, visited: List<Node>): Set<Path> {
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

    fun part2(input: List<String>): Int {
        val graph = inputToGraph(input)
        return findAllPathsWhileVisitingOneSmallCaveTwice(graph.nodes.first { it.id == "start" }, graph.nodes.first { it.id == "end" }, listOf()).size
    }

    /**
     * Find all possible paths from start to end where one of the small nodes can be visited twice.
     */
    private fun findAllPathsWhileVisitingOneSmallCaveTwice(start: Node, end: Node, visited: List<Node>): Set<Path> {
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
    }

    private fun inputToGraph(input: List<String>): Graph {
        return Graph(input.fold(setOf()) { nodes, line ->
            val nodeA = line.substringBefore('-').let { id -> nodes.find { it.id == id } ?: Node(id, mutableListOf()) }
            val nodeB = line.substringAfter('-').let { id -> nodes.find { it.id == id } ?: Node(id, mutableListOf()) }
            nodeA.edges.add(Edge(nodeA, nodeB))
            nodeB.edges.add(Edge(nodeB, nodeA))

            nodes + nodeA + nodeB
        })
    }
}

fun main() {
    println("Part 1: ${Day12().part1(readLines("day12"))}")
    println("Part 2: ${Day12().part2(readLines("day12"))}")
}