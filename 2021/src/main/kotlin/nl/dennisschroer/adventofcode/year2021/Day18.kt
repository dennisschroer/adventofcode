package nl.dennisschroer.adventofcode.year2021

class Day18 {
    abstract class Node(var parent: Node?) {
        fun depth(): Int = if (parent == null) 1 else 1 + (parent as Node).depth()
    }

    data class PairNode(var pair: Pair<Node, Node>) : Node(null)
    data class ValueNode(var value: Int) : Node(null)

    data class Value(val value: Int, val depth: Int)

    fun part1(input: List<String>): Int {
        return input.map { parseToFish(it) }.reduce(this::addAndReduce).let { magnitude(it) }
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    private fun addAndReduce(fish1: List<Value>, fish2: List<Value>): List<Value> {
        return reduce(flattenFish(PairNode(fish1 to fish2)))
    }

    private fun reduce(fish: List<Value>): List<Value> {
        return explodeOrSplit(fish)
    }

    private fun explodeOrSplit(fish: Node): Node {
        // Lets walk the tree
        var currentNode: Node? = fish
        while (currentNode is PairNode) {
            currentNode = currentNode.pair.first
        }

        var done = false
        while (!done && currentNode != null) {
            if (currentNode.depth() >= 4) {
                explode(currentNode as PairNode)
                done = true
            }

            currentNode = right(currentNode)
        }

    }

    private fun explode(node: PairNode) {
        left(node)?.let {
            it.value = it.value + (node.pair.first as ValueNode).value
        }
        right(node)?.let {
            it.value = it.value + (node.pair.second as ValueNode).value
        }

        val parent = node.parent as PairNode
        if (node == parent.pair.first) {
            parent.pair = ValueNode(0) to parent.pair.second
        }
        if (node == parent.pair.second) {
            parent.pair = parent.pair.first to ValueNode(0)
        }
    }

    private fun left(fish: Node): ValueNode? {
        var currentNode = fish

        // Traverse up
        while (currentNode.parent != null && currentNode.parent is PairNode && currentNode == (currentNode.parent as PairNode).pair.second) {
            currentNode = currentNode.parent as PairNode
        }

        // Exception: if this is the leftmost node currentNode is a value
        if (currentNode is ValueNode) return null

        // Go left
        currentNode = (currentNode as PairNode).pair.first

        // Go right until we hit a value
        while (currentNode is PairNode) {
            currentNode = currentNode.pair.second
        }

        return currentNode as ValueNode
    }

    private fun right(fish: Node): ValueNode? {
        var currentNode = fish

        // Traverse up
        while (currentNode.parent != null && currentNode.parent is PairNode && currentNode == (currentNode.parent as PairNode).pair.first) {
            currentNode = currentNode.parent as PairNode
        }

        // Exception: if this is the rightmost node currentNode is a value
        if (currentNode is ValueNode) return null

        // Go right
        currentNode = (currentNode as PairNode).pair.second

        // Go left until we hit a value
        while (currentNode is PairNode) {
            currentNode = currentNode.pair.first
        }

        return currentNode as ValueNode
    }


    private fun magnitude(fish: List<Pair<Int, Int>>): Int {
        return -1
    }

    fun flattenFish(fish: Node, depth: Int = 1): List<Value> {
        if (fish is PairNode) {
            return flattenFish(fish.pair.first, depth + 1) + flattenFish(fish.pair.second, depth + 1)
        }
        return listOf(Value((fish as ValueNode).value, depth))
    }

    fun parseToFish(line: String): List<Value> {
        val (node, result) = parseToNode(line)
        assert(result.isEmpty())
        return flattenFish(node)
    }

    fun parseToNode(line: String): Pair<Node, String> {
        return if (line[0] == '[') {
            val (subNode1, remaining1) = parseToNode(line.drop(1))
            val (subNode2, remaining2) = parseToNode(remaining1.drop(1))
            val parent = PairNode(subNode1 to subNode2)
            subNode1.parent = parent
            subNode2.parent = parent
            parent to remaining2.drop(1)
        } else {
            ValueNode(line[0].digitToInt()) to line.drop(1)
        }
    }
}

fun main() {
    println("Part 1: ${Day18().part1(readLines("day18"))}")
    println("Part 2: ${Day18().part2(readLines("day18"))}")
}