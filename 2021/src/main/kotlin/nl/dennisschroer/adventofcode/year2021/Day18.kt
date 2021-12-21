package nl.dennisschroer.adventofcode.year2021

import kotlin.random.Random

class Day18 {
    abstract class Node(var parent: Node?) {
        fun depth(): Int = if (parent == null) 1 else 1 + (parent as Node).depth()
        abstract fun isLike(other : Node) : Boolean
    }

    class PairNode(var pair: Pair<Node, Node>) : Node(null) {
        private val id = Random.nextInt()
        init {
            pair.first.parent = this
            pair.second.parent = this
        }

        override fun isLike(other: Node) = other is PairNode && this.pair.first.isLike(other.pair.first) && this.pair.second.isLike(other.pair.second)
        override fun equals(other: Any?): Boolean = other is PairNode && this.id == other.id
        override fun toString(): String = "[${pair.first},${pair.second}]"
    }

    data class ValueNode(var value: Int) : Node(null) {
        override fun isLike(other: Node) = other is ValueNode && this.value == other.value
        override fun toString(): String = "$value"
    }

    fun part1(input: List<String>): Int {
        return addAndReduceAll(input.map { parseToFish(it) }).let { magnitude(it) }
    }

    fun addAndReduceAll(fishes: List<Node>): Node {
        return fishes.reduce(this::addAndReduce)
    }

    fun part2(input: List<String>): Int {
        return -1
    }

    fun addAndReduce(fish1: Node, fish2: Node): Node {
        println("  $fish1\n+ $fish2")
        val parent = PairNode(fish1 to fish2)
        fish1.parent = parent
        fish2.parent = parent
        val result = reduce(parent)
        println("= $result")
        return result
    }

    fun reduce(fish: Node): Node {
        repeat(1000) { explodeOrSplit(fish) }
        println()
        return fish
    }

    fun explodeOrSplit(fish: Node): Node {
        var done = false

        // Lets walk the tree
        walk(fish, { pairNode: PairNode ->
            if (!done && pairNode.depth() > 4) {
                explode(pairNode)
                done = true
            }
        }, { })

        walk(fish, { }, { valueNode: ValueNode ->
            if (!done && valueNode.value > 9) {
                split(valueNode)
                done = true
            }
        })

        if (done) {
            println("   => $fish")
        } else {
            print("*")
        }

        return fish
    }

    fun walk(fish: Node, visitPair: (pairNode: PairNode) -> Unit, visitValue: (valueNode: ValueNode) -> Unit) {
        if (fish is PairNode) {
            walk(fish.pair.first, visitPair, visitValue)
            visitPair(fish)
            walk(fish.pair.second, visitPair, visitValue)
        } else {
            visitValue(fish as ValueNode)
        }
    }

    private fun explode(node: PairNode) {
        println("Exploding $node")
        left(node)?.let {
            println("  Left: $it")
            it.value = it.value + (node.pair.first as ValueNode).value
        }
        right(node)?.let {
            println("  Right: $it")
            it.value = it.value + (node.pair.second as ValueNode).value
        }

        val parent = node.parent as PairNode
        val newNode = ValueNode(0)
        newNode.parent = parent
        if (node == parent.pair.first) {
            parent.pair = newNode to parent.pair.second
        }
        if (node == parent.pair.second) {
            parent.pair = parent.pair.first to newNode
        }
    }

    private fun split(node: ValueNode) {
        println("Splitting $node, parent: ${node.parent}")
        val newNode = PairNode(ValueNode(node.value / 2) to ValueNode(node.value - (node.value / 2)))
        newNode.pair.first.parent = newNode
        newNode.pair.second.parent = newNode
        val parent = node.parent as PairNode
        newNode.parent = parent
        if (node == parent.pair.first) {
            parent.pair = newNode to parent.pair.second
        }
        if (node == parent.pair.second) {
            parent.pair = parent.pair.first to newNode
        }
    }

    private fun left(fish: Node): ValueNode? {
        var currentNode: Node? = fish

        // Traverse up
        while (currentNode!!.parent != null && currentNode.parent is PairNode && currentNode == (currentNode.parent as PairNode).pair.first) {
            currentNode = currentNode.parent as PairNode
        }
        currentNode = currentNode.parent

        // Exception: if this is the leftmost node return null
        if (currentNode == null) return null

        // Go left
        currentNode = (currentNode as PairNode).pair.first

        // Go right until we hit a value
        while (currentNode is PairNode) {
            currentNode = currentNode.pair.second
        }

        return currentNode as ValueNode
    }

    private fun right(fish: Node): ValueNode? {
        var currentNode: Node? = fish

        // Traverse up
        while (currentNode!!.parent != null && currentNode.parent is PairNode && currentNode == (currentNode.parent as PairNode).pair.second) {
            currentNode = currentNode.parent as PairNode
        }
        currentNode = currentNode.parent

        // Exception: if this is the rightmost node return null
        if (currentNode == null) return null

        // Go right
        currentNode = (currentNode as PairNode).pair.second

        // Go left until we hit a value
        while (currentNode is PairNode) {
            currentNode = currentNode.pair.first
        }

        return currentNode as ValueNode
    }


    fun magnitude(fish: Node): Int {
        return if (fish is PairNode) {
            3 * magnitude(fish.pair.first) + 2 * magnitude(fish.pair.second)
        } else {
            (fish as ValueNode).value
        }
    }

    fun parseToFish(line: String): Node {
        val (node, result) = parseToNode(line)
        assert(result.isEmpty())
        return node
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

            ValueNode(line.takeWhile { it.isDigit() }.toInt()) to line.dropWhile { it.isDigit() }
        }
    }
}

fun main() {
    println("Part 1: ${Day18().part1(readLines("day18"))}")
    println("Part 2: ${Day18().part2(readLines("day18"))}")
}