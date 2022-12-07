package nl.dennisschroer.adventofcode.year2022

// A class is required to use the class loader
sealed class Loader{}

fun readFile(filename: String): String {
    return Loader::class.java.getResource("/$filename.txt")!!.readText().trim()
}

fun readLines(filename: String): List<String> {
    return readFile(filename).split("\n")
}

fun readBlocks(filename:String):List<List<String>> {
    return readFile(filename).split("\n\n").map {it.split("\n")}
}

fun readNumbers(filename: String): List<Int> {
    return readLines(filename).map { it.toInt() }
}

fun IntRange.fullyContains(other: IntRange): Boolean = first <=other.first && last >= other.last
fun IntRange.overlaps(other: IntRange): Boolean = first <= other.last && last >= other.first
val IntRange.length: Int get() = if (last >= first) last - first + 1 else 0
