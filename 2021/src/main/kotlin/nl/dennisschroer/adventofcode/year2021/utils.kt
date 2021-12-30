package nl.dennisschroer.adventofcode.year2021

fun readFile(filename: String): String {
    return Day1::class.java.getResource("/$filename.txt")!!.readText().trim()
}

fun readLines(filename: String): List<String> {
    return readFile(filename).split("\n")
}

fun readNumbers(filename: String): List<Int> {
    return readLines(filename).map { it.toInt() }
}

fun IntRange.overlaps(other: IntRange): Boolean = first <= other.last && last >= other.first
val IntRange.length: Int get() = if (last >= first) last - first + 1 else 0
