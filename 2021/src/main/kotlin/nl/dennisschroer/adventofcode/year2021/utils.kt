package nl.dennisschroer.adventofcode.year2021

fun readLines(filename: String): List<String> {
    return Day1::class.java.getResource("/$filename")!!.readText().trim().split("\n")
}