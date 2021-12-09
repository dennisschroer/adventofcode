package nl.dennisschroer.adventofcode.year2021

class Day8 {
    fun part1(input: List<String>): Int {
        return input.sumOf { it.substringAfter("| ").split(" ").count { it.length in listOf(2, 3, 4, 7) } }
    }

    fun part2(input: List<String>): Int {
        val lines: List<Pair<List<String>, List<String>>> = input.map { it.substringBefore(" |").split(" ") to it.substringAfter("| ").split(" ") }

        return lines.sumOf { line ->
            line.second.map { decode(it, resolveSegments(line.first)) }.map { segmentsToNumber(it) }.joinToString("").toInt()
        }
    }

    /**
     * Resolve the unique patterns to a map of target segment to mapped segment
     */
    fun resolveSegments(patterns: List<String>): Map<Char, Char> {
        val segments = mutableMapOf<Char, Char>()

        // Target segments:
        //  aaaa
        // b    c
        // b    c
        //  dddd
        // e    f
        // e    f
        //  gggg
        //
        // Length of patterns:
        // 0 = 6
        // 1 = 2 *
        // 2 = 5
        // 3 = 5
        // 4 = 4 *
        // 5 = 5
        // 6 = 6
        // 7 = 3 *
        // 8 = 7 *
        // 9 = 6
        //
        // Amount each segment occurs
        // a = 8
        // b = 6 *
        // c = 8
        // d = 7
        // e = 4 *
        // f = 9 *
        // g = 7

        // Lets count each segment
        val segmentCounts = patterns.joinToString("").groupingBy { it }.eachCount()

        // The top segment is digit 7 minus digit 1
        segments['a'] = (patterns.first { it.length == 3 }.toSet() - patterns.first { it.length == 2 }.toSet()).first()

        // Set segments based on unique counts
        segments['f'] = segmentCounts.filter { it.value == 9 }.keys.first()
        segments['e'] = segmentCounts.filter { it.value == 4 }.keys.first()
        segments['b'] = segmentCounts.filter { it.value == 6 }.keys.first()

        // c is digit 1 minus segment f
        segments['c'] = (patterns.first { it.length == 2 }.toSet() - segments['f']!!).first()

        // d is digit 4 minus b c and f
        segments['d'] = (patterns.first { it.length == 4 }.toSet() - setOf(segments['b']!!, segments['c']!!, segments['f']!!)).first()

        // g is the last one
        segments['g'] = ("abcdefg".toSet() - setOf(
            segments['a']!!,
            segments['b']!!,
            segments['c']!!,
            segments['d']!!,
            segments['e']!!,
            segments['f']!!
        )).first()

        return segments
    }

    /**
     * Decode the value using the segment mapping (of target to encoded segment)
     */
    private fun decode(value: String, segments: Map<Char, Char>): String {
        var result = value
        result = result.replace(segments['a']!!, '1')
        result = result.replace(segments['b']!!, '2')
        result = result.replace(segments['c']!!, '3')
        result = result.replace(segments['d']!!, '4')
        result = result.replace(segments['e']!!, '5')
        result = result.replace(segments['f']!!, '6')
        result = result.replace(segments['g']!!, '7')
        return result.toList().sorted().joinToString("")
    }

    private fun segmentsToNumber(value: String): Char {
        return when (value) {
            "123567" -> '0'
            "36" -> '1'
            "13457" -> '2'
            "13467" -> '3'
            "2346" -> '4'
            "12467" -> '5'
            "124567" -> '6'
            "136" -> '7'
            "1234567" -> '8'
            "123467" -> '9'
            else -> 'E'
        }
    }
}

fun main() {
    println("Part 1: ${Day8().part1(readLines("day8"))}")
    println("Part 2: ${Day8().part2(readLines("day8"))}")
}