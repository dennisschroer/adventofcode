package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class Day24Test {

    @Test
    fun testNegateInput() {
        val program = """
            inp x
            mul x -1
        """.trimIndent().split("\n")

        assertEquals(-3, Day24().runProgram(Day24().parseCommands(program), listOf(3)).memory['x'])
    }

    @Test
    fun testEqualTo3Times() {
        val program = """
            inp z
            inp x
            mul z 3
            eql z x
        """.trimIndent().split("\n")

        assertEquals(1, Day24().runProgram(Day24().parseCommands(program), listOf(1, 3)).memory['z'])
        assertEquals(0, Day24().runProgram(Day24().parseCommands(program), listOf(1, 4)).memory['z'])
        assertEquals(1, Day24().runProgram(Day24().parseCommands(program), listOf(103, 309)).memory['z'])
        assertEquals(1, Day24().runProgram(Day24().parseCommands(program), listOf(-10, -30)).memory['z'])
    }

    @Test
    fun testBinaryConversion() {
        val program = """
            inp w
            add z w
            mod z 2
            div w 2
            add y w
            mod y 2
            div w 2
            add x w
            mod x 2
            div w 2
            mod w 2
        """.trimIndent().split("\n")

        // 1111
        val state = Day24().runProgram(Day24().parseCommands(program), listOf(15))

        assertEquals(1, state.memory['z'])
        assertEquals(1, state.memory['y'])
        assertEquals(1, state.memory['x'])
        assertEquals(1, state.memory['w'])

        // 0110
        val state2 = Day24().runProgram(Day24().parseCommands(program), listOf(6))

        assertEquals(0, state2.memory['z'])
        assertEquals(1, state2.memory['y'])
        assertEquals(1, state2.memory['x'])
        assertEquals(0, state2.memory['w'])
    }
}