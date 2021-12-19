package nl.dennisschroer.adventofcode.year2021

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class Day16Test {

    @Test
    fun testPart1Literal() {
        val packet = Day16().parseHex("D2FE28")

        assertEquals(6, packet.version)
        assertEquals(4, packet.type)
        assertEquals(2021, (packet as Day16.LiteralPacket).literal)
    }

    @Test
    fun testPart1Operator1() {
        val packet = Day16().parseHex("38006F45291200")

        assertEquals(1, packet.version)
        assertEquals(6, packet.type)

        val packets = (packet as Day16.OperatorPacket).packets

        assertEquals(2, packets.size)
        assertEquals(4, packets[0].type)
        assertEquals(10, (packets[0] as Day16.LiteralPacket).literal)
        assertEquals(4, packets[1].type)
        assertEquals(20, (packets[1] as Day16.LiteralPacket).literal)
    }

    @Test
    fun testPart1Operator2() {
        val packet = Day16().parseHex("EE00D40C823060")

        assertEquals(7, packet.version)
        assertEquals(3, packet.type)

        val packets = (packet as Day16.OperatorPacket).packets

        assertEquals(3, packets.size)
        assertEquals(4, packets[0].type)
        assertEquals(1, (packets[0] as Day16.LiteralPacket).literal)
        assertEquals(4, packets[1].type)
        assertEquals(2, (packets[1] as Day16.LiteralPacket).literal)
        assertEquals(4, packets[2].type)
        assertEquals(3, (packets[2] as Day16.LiteralPacket).literal)
    }

    @Test
    fun testPart1() {
        assertEquals(16, Day16().part1("8A004A801A8002F478"))
        assertEquals(12, Day16().part1("620080001611562C8802118E34"))
        assertEquals(23, Day16().part1("C0015000016115A2E0802F182340"))
        assertEquals(31, Day16().part1("A0016C880162017C3686B18A3D4780"))
    }

    @Test
    fun testPart2() {
        assertEquals(0, Day16().part2(""))
    }
}