package nl.dennisschroer.adventofcode.year2021

import java.lang.Exception
import java.util.Collections.max
import java.util.Collections.min

class Day16 {
    // AST classes
    abstract class Packet(open val version: Int, open val type: Int)
    data class LiteralPacket(override val version: Int, override val type: Int, val literal: Long) : Packet(version, type)
    data class OperatorPacket(override val version: Int, override val type: Int, val packets: List<Packet>) : Packet(version, type)

    // AST parser methods
    private fun hexToBinary(hex: String): String = hex.map { it.digitToInt(16).toString(2).padStart(4, '0') }.joinToString("")
    private fun parse(binary: String): Packet = readPacket(binary).first
    fun parseHex(hex: String) = parse(hexToBinary(hex))

    // AST-node parsing methods
    private fun readPacket(binary: String): Pair<Packet, String> {
        val (version, stream1) = readVersion(binary)
        val (type, stream2) = readType(stream1)

        return when (type) {
            4 -> {
                val (literal, stream3) = readLiteral(stream2)
                LiteralPacket(version, type, literal) to stream3
            }
            else -> {
                val (packets, stream3) = readPackets(stream2)
                OperatorPacket(version, type, packets) to stream3
            }
        }
    }

    private fun readLiteral(stream: String): Pair<Long, String> {
        var resultingStream = stream
        var literalBits = ""

        while (resultingStream[0] == '1') {
            literalBits += resultingStream.take(5).drop(1)
            resultingStream = resultingStream.drop(5)
        }

        literalBits += resultingStream.take(5).drop(1)
        resultingStream = resultingStream.drop(5)

        return literalBits.toLong(2) to resultingStream
    }

    private fun readPackets(stream: String): Pair<List<Packet>, String> {
        val packets = mutableListOf<Packet>()
        val (lengthType, stream2) = readLengthType(stream)

        if (lengthType == '0') {
            val (length, stream3) = readType0Length(stream2)
            var packetBits = stream3.take(length)
            while (packetBits.isNotEmpty()) {
                readPacket(packetBits).let {
                    packets.add(it.first)
                    packetBits = it.second
                }
            }

            return packets to stream3.drop(length)
        } else {
            val (length, stream3) = readType1Length(stream2)
            var remainingBits = stream3

            repeat(length) {
                readPacket(remainingBits).let {
                    packets.add(it.first)
                    remainingBits = it.second
                }
            }
            return packets to remainingBits
        }
    }

    private fun readVersion(stream: String): Pair<Int, String> = stream.take(3).toInt(2) to stream.drop(3)
    private fun readType(stream: String): Pair<Int, String> = stream.take(3).toInt(2) to stream.drop(3)
    private fun readLengthType(stream: String): Pair<Char, String> = stream[0] to stream.drop(1)
    private fun readType0Length(stream: String): Pair<Int, String> = stream.take(15).toInt(2) to stream.drop(15)
    private fun readType1Length(stream: String): Pair<Int, String> = stream.take(11).toInt(2) to stream.drop(11)

    // Method to walk over the nodes in the AST
    private fun walk(
        packet: Packet,
        visitLiteral: (literalPacket: LiteralPacket) -> Long,
        visitOperator: (operatorPacket: OperatorPacket, children: List<Long>) -> Long
    ): Long {
        return if (packet is LiteralPacket) {
            visitLiteral(packet)
        } else {
            visitOperator(packet as OperatorPacket, packet.packets.map { walk(it, visitLiteral, visitOperator) })
        }
    }

    // Solutions to puzzle
    fun part1(hex: String): Long {
        val packet = parseHex(hex)
        return walk(packet, { it.version.toLong() }, { packet, children -> packet.version + children.sum() })
    }

    fun part2(hex: String): Long {
        val packet = parseHex(hex)
        return walk(packet, { it.literal }, { childPacket, children ->
            when (childPacket.type) {
                0 -> children.sum()
                1 -> children.reduce(Long::times)
                2 -> min(children)
                3 -> max(children)
                5 -> if (children[0] > children[1]) 1 else 0
                6 -> if (children[0] < children[1]) 1 else 0
                7 -> if (children[0] == children[1]) 1 else 0
                else -> throw Exception()
            }
        })
    }
}

fun main() {
    println("Part 1: ${Day16().part1(readFile("day16"))}")
    println("Part 2: ${Day16().part2(readFile("day16"))}")
}