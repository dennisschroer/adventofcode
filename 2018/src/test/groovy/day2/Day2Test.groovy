package day2

import spock.lang.Specification

class Day2Test extends Specification {

    def "it calculates the correct checksum"() {
        given:
        def data = [
                "abcdef",
                "bababc",
                "abbcde",
                "abcccd",
                "aabcdd",
                "abcdee",
                "ababab"
        ]

        expect:
        Day2.calculateChecksum(data) == 12
    }

    def "it calculates the common letters"(){
        given:
        def data = [
                "abcde",
                "fghij",
                "klmno",
                "pqrst",
                "fguij",
                "axcye",
                "wvxyz",
        ]

        expect:
        Day2.findCommonOfCorrectBoxes(data) == "fgij"
    }
}