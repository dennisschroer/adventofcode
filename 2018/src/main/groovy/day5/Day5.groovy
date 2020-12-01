package day5

class Day5
{
	static String getInput()
	{
		getClass().getResource("/day5/polymer.txt").readLines()[0]
	}

	static int lengthAfterReaction(String input){
		def chars = input.toCharArray() as List<Character>

		int length = 0
		while (length != chars.size())
		{
			length = chars.size()

			(0..length - 2).each {
				if (chars[it] != null && (chars[it] - chars[it + 1]).abs() == 32)
				{
					// Obliterate
					chars[it] = null
					chars[it + 1] = null
				}
			}

			// Cleanup
			chars.removeIf {it == null}
		}

		chars.size()
	}

	static void main(String[] args)
	{
		// Input
		def input = getInput()

		println "1st half: ${lengthAfterReaction(input)}"

		('A'..'Z').each({
			def pair = "${it}${(it.charAt(0)+32) as char}"
			def inputWithoutPair = input.replaceAll("[$pair]", "")
			println "$pair: ${lengthAfterReaction(inputWithoutPair)}"
		})
	}
}


