class WordCalculatorTest extends org.scalatest.FunSuite {
    test("WordCalculatorTest.removeLetter") {
        assert(WordCalculator.removeLetter("oscar", 2) === "ocar")
    }

    test("WordCalculatorTest.expand") {
        assert(WordCalculator.expand("cat") === Vector("at","ct","ca"))
    }

    test("WordCalculatorTest.combinations") {
        assert(WordCalculator.combinations("cat") === Vector("cat", "cta", "act", "atc", "tca", "tac"))
    }


    test("WordCalculatorTest.expandFilter1") {
        assert(WordCalculator.expandFilter("cat") === List("ac"))
    }

    test("WordCalculatorTest.expandFilter2") {
        assert(WordCalculator.expandFilter("radio") === List("oria", "road", "ador", "orad", "arid"))
    }
}
