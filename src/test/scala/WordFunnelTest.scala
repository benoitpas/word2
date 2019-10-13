class WordFunnelTest extends org.scalatest.FunSuite {
    test("WordFunnelTest.removeLetter") {
        assert(WordFunnel.removeLetter("oscar", 2) === "ocar")
    }

    test("WordCalculatorTest.expand") {
        assert(WordFunnel.expand("cat") === Vector("at","ct","ca"))
        assert(WordFunnel.expand("at") === Vector("t","a"))
    }


    test("WordFunnelTest.expandFilter.test0") {
        assert(WordFunnel.expandFilter("at") === List())
    }

    test("WordFunnelTest.expandFilter.test") {
        assert(WordFunnel.expandFilter("cat") === List("at"))
        assert(WordFunnel.expandFilter("radio") === List())
    }

    test("WordFunnelTest.expandFilter2Steps.test") {
        assert(WordFunnel.expandFilter2Steps("cat") === List("at"))
        assert(WordFunnel.expandFilter2Steps("radio") === List("ado", "rad"))
    }

    test("WordFunnelTest.recCombinations") {
        assert(WordFunnel.recCombinations("a") === List(List("a")))
        assert(WordFunnel.recCombinations("at") === List(List("at")))
        assert(WordFunnel.recCombinations("cat") === List(List("cat","at")))
    }

    test("WordFunnelTest.recCombinations2Steps") {
        assert(WordFunnel.recCombinations2Steps("radio") === List(List("radio", "ado", "do"),
            List("radio", "ado", "ad"),
            List("radio", "rad", "ad")))
    }


    test("WordFunnelTest.funnel2") {
        assert(WordFunnel.funnel2("gnash") == 4)
        assert(WordFunnel.funnel2("princesses") == 9)
        assert(WordFunnel.funnel2("turntables") == 5)
        assert(WordFunnel.funnel2("implosive") == 1)
        assert(WordFunnel.funnel2("programmer") == 2)
    }

    test("WordFunnelTest.funnel2steps") {
        assert(WordFunnel.funnel2steps("programmer") == 3)
        assert(WordFunnel.funnel2steps("preformationists") == 12)
    }


    /* times out travis ci
    test("WordFunnelTest.funnelLength10") {
        assert(WordFunnel.funnelLength10() == "complecting")
    }

    test("WordFunnelTest.funnelLength12") {
        // Should return 6 words...
        assert(WordFunnel.funnelLength12() == List("contradictorinesses", "preformationists"))
    }

    test("WordFunnelTest.funnelLength12TailRec") {
        // Should return 6 words...
        assert(WordFunnel.funnelLength12TailRec() == List("contradictorinesses", "preformationists"))
    }
    */


}
