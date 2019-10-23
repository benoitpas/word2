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

    test("WordFunnelText.nextWords") {
        assert(WordFunnel.nextWords("radio",0) == Set())
        assert(WordFunnel.nextWords("radio",1) == Set("adio","rdio","raio","rado","radi"))
        assert(WordFunnel.nextWords("radio",2) == Set("rad", "rdio", "dio", "rado", "aio", "rdi", "adi", "rao", "radi", "raio", "adio", "rio", "ado", 
        "rai", "rdo"))
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

    test("WordFunnelTest.funnelLength10") {
        assert(WordFunnel.funnelLength10() == "complecting")
    }

    test("WordFunnelTest.funnelLength12") {
        assert(WordFunnel.funnelLength12() == List("contradictorinesses", "preformationists"))
    }

    // Takes 14s
    test("WordFunnelTest.funnelLength12TailRec.2") {
        assert(WordFunnel.funnelLength12TailRec(2) == List("contradictorinesses", "preformationists"))
    }

    // Takes 3min
    test("WordFunnelTest.funnelLength12TailRec.3") {
        assert(WordFunnel.funnelLength12TailRec(3) == List("contradictorinesses", "noncooperationists", "preformationists"))
    }

/*
    // Takes 30min
    test("WordFunnelTest.funnelLength12TailRec.4") {
        assert(WordFunnel.funnelLength12TailRec(4) == List("contradictorinesses", "noncooperationists", "preformationists"))
    }
 */

    // 3 cases were tested:
    // [1] no memoization
    // [2] memoization of end results
    // [3] memoization of end results and generated words (from a word)

    // Takes 15s without memoization, 9s with memoization of last results, 6s with memoization of generated words and laest results
    test("WordFunnelTest.funnelLength12TailRec2.2") {
        assert(WordFunnel.funnelLength12TailRec2(2) == List("contradictorinesses", "preformationists"))
    }
/*
    // Takes 3min 50s without memoization, 1min 40s with 1, 1min 17s with 2
    test("WordFunnelTest.funnelLength12TailRec2.3") {
        assert(WordFunnel.funnelLength12TailRec2(3) == List("contradictorinesses", "noncooperationists", "preformationists"))
    }

    // Takes 25min with 1 memoization, 21min with 2
    test("WordFunnelTest.funnelLength12TailRec2.4") {
        assert(WordFunnel.funnelLength12TailRec2(4) == List("contradictorinesses", "noncooperationists", "preformationists"))
    }

    // Takes 2h 30min with 1 memoization, 2h29 with 2...
    test("WordFunnelTest.funnelLength12TailRec2.5") {
        assert(WordFunnel.funnelLength12TailRec2(5) == List("contradictorinesses", "establishmentarianisms", "noncooperationists", "nonrepresentationalisms", "preformationists", "unrepresentativenesses"))
    }
*/
}
