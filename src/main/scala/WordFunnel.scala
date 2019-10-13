import WordCalculator.{expand, words}

import scala.annotation.tailrec

object WordFunnel extends App {

    import scala.io.Source

    println(funnel2tailrec("preformations"));
    println(nextStep(List(List("preformations"))))
    // if lazy is removed then words is not initialized for unit tests
    lazy val wordList = Source.fromResource("enable1.txt").getLines.toList
    lazy val words = wordList.toSet

    def removeLetter(w: String, i: Int): String = w.substring(0, i - 1) + w.substring(i, w.length)

    def expand(w: String): IndexedSeq[String] = (1 to w.length) map (l => removeLetter(w, l))

    def expandFilter(w: String) = {
        val nw = expand(w).filter(words.contains(_))
        nw.toList
    }


    def recCombinations(w: String): List[List[String]] = {

        val nw = expandFilter(w)
        nw match {
            case _ :: _ => nw.flatMap(w2 => (recCombinations(w2).map(lw => w :: lw)))
            case _ => List(List(w))
        }

    }

    def expandFilter2Steps(w: String):List[String] = {
        val nw = expand(w)
        val nw2 = (nw flatMap expandFilter).toSet
        (nw.filter(words.contains(_)) ++ nw2).toList
    }


    def recCombinations2Steps(w: String): List[List[String]] = {

        val nw = expandFilter2Steps(w)
        nw match {
            case _ :: _ => nw.flatMap(w2 => (recCombinations2Steps(w2).map(lw => w :: lw)))
            case _ => List(List(w))
        }

    }

    def funnel2(w: String): Int = {
        val r = recCombinations(w).map(l => (l.length, l)).sortBy(-_._1)
        r match {
            case h :: _ => h._1
            case _ => 0 // shouldn't happen...
        }
    }

    def funnelLength10(): String = wordList.toStream.filter(funnel2(_) == 10).take(1).toList.head

    def funnel2steps(w: String): Int = {
        val r = recCombinations2Steps(w).map(l => (l.length, l)).sortBy(-_._1)
        r match {
            case h :: _ => h._1
            case _ => 0 // shouldn't happen...
        }
    }

    def funnelLength12(): List[String] = wordList.filter(funnel2steps(_) == 12)

    /*
    Expand a list of funnel to the next step, remove funnels that are finished (i.e. no more next step)
     */
    def nextStep(funnels: List[List[String]]): List[List[String]] =
        for (funnel <- funnels;
            nextWord <- {
                val oneLetterLess = expand(funnel.head)
                val twoLetterLess = oneLetterLess flatMap expand
                (oneLetterLess ++ twoLetterLess).toSet.filter(words.contains(_))
            } if funnel.length > 0)
            yield nextWord::funnel

    def funnel2tailrec(w:String) = {
        @tailrec
        def loop(funnels: List[List[String]]) :List[List[String]]= {
            val ns = nextStep(funnels)
            ns match {
                case _::_ => loop(ns)
                case _ => funnels
            }
        }
        val longestFunnel = loop(List(List(w)))
        longestFunnel.head.length
    }

    def funnelLength12TailRec(): List[String] = wordList.filter(funnel2tailrec(_) == 12)

}