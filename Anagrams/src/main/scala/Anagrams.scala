import scala.collection.immutable._
import scala.io.Source
import java.util.regex.Pattern

/*
  Groupe : Mathieu Monteverde, Sathiya Kirushnapillai & Michela Zucca
 */
object Anagrams extends App {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** A fingerprint is a string which represents a sorted sequence of characters:
    * Examples:
    *
    * "aaccx"
    * "abyz"
    * "ppp"
    * ""
    */

  type FingerPrint = String


  /** The dictionary is simply a sequence of words.
    * You can begin your development with this simple example.
    * A dictionary of English words is given to you as an external file (linuxwords.txt)
    * that you can load to use with your program
    */
  val path = "linuxwords.txt"
  val dictionary: List[Word] = Source.fromResource(path).getLines.toList
  

  /** Converts a word/sentence into its fingerprint.
    * The fingerprint has the same characters as the word, with the same
    * number of occurrences, but the characters appear in sorted order.
    */

  def fingerPrint(s: Word): FingerPrint = {
    s.toLowerCase.sorted
  }
  def fingerPrint(s: Sentence): FingerPrint = {
    fingerPrint(s.mkString)
  }

   println("**** fingerPrint")
   println("Anagrams.fingerPrint(\"eat\"): "+ Anagrams.fingerPrint("eat"))
   println("Anagrams.fingerPrint(List(\"I\",\"Love\",\"You\"))"+ Anagrams.fingerPrint(List("I","Love","You"))+"\n")


  /** `matchingWords` is a `Map` from fingerprints to a sequence of all
    * the words that have that fingerprint.
    * This map serves as an easy way to obtain all the anagrams of a word given its fingerprint.
    *
    * For example, the word "eat" has the fingerprint "aet".
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `matchingWords` map will contain an entry:
    *
    * "aet"-> List("ate", "eat", "tea")
    */

  val matchingWords: Map[FingerPrint, List[Word]] = {
    dictionary groupBy fingerPrint
  }


  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    matchingWords.getOrElse(fingerPrint(word), Nil)
  }

  // Test code with for example:
   println("**** Test wordAnagrams")
   println("wordAnagrams(\"eta\"): " + wordAnagrams("eta"))
   println("wordAnagrams(\"jbdikb\"): " + wordAnagrams("jbdikb")+"\n")


  /** Returns the list of all subsequences of a fingerprnt.
    * This includes the fingerprint itself, i.e.
    * "ko" is a subsequence of "kkoo". It also always includes
    * the empty string "".
    *
    * Example: the subsequences of the fingerprint "abbc" are
    *
    * List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc")
    *
    * Note that the order of the subsequences does not matter -- the subsequences
    * in the example above could have been displayed in some other order.
    */

  def subseqs(fp: FingerPrint): List[FingerPrint] = {
    fp match {
      case s if s.isEmpty => List[String]("")
      case _ => {
        // Retrieve the first character and compute subsequences without this character
        val char = fp.charAt(0)
        val subsequences = subseqs(fp.substring(1))
        // Add the first character in front of every subsequence to create new ones
        val n : List[FingerPrint] = for (s <- subsequences) yield char + s
        // Return a list containing all resulting subsequences
        (subsequences ++ n).distinct
      }
    }
  }

  // Test code with for example:
  println("**** Testing subseqs")
  println("subseqs(\"abbc\"): " + subseqs("abbc")+"\n")


  /** Subtracts fingerprint `y` from fingerprint `x`.
    *
    * The precondition is that the fingerprint `y` is a subsequence of
    * the fingerprint `x` -- any character appearing in `y` must
    * appear in `x`.
    */

  def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = (x, y) match {
    case (x, y) if y.isEmpty => x
    case (x, y) if subseqs(x) contains y => {
      // Remove the first occurence of the first letter of y from x
      val filtered = x.replaceFirst(Pattern.quote(y.charAt(0).toString), "")
      // Continue subtracting y from x without the first letter
      subtract(filtered, y.substring(1))
    }
    case _ => x
  }


  // Test code with for example:
  println("**** Testing subtract: ")
  println("subtract(\"aabbcc\", \"abc\"): "+ subtract("aabbcc", "abc")+"\n")


  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the fingerprint of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive","you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    /**
      * Recursively builds a List of Sentence that contians anagrams of the given FingerPrint
      * @param sf the FingerPrint to use to build anagrams
      * @return a List of Sentences made of anagrams possible from the FingerPrint
      */
    def recursiveAnagrams (sf: FingerPrint): List[Sentence] = sf match {
      case sf if sf.isEmpty => List(List())
      case _ => {
        for (
          s <- subseqs(sf); // For each subsequence
          a <- wordAnagrams(s); // Get all anagrams for that subsequence
          sa <- recursiveAnagrams(subtract(sf, s)) // Retrieve all possible remaining sentences
        ) yield (a :: sa) // Yield a new sentence for each anagram and each remaing sentences
      }
    }

    val sentenceFingerprint = fingerPrint(sentence)
    recursiveAnagrams(sentenceFingerprint).distinct
  }

  // Test code with for example:
  println("**** Testing anagrams")
  println("sentenceAnagrams(List(\"eat\", \"tea\")): "+sentenceAnagrams(List("eat", "tea")))
  println("sentenceAnagrams(List(\"you\", \"olive\")): " + sentenceAnagrams(List("you", "olive")))
  println("sentenceAnagrams(List(\"I\", \"love\", \"you\")): " + sentenceAnagrams(List("I", "love", "you"))+"\n")
  println("sentenceAnagrams(List()): " + sentenceAnagrams(List()))
  println("sentenceAnagrams(List(\"\")): " + sentenceAnagrams(List("")))

}

