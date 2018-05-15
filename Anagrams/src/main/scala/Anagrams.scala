import scala.collection.immutable._
import scala.io.Source
import java.util.regex.Pattern


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
  // println(wordAnagrams("eta"))
  // println(wordAnagrams("jbdikb"))


  /** Returns the list of all subsequences of a fingerprint.
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
        val char = fp.charAt(0)
        val list = subseqs(fp.substring(1))
        val n : List[FingerPrint] = for (s <- list) yield char + s
        (list ++ n).distinct
      }
    }
  }

  // Test code with for example:
  println("Testing subseqs")
  println("Result: " + subseqs("abbc"))


  /** Subtracts fingerprint `y` from fingerprint `x`.
    *
    * The precondition is that the fingerprint `y` is a subsequence of
    * the fingerprint `x` -- any character appearing in `y` must
    * appear in `x`.
    */

  def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = (x, y) match {
    case (x, y) if y.isEmpty => x
    case (x, y) if subseqs(x) contains y => {
      val filtered = x.replaceFirst(Pattern.quote(y.charAt(0).toString), "")
      subtract(filtered, y.substring(1))
    }
    case _ => x
  }


  // Test code with for example:
  println("Testing subtract: ")
  println(subtract("aabbcc", "abc"))


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
    def recursiveAnagrams (sf: FingerPrint): List[Sentence] = sf match {
      case sf if sf.isEmpty => List(List())
      case _ => {
        for (
          s <- subseqs(sf); // For each subsequence
          a <- wordAnagrams(s); // Get all anagrams for that subsequence
          sa <- recursiveAnagrams(subtract(sf, s)) // Retrieve all possible remaining sentences
        ) yield (a :: sa) // Yield a new sentence  for each remaing sentences
      }
    }

    val sentenceFingerprint = fingerPrint(sentence)
    recursiveAnagrams(sentenceFingerprint).distinct
  }

  // Test code with for example:
  println("Testing anagrams")
  println(sentenceAnagrams(List("eat", "tea")))
  println(sentenceAnagrams(List("you", "olive")))
  println(sentenceAnagrams(List("I", "love", "you")))


  // ------------ TEST DU CODE
  // Test de fingerPrint(word)
  println(Anagrams.fingerPrint("eat"))
  // Test de wordAnagrams
  println(Anagrams.wordAnagrams("eat"))
  // Test de fingerprint(sentence)
  println(Anagrams.fingerPrint(List("eat","ate","tea")))
  // Test map du dico
  // for ((fingerPrint, words) <- matchingWords; word <- words) println(fingerPrint, word)
}

