import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    // Todo voir si prob sur linux aussi avec getClass
    val path = "D:\\HEIG\\COURS\\Annee_3\\2_sem\\SCALA\\Labo\\labo3\\SCALA_Labo03\\Anagrams\\src\\main\\scala\\linuxwords.txt" // getClass.getResource("linuxwords.txt")
    val source = scala.io.Source.fromFile(path).getLines.toList
    Anagrams.dictionary = source

    //println(Anagrams.fingerPrint("Bonjour"))
    //println(Anagrams.fingerPrint(List("Bonjour","a","tous")))
    println("------------ Test afficher les combinaison du dico (fingerPrint, List[words]---------------")
    val dicoFingerPrint = Anagrams.anagramDico()
    for((fingerPrint,words) <- dicoFingerPrint; word <- words) println(fingerPrint,word)

  }
}