object Main extends App {

  def score(word: String): Int =
    word.replaceAll("a", "").length

  def negativeScore(word: String): Int = -1 * score(word)

  def rankedWords(
      wordScore: String => Int,
      words: List[String]
  ): List[String] = {
    words.sortBy(wordScore).map(w => w + wordScore(w))
  }

  def bonus(word: String): Int = if (word.contains("c")) 5 else 0

  def penalty(word: String): Int = if (word.contains("s")) 7 else 0

  def wordScores(wordScore: String => Int, words: List[String]): List[Int] = {
    words.map(wordScore)
  }

  def highScoringWords(
      wordScore: String => Int
  )(higherThan: Int)(words: List[String]): List[String] = {
    words.filter(word => wordScore(word) > higherThan)
  }

  def highScoringWords1(
      wordScore: String => Int
  )(words: List[String])(higherThan: Int): List[String] = {
    words.filter(word => wordScore(word) > higherThan)
  }

  val input = List("java", "scala", "rust", "ada")

  val wordsWithScoreHigherThan: Int => List[String] => List[String] =
    highScoringWords(w => score(w) + bonus(w) - penalty(w))

  val wordsWithScoreHigherThan0: List[String] => List[String] =
    wordsWithScoreHigherThan(0)

  val wordsWithScoreHigherThan1: List[String] => Int => List[String] =
    highScoringWords1(w => score(w) + bonus(w) - penalty(w))

  val wordsWithScoreHigherThan1nput: Int => List[String] =
    wordsWithScoreHigherThan1(input)

  println(wordsWithScoreHigherThan1nput(1))

  println(wordsWithScoreHigherThan0(input))

  println(wordsWithScoreHigherThan(1)(input))
}
