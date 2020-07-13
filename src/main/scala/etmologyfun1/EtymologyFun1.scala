package etmologyfun1

sealed trait Language
case object English extends Language
case object MiddleEnglish extends Language
case object OldEnglish extends Language
case object ProtoGermanic extends Language
case object ProtoIndoEuropean extends Language
case object Latin extends Language

case class Word(characters: String, language: Language)

sealed trait Etymology
case class EtymologyNode(word: Word, root: Etymology) extends Etymology
case object End extends Etymology

object EtymologyFun1 {
  def fold[V](e: Etymology, v: V, f: (Word, V) => V): V = e match {
    case End =>
      v
    case EtymologyNode(word, root) =>
      fold(root, f(word, v), f)
  }

  def describe(e: Etymology): String =
    fold[String](
      e,
      // Initial accumulator value is the empty string
      "",
      // The accumulator function, defined anonymously
      (w: Word, acc: String) => acc + "(%s, %s)\n".format(w.characters, w.language)
    )

  def length(e: Etymology): Int =
    fold(e, 0, (_, acc: Int) => acc + 1)

  def contains(e: Etymology, language: Language): Boolean =
    fold(e, false, (w: Word, acc: Boolean) => acc || w.language == language)

  def main(args: Array[String]): Unit = {
    val foldEtymology =
      EtymologyNode(Word("fold", English),
        EtymologyNode(Word("folden", MiddleEnglish),
          EtymologyNode(Word("fealdan", OldEnglish),
            EtymologyNode(Word("falþaną", ProtoGermanic),
              EtymologyNode(Word("pel", ProtoIndoEuropean),
                End)))))

    print(describe(foldEtymology))
    assert(length(foldEtymology) == 5)
    assert(contains(foldEtymology, English))
    assert(contains(foldEtymology, OldEnglish))
    assert(contains(foldEtymology, ProtoIndoEuropean))
    assert(!contains(foldEtymology, Latin))
  }
}