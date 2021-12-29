import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.sys.process._

sealed trait GitOperation
case object Addition extends GitOperation
case object Removal extends GitOperation

final case class LineChange(date: Int, operation: GitOperation, translation: Translation)

final case class Translation(key: String, text: String)

object WelshTranslation {

  def fileEn     = Source.fromFile("conf/messages").getLines().toList
  def fileCy     = Source.fromFile("conf/messages.cy").getLines().toList
  def messagesEn = linesToMap(fileEn)
  def messagesCy = linesToMap(fileCy)

  val csvSeparatorCharacter =
    ';' //Using semicolon, because the text can contain commas, and then you have to use quotes ... just complicates things

  /** Generates 2 new files:
    *  - a new messages.cy file (from the english version + git history)
    *  - a csv file to send to the translation team with all the missing translations
    */
  def importAndExport(): Unit = {
    val newlinesToTranslate = ListBuffer[String]()
    val newWelshLines       = fileEn
      .map { line =>
        parseMessagesFileLine(line) match {
          case Some(translation: Translation) =>
            messagesCy.get(translation.key) match {
              case Some(welshText) =>
                translation.key + "=" + welshText
              case None            =>
                newlinesToTranslate += s"${translation.key}$csvSeparatorCharacter${translation.text}"
                translation.key + "="
            }
          case None                           =>
            line
        }
      }

    val newWelshLanguageFile = newWelshLines.mkString(System.lineSeparator()).getBytes(StandardCharsets.UTF_8)

    val changedTranslations = gitDiffLanguageFileChanges()
      .map(change => s"${change.translation.key}$csvSeparatorCharacter${change.translation.text}")

    val translationTeamFile = (newlinesToTranslate.toList ::: changedTranslations)
      .mkString(System.lineSeparator())
      .getBytes(StandardCharsets.UTF_8)

    Files.write(Paths.get("conf/messages.cy"), newWelshLanguageFile)
    Files.write(Paths.get("conf/CdsReimbursementNewOrChanged.csv"), translationTeamFile)

    ()
  }

  def gitDiffLanguageFileChanges(): List[LineChange] = {
    val gitLog   = "git --no-pager log -30".!!
    val commits  = """commit (\w+)""".r //Commit log lines: commit 043c494a3058d69e055be485bd30e485eb04cc9d
    val comitIds = commits.findAllMatchIn(gitLog).map(_.group(1)).toList

    val changedLineRegex = "^[-+](?![-+])(.+)".r //exclude lines like these: --- a/conf/messages OR +++ b/conf/messages

    comitIds.zipWithIndex
      .zip(comitIds.tail)
      .map { case ((newCommit, date), oldCommit) =>
        val gitDiff = s"git diff $oldCommit $newCommit -U0 conf/messages"
        println(gitDiff)
        gitDiff.!!.split(System.lineSeparator()).toList
          .map(changedLineRegex.findFirstIn(_)) //Keep only git changed lines
          .flatten(Option.option2Iterable)
          .map(parseGitMessagesFileLine(_, date))
          .flatten(Option.option2Iterable)
          .filter(_.operation == Addition)
      }
      .flatten
      .groupBy(_.translation.key)
      .toList
      .map(_._2.sortWith((a, b) => a.date < b.date).head)
  }

  def parseGitMessagesFileLine(line: String, date: Int): Option[LineChange] =
    parseMessagesFileLine(line.tail) match {
      case Some(translation: Translation) =>
        (line(0) == '+') match {
          case true  =>
            Some(LineChange(date, Addition, translation))
          case false =>
            Some(LineChange(date, Removal, translation))
        }
      case None                           =>
        None
    }

  def parseMessagesFileLine(line: String): Option[Translation] = {
    val separator        = line.indexOf('=')
    val sectionSeparator = line.indexOf("#==")
    (separator >= 0 && sectionSeparator < 0) match {
      case true  =>
        val languageKey = line.substring(0, separator)
        val text        = line.substring(separator + 1)
        Some(Translation(languageKey, text))
      case false =>
        None
    }
  }

  def linesToMap(lines: List[String]): Map[String, String] = {
    val kvs = lines
      .map { line =>
        val separator = line.indexOf('=')
        if (separator >= 0 && (line.length > separator + 1)) {
          Some(line.substring(0, separator) -> line.substring(separator + 1))
        } else
          None
      }
      .flatten(Option.option2Iterable)
    ListMap(kvs: _*)
  }

  //Prints language keys and translations that exists in english but are missing in welsh
  def translationsMissingInWelsh() {
    val missingLinesFromWelsh = messagesEn.toList
      .map(englishKV => if (messagesCy.isDefinedAt(englishKV._1)) None else Some(englishKV))
      .flatten(Option.option2Iterable)
      .sortWith((kv1, kv2) => kv1._1 < kv2._1)
      .map(kv => kv._1 + "=" + kv._2)

    if (missingLinesFromWelsh.length > 0) {
      println(System.lineSeparator() * 2)
      println("=" * 100)
      println(
        "=" * 15 + s"   Missing Language keys (and values) from messages.cy, counted: ${missingLinesFromWelsh.length}  " + "=" * 15
      )
      println("=" * 100)
      println(missingLinesFromWelsh.mkString(System.lineSeparator()))
    }
  }

  //Show orphaned welsh translations, for which the keys no longer exist in english
  def orphanedWelsTranslations() {
    val orphanedWelshLines = messagesCy.toList
      .map(englishKV => if (messagesEn.isDefinedAt(englishKV._1)) None else Some(englishKV))
      .flatten(Option.option2Iterable)
      .sortWith((kv1, kv2) => kv1._1 < kv2._1)
      .map(kv => kv._1 + "=" + kv._2)

    if (orphanedWelshLines.length > 0) {
      println(System.lineSeparator() * 2)
      println("=" * 110)
      println(
        "=" * 5 + s"   Orphaned welsh translations, for which the keys no longer exist in english: ${orphanedWelshLines.length} (Delete these)  " + "=" * 5
      )
      println("=" * 110)
      println(orphanedWelshLines.mkString(System.lineSeparator()))
    }
  }

  //"Show untranslated welsh lines"
  def englishLinesInWelsLanguageFiles() {
    val excludedKeys = List("footer.", "language-switcher.", "country.", "currency", "header.govuk.url", "lang")

    val untranslatedInWelsh = messagesCy.toList
      .filter { welshKV =>
        messagesEn.get(welshKV._1) match {
          case Some(english) =>
            if (english == welshKV._2 || english == "___" + welshKV._2) true else false
          case None          => false
        }
      }
      .filterNot(kv => excludedKeys.filter(a => kv._1.startsWith(a)).nonEmpty)
      .sortWith((kv1, kv2) => kv1._1 < kv2._1)
      .map(kv => kv._1 + "=" + kv._2)

    if (untranslatedInWelsh.length > 0) {
      println(System.lineSeparator() * 2)
      println("=" * 100)
      println(
        "=" * 15 + s"   Untranslated/English text in the Welsh messages.cy, counted: ${untranslatedInWelsh.length}  " + "=" * 15
      )
      println("=" * 100)
      println(untranslatedInWelsh.mkString(System.lineSeparator()))
    }
  }

}
