#!/usr/bin/env -S scala-cli shebang

//> using scala 3.5.2
//> using jvm 11
//> using toolkit 0.6.0

import scala.io.AnsiColor.*

// Example usage
// ./utils/findAndReplaceInMessages.sc --lang=en --find='MRNs' --replace='Movement Reference Numbers (MRNs)' --key='.title'  --value-not='Movement Reference Number' --key-not='error' --key-not='help' --key-not='hint'
// ./utils/findAndReplaceInMessages.sc --lang=en --find='MRN' --replace='Movement Reference Number (MRN)' --key='.title'  --value-not='Movement Reference Number' --key-not='error' --key-not='help' --key-not='hint'

val save = optionalScriptFlag('s',"save")(args)
val language = requiredScriptParameter('l',"lang")(args)
val existingWord = requiredScriptParameter('f',"find")(args)
val replacementWord = requiredScriptParameter('r',"replace")(args)
val ifValueContains = multipleScriptParameters('v',"value")(args)
val ifKeyContains = multipleScriptParameters('k',"key")(args)
val ifValueNotContains = multipleScriptParameters('n',"value-not")(args)
val ifKeyNotContains = multipleScriptParameters('m',"key-not")(args)

val messagesFilePath = language.match {
    case "cy" => os.pwd/"conf"/"messages.cy"
    case _ => os.pwd/"conf"/"messages"
}

val debug = !save

val messagesFile = os.read(messagesFilePath)

val messages: Seq[(String,String)] = 
    messagesFile
        .linesIterator
        .map(_.trim())
        .filterNot(_.isEmpty())
        .filterNot(_.startsWith("#"))
        .map(_.split("="))
        .map(a => (a.head, a.tail.mkString("=")))
        .toSeq  

val builder = new StringBuilder()

final val CROSSED = "\u001b[9m"

var index = 0
var counter = 0
var previousPrefix = ""
var blockLineCount = 0

messages.foreach{ case (key, value) =>

    if(save) then
        val prefix = key.takeWhile(_ != '.')
        if( prefix != previousPrefix && index > 0) then {
            if( blockLineCount > 1 ) then builder.append("\n")
            previousPrefix = prefix
            blockLineCount = 1
        } else {
            blockLineCount = blockLineCount + 1
        }

    if(
        ifKeyContains.forall(key.contains)
        && ifValueContains.forall(value.contains)
        && !ifKeyNotContains.exists(key.contains)
        && !ifValueNotContains.exists(value.contains)
    ) then
        val newValue = value.replaceFirst(existingWord, if(save) then replacementWord else s"$RED$CROSSED$existingWord$RESET$GREEN$replacementWord$YELLOW")
        val replaced = newValue!=value
        if(replaced) then counter = counter + 1
        if(save || replaced) then 
            if(debug) builder.append(CYAN)
            builder.append(key)
            if(debug) builder.append(BLUE)
            builder.append("=")
            if(debug) builder.append(YELLOW)
            if(replaced) then builder.append(newValue) else builder.append(value)
            if(debug) builder.append(RESET)
            builder.append("\n")
    else 
        if(save) then 
            builder.append(key)
            builder.append("=")
            builder.append(value)
            builder.append("\n")

    index = index + 1
}


if(debug) 
    println(s"Found $YELLOW$counter$RESET messages to be replaced:")
    println(builder.toString)
else {
    os.write.over(messagesFilePath, builder.toString)
    println(s"Found and replaced $YELLOW$counter$RESET messages in $CYAN$messagesFilePath$RESET")
}

// ---------------------
//  COMMAND -LINE UTILS 
// ---------------------

def requiredScriptParameter(shortName: Char, longName: String)(
      args: Array[String]
  ): String =
    optionalScriptParameter(shortName, longName)(args)
      .getOrElse {
        System.err.println(
          s"${RED}Required ${BOLD}-$shortName${RESET}${RED} or ${BOLD}--$longName${RESET}${RED} parameter is missing.${RESET}"
        )
        System.exit(1)
        ""
      }

def optionalScriptParameter(shortName: Char, longName: String)(
      args: Array[String]
  ): Option[String] =
    args.zipWithIndex
      .find((a, i) =>
        a == s"-$shortName"
          || a == s"--$longName"
          || a.startsWith(s"--${longName}=")
      )
      .flatMap((a, i) =>
        if (a.startsWith(s"--${longName}="))
        then Some(a.drop(s"--${longName}=".size))
        else if (args.length > i + 1) then
          val value = args(i + 1)
          if (value.startsWith("-")) None
          else Some(value)
        else None
      )
      .orElse {
        val sysValue = System.getProperty(longName)
        if (sysValue != null) then Some(sysValue)
        else
          val envValue = System.getenv(longName.toUpperCase())
          if (envValue != null) then Some(envValue)
          else None
      }

def multipleScriptParameters(shortName: Char, longName: String)(
      args: Array[String]
  ): Seq[String] = {
    val params =
        args.zipWithIndex
      .filter((a, i) =>
        a == s"-$shortName"
          || a == s"--$longName"
          || a.startsWith(s"--${longName}=")
      )
      .flatMap((a, i) =>
        if (a.startsWith(s"--${longName}="))
        then Some(a.drop(s"--${longName}=".size))
        else if (args.length > i + 1) then
          val value = args(i + 1)
          if (value.startsWith("-")) None
          else Some(value)
        else None
      )
    
    params ++ {
        val sysValue = System.getProperty(longName)
        if (sysValue != null) then Seq(sysValue) else Seq.empty
    } ++ {
        val envValue = System.getenv(longName.toUpperCase())
        if (envValue != null) then Seq(envValue) else Seq.empty
    }
}

def optionalScriptFlag(shortName: Char, longName: String)(
      args: Array[String]
  ): Boolean =
    args.zipWithIndex
      .exists((a, i) => a == s"-$shortName" || a == s"--$longName")

        

