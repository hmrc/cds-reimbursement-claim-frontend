#!/usr/bin/env -S scala-cli shebang

//> using scala 3.3.3
//> using jvm 11
//> using toolkit 0.4.0

import scala.io.AnsiColor

val debug = !args.headOption.contains("--save")

sortMessages(os.pwd/"conf"/"messages")
sortMessages(os.pwd/"conf"/"messages.cy")

def sortMessages(messagesFilePath: os.Path) = {

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
            .sortBy(_._1)    

    println(messages.size) 

    val builder = new StringBuilder()

    var previousPrefix = ""
    var blockBuilder = new StringBuilder()
    var blockLineCount = 0

    messages.foreach{ case (key, value) =>
        val prefix = key.takeWhile(_ != '.')
        if(prefix!=previousPrefix){
            if(blockLineCount>1){
                builder.append("\n")
            }
            builder.append(blockBuilder.toString)
            blockBuilder = new StringBuilder()
            blockLineCount = 0
        }
        blockBuilder.append(key)
        blockBuilder.append("=")
        blockBuilder.append(value)
        if(debug) blockBuilder.append(AnsiColor.RESET)
        blockBuilder.append("\n")
        blockLineCount = blockLineCount + 1
        previousPrefix = prefix
    }
    if(blockLineCount>1){
        builder.append("\n")
    }
    builder.append(blockBuilder.toString)

    if(debug) println(builder.toString)
    else {
        os.write.over(messagesFilePath, builder.toString)
        println(s"Saved sorted messages back to $messagesFilePath")
    }
}

        

