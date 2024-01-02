#!/usr/bin/env -S scala-cli shebang

//> using scala 3.3.1
//> using jvm 11
//> using toolkit latest

import scala.io.AnsiColor

val baseViewsPath = os.pwd/"app"/"uk"/"gov"/"hmrc"/"cdsreimbursementclaimfrontend"/"views"

val keyPattern = java.util.regex.Pattern.compile("\\@key\\s=\\s\\@\\{\"(.+)\"\\}")  

var keyCount = 0

os
    .walk(baseViewsPath, skip = path => os.isFile(path) && !path.last.endsWith(".scala.html"))
    .filter(os.isFile)
    .foreach(processView)

println(s"Found $keyCount keys in total")    

def processView(path: os.Path): Unit = {
    println(s"Processing ${AnsiColor.YELLOW}${path.relativeTo(baseViewsPath)}${AnsiColor.RESET} ...")

    val view = os.read(path)

    var i = 0
    var hasKey = false
    var key = ""

    val modifiedView = view.linesIterator.toSeq.map{ l => 
        var line = l
        if(!hasKey){
            val foundKey = line.contains("@key = @{\"")
            if(foundKey){
                val matcher = keyPattern.matcher(line)
                if(matcher.find()){
                    key = matcher.group(1)
                    keyCount = keyCount + 1
                    println(s"${AnsiColor.GREEN}Found key ${AnsiColor.MAGENTA}${key}${AnsiColor.GREEN} in line $i${AnsiColor.RESET}")
                    hasKey = true
                }
            }
        } else {
            if(line.contains("key")){
                line  = line
                    .replace("$key\"",s"$key\"")
                    .replace("$key.",s"$key.")
                    .replace("$key-",s"$key-")
                    .replace("$key[",s"$key[")
                    .replace("${key}",key)
            }
        }  
        i = i + 1  
        line
    }.mkString("\n")

    if(hasKey){
        os.write.over(path, modifiedView)
    }
}