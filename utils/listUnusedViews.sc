#!/usr/bin/env -S scala-cli shebang

//> using scala 3.3.1
//> using jvm 11
//> using toolkit latest

import scala.io.AnsiColor

val baseAppPath = os.pwd/"app"
val baseViewsPath = baseAppPath/"uk"/"gov"/"hmrc"/"cdsreimbursementclaimfrontend"/"views"

val viewsPackage = "uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html."
val importPrefix = s"import $viewsPackage"

val importsFound = scala.collection.mutable.Set[String]()

val viewList = os
    .walk(baseViewsPath, skip = path => os.isFile(path) && !path.last.endsWith(".scala.html"))
    .filter(os.isFile)
    
val viewFullNames = viewList
    .map(path => path.relativeTo(baseViewsPath).segments.mkString(".").dropRight(".scala.html".length))
    .toSet

println(s"\n${AnsiColor.YELLOW}All views:${AnsiColor.RESET}")
println(viewFullNames.toSeq.sorted.mkString("\n"))  

viewList.foreach(processView)

os
    .walk(baseAppPath, skip = path => os.isFile(path) && !path.last.endsWith(".scala"))
    .filter(os.isFile)
    .foreach(processScalaFile)

println(s"\n${AnsiColor.YELLOW}Imports found:${AnsiColor.RESET}")
println(importsFound.toSeq.sorted.mkString("\n"))  

val notReferenced = viewFullNames--importsFound

println(s"\n${AnsiColor.YELLOW}Views not referenced:${AnsiColor.RESET}")
println(notReferenced.toSeq.sorted.mkString("\n"))  

def processView(path: os.Path): Unit = {
    //println(s"Processing view ${AnsiColor.YELLOW}${path.relativeTo(baseViewsPath)}${AnsiColor.RESET} ...")

    val code = os.read(path)

    code.linesIterator.toSeq.map{ line => 
        val i = line.indexOf(viewsPackage)
        if(i>=0){
            val viewName = line.drop(i).drop(viewsPackage.length).filter(c => c!=',' && c!=')')
            importsFound.addOne(viewName)
        }
    }
}

def processScalaFile(path: os.Path): Unit = {
    //println(s"Processing scala file ${AnsiColor.YELLOW}${path.relativeTo(baseAppPath)}${AnsiColor.RESET} ...")

    val code = os.read(path)

    code.linesIterator.toSeq.map{ line => 
        if(line.startsWith(importPrefix)){
            importsFound.addOne(line.drop(importPrefix.length))
        }
    }
}