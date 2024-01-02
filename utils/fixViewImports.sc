#!/usr/bin/env -S scala-cli shebang

//> using scala 3.3.1
//> using jvm 11
//> using toolkit latest

import scala.io.AnsiColor

val baseAppPath = os.pwd/"app"
val baseViewsPath = baseAppPath/"uk"/"gov"/"hmrc"/"cdsreimbursementclaimfrontend"/"views"

val subpackages = Set("rejectedgoods","claims","common","overpayments","securities")

val generalReferencePattern = s"(.+)\\sviews\\.html\\.(.+?)(,?)".r

val pagesReferencePattern = s"(.+)\\spages\\.(.+?)(,?)".r

val generalViewsImportStatement = "import uk.gov.hmrc.cdsreimbursementclaimfrontend.views"

var modifiedFilesCount = 0

os
    .walk(baseAppPath, skip = path => os.isFile(path) && !path.last.endsWith(".scala"))
    .filter(os.isFile)
    .foreach(processScalaFile)

println(s"Fixed $modifiedFilesCount files.")    

def processScalaFile(path: os.Path): Unit = {
    var code = os.read(path)
    var hasChanges = false
    subpackages.foreach{ subpackage =>

        val pagesImportStatement = s"import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{$subpackage => pages}"
    
        var importFound = false
        var pagesImports = scala.collection.mutable.Set[String]()
        var generalImports = scala.collection.mutable.Set[String]()

        val modifiedCode = code.linesIterator.toSeq.map{ l => 
            var line = l
            if(line.startsWith(generalViewsImportStatement)){
                importFound = true
                line = "GENERAL IMPORT PLACEHOLDER"
            } else if(line.startsWith(pagesImportStatement)){
                importFound = true
                line = "PAGES IMPORT PLACEHOLDER"
            } else if(importFound){
            line match {
                case generalReferencePattern(prefix, name, comma) => 
                    generalImports.addOne(name)
                    line = s"$prefix $name${Option(comma).getOrElse("")}"
                case pagesReferencePattern(prefix, name, comma) => 
                    pagesImports.addOne(name)
                    line = s"$prefix $name${Option(comma).getOrElse("")}"    
                case _ =>    
            }
            }
            line
        }.map{
            case "GENERAL IMPORT PLACEHOLDER" =>
                if(generalImports.size>1)
                    s"import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{${generalImports.toSeq.sorted.mkString(",")}}"
                else 
                    s"import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.${generalImports.toSeq.sorted.mkString}"
            
            case "PAGES IMPORT PLACEHOLDER" =>
                if(pagesImports.size>1)
                    s"import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.$subpackage.{${pagesImports.toSeq.sorted.mkString(",")}}"
                else 
                    s"import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.$subpackage.${pagesImports.toSeq.sorted.mkString}"
            case other => other    
        }.mkString("\n")

        if(importFound && (pagesImports.nonEmpty || generalImports.nonEmpty)){
            code = modifiedCode
            if(generalImports.nonEmpty){
                println(s"${AnsiColor.GREEN}Replacing general imports ${AnsiColor.WHITE}${generalImports.mkString(", ")}${AnsiColor.GREEN} at ${AnsiColor.MAGENTA}${path}${AnsiColor.RESET}")
            }
            if(pagesImports.nonEmpty){
                println(s"${AnsiColor.GREEN}Replacing ${AnsiColor.CYAN}$subpackage${AnsiColor.GREEN} with imports ${AnsiColor.WHITE}${pagesImports.mkString(", ")}${AnsiColor.GREEN} at ${AnsiColor.MAGENTA}${path}${AnsiColor.RESET}")
            }
            hasChanges = true
        }
    }

    if(hasChanges){
        os.write.over(path, code)
        modifiedFilesCount = modifiedFilesCount + 1
    }
}