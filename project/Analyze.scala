package uk.gov.hmrc
import java.io.File

import sbt.Keys._
import sbt._
import sbt.nio.Keys._
import sbt.plugins.JvmPlugin

import scala.collection.immutable
import scala.meta._
import scala.meta.contrib.AssociatedComments
import scala.meta.contrib.DocToken
import scala.meta.contrib.ScaladocParser
import scala.meta.internal.parsers.ScalametaParser
import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.reflect.NameTransformer

import scala.io.AnsiColor._

object Analyze extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val findUnusedComponents =
      taskKey[String]("Find unused components")

    val sourceDialect =
      taskKey[Dialect]("The source code dialect")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    findUnusedComponents / fileInputs := (Compile / unmanagedSources / fileInputs).value,
    findUnusedComponents := {
      val logger         = (findUnusedComponents / streams).value.log
      val traverse       = new FindUnusedComponents(logger)
      val compileDialect = (Compile / sourceDialect).value

      findUnusedComponents.inputFiles.view
        .foreach { file =>
          println(s"$CYAN${file.toString()}$RESET")
          val source = new ScalametaParser(Input.File(file))(compileDialect).parseSource()
          source.stats.foreach(traverse.apply(_))
        }

      traverse.report
    },
    commands += findUnusedComponentsCommand
  ) ++
    (for (configuration <- Seq(Test, Compile)) yield configuration / sourceDialect := {
      VersionNumber((configuration / scalaVersion).value).numbers match {
        case Seq(3L, _*)      =>
          dialects.Scala3
        case Seq(2L, 13L, _*) =>
          if ((configuration / scalacOptions).value.contains("-Xsource:3")) {
            dialects.Scala213Source3
          } else {
            dialects.Scala213
          }
        case Seq(2L, 12L, _*) =>
          if ((configuration / scalacOptions).value.contains("-Xsource:3")) {
            dialects.Scala212Source3
          } else {
            dialects.Scala212
          }
        case Seq(2L, 11L, _*) =>
          dialects.Scala211
        case Seq(2L, 10L, _*) =>
          dialects.Scala210
      }
    })

  val DOT = s"$BLUE.$RESET"

  def path(term: Term): Option[String] =
    term match {
      case Term.Select(term, name: Term.Name) =>
        path(term).map(_ + "." + name).orElse(Some(name.value))

      case name: Term.Name =>
        Some(name.value)
    }

  def fqn(tree: Tree): Option[String] = {
    val parentFqn = tree.parent.flatMap(fqn)
    tree match {
      case pkg: Pkg =>
        val n = s"$BLUE${path(pkg.ref).getOrElse(pkg.name.value)}$RESET"
        parentFqn.map(_ + DOT + n).orElse(Some(n))

      case trt: Defn.Trait =>
        val n = s"$YELLOW${trt.name.value}$RESET"
        parentFqn.map(_ + DOT + n).orElse(Some(n))

      case clz: Defn.Class =>
        val n = s"$GREEN${clz.name.value}$RESET"
        parentFqn.map(_ + DOT + n).orElse(Some(n))

      case obj: Defn.Object =>
        val n = s"$MAGENTA${obj.name.value}$RESET"
        parentFqn.map(_ + DOT + n).orElse(Some(n))

      case other =>
        parentFqn
    }
  }

  def findUnusedComponentsCommand =
    Command.command("findUnusedComponents") { (state: State) =>
      val taskKey = Compile / findUnusedComponents

      Project.runTask(taskKey, state) match {
        case None                            =>
        case Some((newState, Inc(inc)))      =>
          println(Incomplete.show(inc.tpe))
        case Some((newState, Value(report))) =>
          println(report)
      }
      state
    }

  class FindUnusedComponents(logger: Logger) extends Traverser {
    def report: String = ""

    override def apply(tree: Tree): Unit =
      tree match {
        case clz: Defn.Class =>
          println(s"${fqn(clz).getOrElse("")}")
          super.apply(clz)

        case obj: Defn.Object =>
          println(s"${fqn(obj).getOrElse("")}")
          super.apply(obj)

        case trt: Defn.Trait =>
          println(s"${fqn(trt).getOrElse("")}")
          super.apply(trt)

        case imp: scala.meta.Import =>
          imp.importers
            .map(i => path(i.ref))
            .collect { case Some(x) => x }
            .foreach(println)

        case tmp: Template =>
          super.apply(tmp)

        case node =>
          super.apply(node)
      }
  }

}
