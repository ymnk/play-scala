package play.scalasupport.core

import play._
import play.test._
import play.vfs.{VirtualFile => VFile}
import play.classloading.ApplicationClasses.ApplicationClass

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import java.util.{List => JList}
import org.scalatest.{Suite, Assertions}
import org.scalatest.tools.ScalaTestRunner

/**
 * The ScalaPlugin in mainly responsible for compiling .scala files
 *
 * As scala compilation is pretty slow, we try to optimize things a lot:
 * The plugin will keep an in memory instance of the scala compiler. After each change to a scala file,
 * it will check for dependent files and recompile only needed sources. 
 *
 * When a compilation error occurs we will try to recompile all sources to avoid problems.
 *
 * We discard the in-memory compiler in some cases:
 * - After a change in the .scala files set (adding or removing a scala file to the application)
 * - When a modification to a scala file destroy some types (however if the type was anonymous we try to keep the compiler anyway)
 */
class ScalaPlugin extends PlayPlugin {
    
  var lastHash = 0

  lazy val compiler = new ScalaCompiler
  
  override def addTemplateExtensions(): JList[String] = List("play.scalasupport.templates.TemplateExtensions")

  /**
   * Scanning both java and scala sources for compilation
   */
  def scanSources = {
    val sources = ListBuffer[VFile]()
    val hash = new StringBuffer
    def scan(path: VFile): Unit = {
      path match {
        case _ if path.isDirectory => path.list foreach scan
        case _ if (path.getName().endsWith(".scala") || path.getName().endsWith(".java")) && !path.getName().startsWith(".") => sources add path; hash.append(path.relativePath)
        case _ =>
      }
    }
    Play.javaPath foreach scan
    (sources, hash.toString.hashCode)
  }

  /**
   * try to detect source changes
   */
  override def detectChange = {
    if (lastHash != scanSources._2) {
      reset()
      throw new PathChangeException
    }
  }
  

  def reset() {
    lastHash = 0
    compiler.clean()
  }
  
  def eval(code: String): Any = compiler.eval(code)

  /**
   * compile all classes
   * @param classes classes to be compiled
   * @param return return compiled classes
   **/
  override def compileAll(classes: JList[ApplicationClass]) = {

    // Precompiled
    if (Play.usePrecompiled) {
      new java.util.ArrayList[ApplicationClass]()
    }

    val (sources, hash) = scanSources
    if (lastHash == hash) {
      classes.addAll(compile(ListBuffer[VFile]()))
    } else {
      lastHash = hash
      try {
        classes.addAll(compile(sources))
      } catch {
        case ex: PathChangeException => // Don't bother with path changes here
        case ex: Throwable => lastHash = 0; throw ex
      }

    }
  }

  /**
   * inject ScalaTestRunner into play's test framework
   * @param testClass a class under testing
   */
  override def runTest(testClass: Class[BaseTest]) = {
    testClass match {
      case suite if classOf[Suite] isAssignableFrom testClass => ScalaTestRunner runSuiteClass suite.asInstanceOf[Class[Suite]]
      case junit if classOf[Assertions] isAssignableFrom testClass => ScalaTestRunner runJunitClass junit
      case _ => null
    }
  }

  /**
   * compile a class if a change was made.
   * @param modified classes that were modified
   */
  override def onClassesChange(modified: JList[ApplicationClass]) = {
    val sources = new java.util.ArrayList[VFile]
    modified foreach {
      cl: ApplicationClass =>
        var source = cl.javaFile
        if (!(sources contains source)) {
          sources add source
        }
    }
    compile(sources)
  }
  
  override def onConfigurationRead {
      if(!Play.configuration.getProperty("scala.enableAutoRedirect", "true").equals("false")) {
          Logger.warn("Automatic action redirect is deprecated for scala controllers. Add 'scala.enableAutoRedirect=false' to your application.conf file to disable it. Note that it will be the default in the final release.")
      }
  }


  class PathChangeException extends Exception

  /**
   * compiles all given source files
   * @param sources files to be compiled
   * @param return List of compiled classes
   */
  def compile(sources: JList[VFile]) = {
    detectChange
    val (classes, shouldReset) = compiler.compile(sources.toList)
    if (shouldReset) reset()
    detectChange
    classes
  }



}

object OnTheFly {
  
  def eval(code: String): Any = {
    play.Play.plugin(classOf[ScalaPlugin]).eval(code)
  }

}

