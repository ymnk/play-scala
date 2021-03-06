package play.db.sql

import scala.util.parsing.combinator._
object SqlParser extends JavaTokenParsers{
  def parse (in:String):(String,List[String])= {
    val r=parse(instr,in).get;
    (r.flatMap(_._1).mkString,(r.flatMap(_._2)))}
  def instr= rep( literal | variable | other)
  def literal= (stringLiteral | simpleQuotes) ^^ {case s => (s,None)}
  def variable="{"~>ident<~"}" ^^ {case s => ("?":String,Some(s))}
  def other=""".""".r ^^ {case element => (element,None)}
  def simpleQuotes=("'"+"""([^'\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"'").r
  override def skipWhitespace=false
}

