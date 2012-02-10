package com.ubiregi
import java.io.InputStream
import java.io.FileInputStream

package object api {
  type StringMap = Map[String, Any]
  type RequestHeader = Map[String, String]
  type BASE64Encoder = sun.misc.BASE64Encoder
  type BASE64Decoder = sun.misc.BASE64Decoder
  type ==>[-A, +B] = PartialFunction[A, B]
  def toJsonString(value: Any): String = value match {
    case null => "null"
    case o:Map[String, _] => "{" + o.map{case (k, v) => toJsonString(k) + ":" + toJsonString(v)}.mkString(",") + "}"
    case o:List[_] => o.map{v => toJsonString(v) }.mkString("[", "," , "]")
    case o:String => '"' + o.flatMap{ case '\n' => """\n"""; case '\r' => """\r""" case other => "" + other } + '"'
    case o => o.toString()
  }
  val Cather = scala.util.control.Exception
  def openStream[A](path: String)(block: InputStream => A): A = {
    val stream = new FileInputStream(path)
    try { block(stream) } finally { Cather.allCatch(stream.close()) }
  }
  def readBytes(in: InputStream): Array[Byte] = {
    Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toByte).toArray
  }
}
