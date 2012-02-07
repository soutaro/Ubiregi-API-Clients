package com.ubiregi

package object api {
  type StringMap = Map[String, Any]
  type RequestHeader = Map[String, String]
  def toJsonString(value: Any): String = value match {
    case null => "null"
    case o:Map[String, _] => "{" + o.map{case (k, v) => toJsonString(k) + ":" + toJsonString(v)}.mkString(",") + "}"
    case o:List[_] => o.map{v => toJsonString(v) }.mkString("[", "," , "]")
    case o:String => '"' + o + '"'
    case o => o.toString()
  }
}
