package com.ubiregi.api

import dispatch._
import dispatch./._
import dispatch.json._
import scala.util.parsing.json._
import java.io.File
import java.util.Calendar
import java.util.UUID
import java.util.TimeZone

class UbiregiClient(val secret: String, val token: String, val endpoint: String) {
  JSON.globalNumberParser = {input => input.toInt}
  val client: Http = new Http()
    
  def shutdown(): Unit = {
    client.shutdown()
  }
    
  val defaultHeaders: RequestHeader = Map(
    "User-Agent" -> "Sample API Client; en",
    "X-Ubiregi-Auth-Token" -> token,
    "X-Ubiregi-App-Secret" -> secret
  )
    
  def accounts(block: StringMap => Any = null, id: Int = 1): Map[String, List[Any]] = {
    val response = _get("accounts/"+id)
    if (block != null) block(response)
    response("account").asInstanceOf[Map[String, List[String]]]
  }
    
  def menuItems(menuId: String)(implicit block: StringMap => Any = null): Any = {
    _index("menus/"+menuId+"/items", "items", acc=List(), block)
  }
    
  def menuCategories(menuId: String)(implicit block: StringMap => Any = null): Any = {
    _index("menus/"+menuId+"/categories", "categories", acc=List(), block)
  }
    
  def checkouts(implicit block: StringMap => Any = null): Any = {
    _index("checkouts", "checkouts", acc=List(), block)
  }
    
  def postCheckouts(checkouts: List[StringMap]): Any = {
    _post("checkouts", toJsonString(Map("checkouts" -> checkouts)))
  }
    
  def _index(url: String, collection: String, acc: List[Any] = Nil, block: StringMap => Any): Any = {
    val response = _get(url)
    if (block != null) block(response)
    val nextUrl = response("next-url").asInstanceOf[String]
    if (nextUrl != null)
      _index(nextUrl, collection, acc ++ response(collection).asInstanceOf[StringMap], null)
    else
      acc ++ response(collection).asInstanceOf[List[Any]]
  }
    
  def _get(urlOrPathInit: String, query: Map[Any, Any] = Map(), extHeaders: RequestHeader = Map()): Map[String, Any] = {
    val urlOrPath = if (urlOrPathInit.matches("""^http.*""")) urlOrPathInit else endpoint + urlOrPathInit
    Console.err.println("Sending GET request to " + urlOrPath + " ...")
    val headers = defaultHeaders ++ extHeaders
    client((url(urlOrPath) <:< headers) >- {json => JSON.parseFull(json).get}).asInstanceOf[Map[String, Any]]
  }
    
  def _post(urlOrPathInit: String, content: String, query: StringMap = Map(), extHeaders: RequestHeader = Map()): Any = {
    val urlOrPath = if (urlOrPathInit.matches("""^http.*""")) urlOrPathInit else endpoint + urlOrPathInit
    Console.err.print("Sending POST request to " + urlOrPath + " ...")
    val headers = defaultHeaders + ("Content-Type" -> "application/json") ++ extHeaders
    val result = client((url(urlOrPath).POST << (content.toString()) <:< headers) >- (json => JSON.parseRaw(json)))
    Console.err.println("Done")
  }
  def processRequest(command: String): String = {
    val ACCOUNTS = """accounts/([0-9]+)""".r
    val MENUS_MENUIDS_ITEMS = """menus/([0-9]+)/items""".r
    val CHECKOUTS = """checkouts""".r
    val MENU_CATEGORIES = """menus/([0-9]+)/categories""".r
    toJsonString(command match {
      case ACCOUNTS(idString) =>
        accounts(id=idString.toInt)
      case MENUS_MENUIDS_ITEMS(menuId) =>
        menuItems(menuId)
      case CHECKOUTS() =>
        checkouts()
      case MENU_CATEGORIES(menuId) =>
        menuCategories(menuId)
    })
  }
}
