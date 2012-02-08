package com.ubiregi.api

/*
 * REPL for testing ubiregi api version 3
 */
object REPL {
  private final val ENDPOINT_URL = "http://localhost:3000/api/3/"
  private final val LOGIN_URL = "%s/api/2/auth/" format (ENDPOINT_URL)
  private final val STATUS_URL = ENDPOINT_URL + "%s/status?since=%s"
  private final val REPORT_URL = ENDPOINT_URL + "%s/report"
  private final val CUSTOMERS_URL = ENDPOINT_URL + "%s/customers"
  private final val NOTES_URL = ENDPOINT_URL + "%s/notes"
  private final val NOTES_POST_URL = ENDPOINT_URL + "%s/notes"
  
  class UnknownCommandException(message: String = "") extends Exception(message)
  
  def parseOptions(args: List[String]): (Map[String, String], String) = args match {
    case "--secret"::secret::rest => val (m, v) = parseOptions(rest); (m ++ Map("SECRET" -> secret), v)
    case "--token"::token::rest =>   val (m, v) = parseOptions(rest); (m ++ Map("TOKEN" -> token), v)
    case "--endpoint"::endpoint::rest => val (m, v) = parseOptions(rest); (m ++ Map("ENDPOINT" -> endpoint), v)
    case value::rest => (Map(), value)
    case Nil => (Map(), "")
  }
  
  var secret: String = ""
  var token: String = ""
  var endpoint: String = ""
  var client: UbiregiClient = _
  
  val Show     = """^show""".r
  val Help     = """^help""".r
  val Secret   = """^secret (.*)""".r
  val Token    = """^token (.*)""".r
  val Endpoint = """^endpoint (.*)""".r
  val Exit     = """^(quit|exit).*""".r
  val Connect  = """^(c|connect).*""".r
  val UpdateOrNewCashier = """^(update|new)_cashier (.*)""".r
  
  private def help():Unit = {
    println("available commands:")
    println("""| > secret [secret]
               | > token [token]
               | > endpoint [endpoint]
               | > show
               | > c|connect
               | > exit
               | ! disconnect
               | ! update_cashier <json_file_path>
               | ! new_cashier <json_file_path>
               | ! <request> """.stripMargin)
  }
  
  private def show():Unit = {
    println("secret = " + secret)
    println("token = " + token)
    println("endpoint = " + endpoint)
  }
  
  private def secretCommand(newSecret: String):Unit = {
    secret = newSecret
    println("set secret = " + newSecret)
  }
  
  private def tokenCommand(newToken: String): Unit = {
    token = newToken
    println("set token = " + newToken)
  }
  
  private def endpointCommand(newEndpoint: String): Unit = {
    endpoint = newEndpoint
    println("set endpoint = " + newEndpoint)
  }
  
  private def updateOrNewCashier(jsonFilePath: String): Unit = {
    val encoder = new BASE64Encoder()
    val jsonString = openStream(jsonFilePath){in =>
      val bytes = readBytes(in)
      val iconBase64 = encoder.encodeBuffer(bytes)
      new String(bytes, "UTF-8")
    }
    printf("response: %s%n", client._post("accounts/current/cashiers", jsonString))
  }
   
  private def repl(prompt: String): Unit = {
    val line = Console.readLine(prompt + " ").trim()
    line match {
      case Help() =>
        help()
        repl(prompt)
      case Show() =>
        show()
        repl(prompt)
      case Secret(newSecret) =>
        secretCommand(newSecret)
        repl(prompt)
      case Token(newToken) =>
        tokenCommand(newToken)
        repl(prompt)
      case Endpoint(newEndpoint) =>
        endpointCommand(newEndpoint)
        repl(prompt)
      case Connect(_) =>
        client = new UbiregiClient(secret, token, endpoint)
        repl("!")
      case Exit(_) =>
        println("exit")
        client.shutdown()
        client = null
        return
      case UpdateOrNewCashier(_, jsonFilePath) =>
        updateOrNewCashier(jsonFilePath)
      case line =>
        if(prompt == "!")
          printf("response: %s%n", client.processRequest(line.stripLineEnd))
        else
          Console.err.printf("unknown command '%s' %n", line)
        repl(prompt)
    }
  }
  
  def main(args: Array[String]){
    val (options, _) = parseOptions(args.toList)
    secret = options.get("SECRET").getOrElse("nothing")
    token = options.get("TOKEN").getOrElse("nothing")
    endpoint = options.get("ENDPOINT").getOrElse(ENDPOINT_URL)
    repl(">")
  }
}
