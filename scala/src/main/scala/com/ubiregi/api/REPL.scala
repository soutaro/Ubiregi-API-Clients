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
  val Connect  = """^connect.*""".r
  val Request  = """^request (.*)""".r
  
  private def repl(prompt: String): Unit = {
    val line = Console.readLine(prompt + " ").trim()
    line match {
      case Help() =>
        println("available commands:")
        println("""| > secret [secret]
                   | > token [token]
                   | > endpoint [endpoint]
                   | > show
                   | > connect
                   | > exit
                   | ! disconnect
                   | ! <request> """.stripMargin)
        repl(prompt)
      case Show() =>
        println("secret = " + secret)
        println("token = " + token)
        println("endpoint = " + endpoint)
        repl(prompt)
      case Secret(newSecret) =>
        secret = newSecret
        println("set secret = " + newSecret)
        repl(prompt)
      case Token(newToken) =>
        token = newToken
        println("set token = " + newToken)
        repl(prompt)
      case Endpoint(newEndpoint) =>
        endpoint = newEndpoint
        println("set endpoint = " + newEndpoint)
        repl(prompt)
      case Connect() =>
        client = new UbiregiClient(secret, token, endpoint)
        repl("!")
      case Exit(_) =>
        println("exit")
        client.shutdown()
        client = null
        return
      case line if line != "disconnect" && prompt == "!" =>
        val result = client.processRequest(line.stripLineEnd)
        printf("response: %s%n", result)
        repl(prompt)
      case "disconnect" if prompt == "!" =>
        repl(">")
      case line =>
        Console.err.printf("unknown command'%s'", line)
        repl(prompt)
    }
  }
  
  
  def main(args: Array[String]){
    val (options, value) = parseOptions(args.toList)
    secret = options.get("SECRET").getOrElse("nothing")
    token = options.get("TOKEN").getOrElse("nothing")
    endpoint = options.get("ENDPOINT").getOrElse(ENDPOINT_URL)
    repl(">")
  }
}
