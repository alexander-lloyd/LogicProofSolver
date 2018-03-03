package com.csshackathon

import sun.misc.{Signal, SignalHandler}


trait Command {
  def action(tree: TokenTree): Unit
}

class Printer extends Command {
  override def action(tree: TokenTree): Unit = {
    println(PrettyPrint.pretty(tree))
  }
}

class Resolver extends Command {
  override def action(tree: TokenTree): Unit = ???
}

object Interpreter {
  def help(): Unit = {
    println("Help: ")
    println("\t-r Resolution Mode")
    println("\t-p Pretty Printer Mode")
  }


  def main(args:Array[String]): Unit = {

    // Get mode
    var command: Command = null

    if (args.length == 0) {
      help()
      sys.exit(1)
    }

    args.head match {
      case "-r" => command = new Resolver()
      case "-p" => command = new Printer()
      case unknown => {
        println("Unknown argument: " + unknown)
        help()
        sys.exit(1)
      }
    }

    while(true) {
      val input: String = Console.readLine(">>> ")

      val tokens: List[Token] = Scanner.scan(input.toList)

      val parser: Parser = new Parser(tokens)
      val tree: TokenTree = parser.parse()

      command.action(tree)

    }

  }

}
