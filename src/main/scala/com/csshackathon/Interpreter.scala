package com.csshackathon

import java.util.logging.Logger
import java.util.logging.Level

import scala.collection.mutable



trait Command {
  def action(tree: TokenTree): Unit
}

class Printer extends Command {
  override def action(tree: TokenTree): Unit = {
    println(PrettyPrint.pretty(tree))
  }
}

class Resolver extends Command {
  override def action(tree: TokenTree): Unit = {
    var logger: Logger = Logger.getLogger(this.getClass.getCanonicalName)
    logger.setLevel(Level.OFF)
    logger.info("Original " + PrettyPrint.pretty(tree))
    val cnf = CNF.toCNF(tree)
    logger.info("CNF " + PrettyPrint.pretty(cnf))
    val sets: Set[Set[TokenTree]] = CNF.toSets(cnf)
    val mutableSets = mutable.Set(sets.toArray)
    val didResolve = Resolution.resolution(mutableSets)
    println(didResolve)
  }
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

    args.head.toLowerCase match {
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
