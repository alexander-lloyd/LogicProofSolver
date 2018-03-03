package com.csshackathon

import sun.misc.{Signal, SignalHandler}

object Interpreter extends App {
  val start = System.nanoTime()

  Signal.handle(new Signal("INT"), new SignalHandler() {
    def handle(sig: Signal) {
      println(f"\nProgram execution took ${(System.nanoTime() - start) / 1e9f}%f seconds\n")
      sys.exit(0)
    }
  })

  while(true) {
    val input: String = Console.readLine(">>> ")

    val tokens: List[Token] = Scanner.scan(input.toList)

    val parser: Parser = new Parser(tokens)
    val tree: TokenTree = parser.parse()

    // TODO: Apply Resolution
    println(tree)
  }

}
