package soda.tiles.emotion.main

/*
 * This package is for the main class.
 * This is the entry point when the application is executed from a terminal.
 */

import   java.io.FileReader

/**
 * This is the main entry point.
 */

trait Main
{



  lazy val help = "" +
    "\nEmotional Reasoning" +
    "\n" +
    "\nCopyright 2026 Julian Alfredo Mendez" +
    "\n" +
    "\nhttps://github.com/julianmendez/emotional-reasoning" +
    "\n" +
    "\nEmotional Reasoning is a project that instantiates the Tiles framework." +
    "\n" +
    "\n"

  def main (arguments : Array [String] ) : Unit =
    println (help)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

