package fr.istic.si2.adnmatch

import fr.istic.si2.adnmatch.ADNMatchV1.lireExpression
import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 2
 */
object ADNMatchV2 extends App {

  println("ADNMatch Version 2")
  def boucle(): Unit = {
    val expression = lireExpression();
    print("Expression en chaîne de charactère : ")
    println(rExpToString(expression))
    val sequence = lireUneSequence()
    matchComplet(expression, sequence) match {
      case true => println("La séquence " + listeBasesToString(sequence)  + " correspond entièrement à l'expression " + rExpToString(expression))
      case false => println("La séquence " + listeBasesToString(sequence) + " ne correspond pas entièrement à l'expression " + rExpToString(expression))
    }
    println("Recommencer ? o/n")
    scala.io.StdIn.readLine() match {
      case "o" => boucle()
      case _   =>
    }
  }

  def lireUneSequence(): List[Base] = {
    println("Veuillez entrez une sequence:")
    lireSequence() match {
      case None => println("La séquence renseignée est incorrecte"); lireUneSequence();
      case Some(sequence) => sequence
    }
  }

  boucle()
}