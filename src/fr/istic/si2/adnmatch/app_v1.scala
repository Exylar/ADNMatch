package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  println("ADNMatch Version 1")

  def boucle(): Unit = {
    val expression = lireExpression();
    print("Expression en chaîne de charactère : ")
    println(rExpToString(expression))
    println(deroule(expression) match {
      case None => "L'expression renseignée ne décrit aucune séquence"
      case Some(bases) => print("Séquence décrite : "); listeBasesToString(bases)
    })
    println("Recommencer ? o/n")
    scala.io.StdIn.readLine() match {
      case "o" => boucle()
      case _ =>
    }
  }

  def lireExpression(): RExp = {
    println("Veuillez entrez une expression régulière:")
    litRExp(scala.io.StdIn.readLine()) match {
      case None => println("L'expression renseignée est incorrecte"); lireExpression();
      case Some(expression) => expression
    }
  }

  boucle()
}