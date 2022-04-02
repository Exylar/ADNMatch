package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

object SequencesImages {

  /**
   * @param lmb une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return une liste contenant des sous-listes de lmb, toutes de taille tligne, sauf la dernière qui
   *         peut être de taille inférieure.
   */
  def lignes(lmb: List[(Marqueur, Base)], tligne: Int): List[List[(Marqueur, Base)]] = {
    recupereReste(lmb, tligne) match {
      case Nil => List(lmb)
      case tail: List[(Marqueur, Base)] => List(recupereLigne(lmb, tligne)) ++ lignes(tail, tligne)
    }
  }

  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return la liste lmb sans les tligne premieres base marquées
   */
  def recupereReste(lmb: List[(Marqueur, Base)], tligne: Int): List[(Marqueur, Base)] = {
    tligne match {
      case 0 => lmb
      case _ => lmb match {
        case Nil => Nil
        case _ :: tail => recupereReste(tail, tligne - 1)
      }
    }
  }
  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return les "tligne" premieres bases marquées de la liste lmb
   */
  def recupereLigne(lmb: List[(Marqueur, Base)], tligne: Int): List[(Marqueur, Base)] = {
    tligne match {
      case 0 => Nil
      case _ => lmb match {
        case Nil => Nil
        case first :: tail => List(first) ++ recupereLigne(tail, tligne - 1)
      }
    }
  }

  /**
   * Taille du texte à utiliser pour représenter
   * graphiquement les bases azotées.
   */
  val fontSizeBase: Int = 14

  /**
   * @param mb une base azotée marquée
   * @return une image représentant la base avec son marqueur visuel
   */
  def marqueurBaseToImage(mb: (Marqueur, Base)): Image = {
    mb match {
      case (EstMarque, base) => FillColor(Text(base.toString, fontSizeBase), Color(255, 0, 0, 255))
      case (EstNonMarque, base) => FillColor(Text(base.toString, fontSizeBase), Color(0, 0, 0, 255))
    }
  }

  /**
   * @param ligne une liste de bases azotées marquées
   * @return une image représentant les bases marquées de ligne, dans l'ordre, toutes sur la même ligne
   */
  def imageUneLigne(ligne: List[(Marqueur, Base)]): Image = {
    ligne match {
      case Nil => Empty
      case first :: tail => Beside(marqueurBaseToImage(first), imageUneLigne(tail))
    }
  }

  /**
   * @param llignes une liste de listes de bases azotées marquées
   * @return une image représentant les bases marquées de llignes, dans l'ordre.
   *         Chaque élément de llignes est sur une ligne distincte.
   *         Les lignes sont visualisées les unes en dessous des autres.
   */
  def imagePlusieursLignes(llignes: List[List[(Marqueur, Base)]]): Image = {
    llignes match {
      case Nil => Empty
      case first :: tail => Below(imageUneLigne(first), imagePlusieursLignes(tail))
    }
  }

}