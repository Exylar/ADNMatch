package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatchlib._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatch.SequencesImages._

/**
 * Type algébrique décrivant l'état interne de l'application ADN Match V3
 * On doit à minima disposer :
 * - d'une expression régulière
 * - d'une liste de bases azotées, où chaque base est annotée par un marqueur
 * On introduit aussi une distinction entre :
 * - l'état initial,
 * - un état d'erreur,
 * - un état normal,
 * - et l'état final.
 */
sealed trait Etat
case object Init extends Etat
case class Normal(re: RExp, s: List[(Marqueur, Base)]) extends Etat
case class Erreur(re: RExp, s: List[(Marqueur, Base)]) extends Etat
case object End extends Etat

object ADNMatchUniverse extends Universe[Etat] {

  /**
   * Titre de la fenêtre graphique pour l'application réactive
   */
  val name: String = "ADN Match"

  /**
   * Largeur en pixels de la fenêtre graphique
   */
  val WIDTH: Int = 800

  /**
   * Hauteur en pixels de la fenêtre graphique
   */
  val HEIGHT: Int = WIDTH

  /**
   * @return l'état initial de l'application
   */
  def init: Etat = Init

  /**
   * Lit au clavier une chaîne de caractères représentant une expression régulière.
   * Validation au clavier avec la touche 'entrée'
   * @return si elle existe, l'expression régulière correspondant à la chaîne entrée
   */
  def nouvelleRExp(): Option[RExp] = {
    println("Entrez votre RExp: ")
    litRExp(io.StdIn.readLine())
  }

  /**
   * @param s l'état courant de l'application réactive
   * @param e un événement scribble
   * @return le nouvel état de l'application réactive, après que e ait eu lieu dans s
   */
  def react(s: Etat, e: Event): Etat = {
    (s, e) match {

      // Chargement de fichier accessible à tout moment
      case (_, KeyPressed(KeyAscii('f'))) =>
        println("Fichier à charger depuis le répertoire fichiers/ (validez par Entrée) :")
        val filename: String = "fichiers/" + scala.io.StdIn.readLine()
        lireSequenceFichier(filename) match {
          case Some(lb) => Normal(Impossible, sequenceNonDecrite(lb))
          case None     => println("Attention, lecture du fichier " + filename + " a échoué"); s
        }

      // Saisie d'une expression régulière possible dans un état d'erreur ou normal
      case (Erreur(re, lsm), KeyPressed(KeyAscii('e'))) =>
        val nms = annulerResultat(lsm)
        nouvelleRExp() match {
          case Some(re) => Normal(re, nms)
          case None     => Erreur(re, nms)
        }

      case (Normal(re, lsm), KeyPressed(KeyAscii('e'))) =>
        val nms = annulerResultat(lsm)
        nouvelleRExp() match {
          case Some(re) => Normal(re, nms)
          case None     => Erreur(re, nms)
        }

      // Déterminer la correspondance complète: une séquence et une expression régulière doivent être présents
      case (Normal(re, lsm), KeyPressed(KeyAscii('c'))) =>
        val noMarks: List[Base] = sansMarqueurs(lsm)
        if (matchComplet(re, noMarks)) Normal(re, sequenceDecrite(noMarks))
        else Normal(re, sequenceNonDecrite(noMarks))

      // Rechercher le motif: une séquence et une expression régulière doivent être présents
      case (Normal(re, lsm), KeyPressed(KeyAscii('m'))) =>
        Normal(re, tousLesMatchs(re, sansMarqueurs(lsm)))

      // Effacer la recherche de motif: une séquence et une expression régulière doivent être présents
      // On conserve l'expression régulière
      case (Normal(re, lsm), KeyPressed(KeyAscii('a'))) =>
        Normal(re, annulerResultat(lsm))

      // Arrêter l'application : possible à tout moment
      case (_, KeyPressed(KeyAscii('q'))) => End

      case _                              => s
    }
  }

  /**
   * Condition d'arrêt de l'application réactive
   * @param s l'état courant de l'application
   * @return vrai ssi l'application réactive se suspend dans cet état (état final)
   */
  def stopWhen(s: Etat): Boolean = {
    s == End
  }

  /**
   * @param s état de l'application réactive
   * @return une image représentant graphiquement l'état s
   */
  def toImage(s: Etat): Image = {
    s match {
      case Init           => cadre("Appuyez sur la touche f pour charger une sequence", Vide, Nil)
      case End            => cadre("Fini!", Vide, Nil)
      case Normal(re, lw) => cadre(messageResultat(lw), re, lw)
      case Erreur(re, lw) => cadre("RExp saisie invalide, veuillez ré-essayer", re, lw)
    }
  }

  /**
   * Constantes utilisées pour la fenêtre graphique
   */
  val tligne: Int = 80
  val fontsize: Int = 20
  val msgSize: Float = 1.1f * fontsize
  val seqSize: Float = 0.9f * HEIGHT

  /**
   * @param c une couleur
   * @return une image pour séparer les différentes zones de la fenêtre graphique
   */
  def sep(c: Color): Image = {
    Below(
      FillColor(LineColor(Rectangle(WIDTH, (HEIGHT - seqSize - (3 * msgSize)) / 2), c), c),
      Rectangle(WIDTH, 1))
  }

  /**
   * @param d une chaîne de caractères
   * @param i une image
   * @param front une couleur pour la police de caractères
   * @param back une couleur pour l'arrière plan
   * @return une image comprenant i, et indiquant sa description d au dessus
   */
  def description(d: String, i: Image, front: Color, back: Color): Image = {
    BelowAlign(
      Left,
      onAlign(
        Left,
        FillColor(Text(d, fontsize), front),
        FillColor(LineColor(Rectangle(WIDTH, msgSize), back), back)),
      i)
  }

  /**
   * @param t une chaîne de caractères
   * @param descr un boolean
   * @param front une couleur pour la police de caractères
   * @param back une couleur pour l'arrière plan
   * @return une image reduite au texte t si descr est faux
   *         une image du text t et sa description "Message" sinon
   */
  def message(t: String, descr: Boolean, front: Color, back: Color): Image = {
    val msg = On(
      FillColor(Text(t, fontsize), front),
      FillColor(LineColor(Rectangle(WIDTH, msgSize), back), back))
    if (descr) {
      description("Message:", msg, front, back)
    } else {
      msg
    }
  }

  /**
   * @param lmb une liste de bases marquées
   * @return une image illustrant lmb, avec des lignes de tligne bases
   */
  def sequence(lmb: List[(Marqueur, Base)]): Image = {
    val GREY: Color = Color(0, 0, 0, 30)
    description(
      "Séquence chargée:",
      OnAlign(
        Top,
        imagePlusieursLignes(lignes(lmb, tligne)),
        FillColor(LineColor(Rectangle(WIDTH, seqSize), GREY), GREY)),
      BLACK,
      GREY)
  }

  /**
   * @param re une expression régulière
   * @return une image illustrant re
   */
  def imageRExp(re: RExp): Image = {
    description(
      "Expression régulière recherchée:",
      message(rExpToString(re), false, GREEN, BLACK),
      GREEN, BLACK)
  }

  /**
   * @param t une chaîne
   * @param re une expression régulière
   * @param lmb une liste de bases marquées
   * @return une image permettant de visualiser :
   *         - un message contenant le texte t
   *         - l'expression régulière recherchée re
   *         - la liste de bases marquées lmb
   */
  def cadre(t: String, re: RExp, lmb: List[(Marqueur, Base)]): Image = {
    below(message(t, true, WHITE, BLUE), sep(BLUE), imageRExp(re), sep(BLACK), sequence(lmb))
  }

}