package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur
case object EstMarque extends Marqueur
case object EstNonMarque extends Marqueur

object RExpMatcher {

  /**
   * @param e une expression régulière
   * @return vrai ssi e représente une séquence vide
   */
  def expIsEmpty(e: RExp): Boolean = {
    e match {
      case Vide => true
      case NFois(e, _) => expIsEmpty(e)
      case Concat(e1, e2) => expIsEmpty(e1) && expIsEmpty(e2)
      case Choix(e1, e2) => expIsEmpty(e1) || expIsEmpty(e2)
      case _ => false
    }
  }

  /**
   * @param e une expression régulière
   * @param b une base azotée
   * @return la dérivée de Brzozowski de e par rapport à b
   */
  def derivee(e: RExp, b: Base): RExp = {
    e match {
      case Impossible => Impossible
      case Vide => Impossible
      case UneBase(base) => if (base == b) Vide else Impossible
      case Nqb => Vide
      case NFois(e1, n) => (derivee(e1, b), n) match {
        case (Impossible, _) => Impossible
        case (_, 0)  => Impossible
        case (_, 1) => Vide
        case (Vide, 2) => e1
        case (Vide, _) => NFois(e1, n - 1)
        case (ed1, 2) => Concat(ed1, e1)
        case (ed1, _) => Concat(ed1, NFois(e1, n - 1))
      }
      case Choix(e1, e2)  => {
        (derivee(e1, b), derivee(e2, b)) match {
          case (Impossible, Impossible) => Impossible
          case (Vide, Vide) => Vide
          case (Impossible, ed2) => ed2
          case (ed1, Impossible) => ed1
          case (ed1, ed2) => Choix(ed1, ed2)
        }
      }
      case Repete(e1) => derivee(e1, b) match {
        case Impossible => Impossible
        case Vide => Repete(e1)
        case value: RExp => Concat(value, Repete(e1))
      }
      case Concat(e1, e2) => {
        if (expIsEmpty(e1)) {
          (derivee(e1, b), derivee(e2, b)) match {
            case (Impossible, _) => Impossible
            case (_, Impossible) => Impossible
            case (Vide, Vide) => Vide
            case (Vide, expDeriveeE2) => Choix(e2, expDeriveeE2)
            case (expDeriveeE1, Vide) => Concat(expDeriveeE1, e2)
            case (expDeriveeE1, expDeriveeE2) => Choix(Concat(expDeriveeE1, e2), expDeriveeE2)
          }
        } else {
          derivee(e1, b) match {
            case Vide => e2
            case Impossible => Impossible
            case _ => Concat(derivee(e1, b), e2)
          }
        }
      }
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return vrai ssi la liste lb entière est décrite par e
   */
  def matchComplet(e: RExp, lb: List[Base]): Boolean = {
    lb match {
      case Nil => false
      case first :: tail => (derivee(e, first), tail)  match {
        case (Vide, Nil) => true
        case (Repete(_), Nil) => true
        case (Impossible, _) => false
        case (Vide, _) => false
        case (value, _) => matchComplet(value, tail)
      }
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite
   */
  def sequenceDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case first :: tail => (EstMarque, first) :: sequenceDecrite(tail)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb n'est pas décrite
   */
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case first :: tail => (EstNonMarque, first) :: sequenceNonDecrite(tail)
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = {
    lb match {
      case Nil => None
      case first :: tail => derivee(e, first) match {
        case Vide => Some(List(first))
        case Impossible => None
        case value: RExp => prefixeMatch(value, tail) match {
          case None => None
          case Some(value) => Some(first :: value)
        }
      }
    }
  }

  /**
   * @param pref une liste de bases azotées *préfixe* de lb
   * @param lb une liste de bases azotées
   * @return la sous-liste de lb située après le préfixe pref
   */
  def suppPrefixe(pref: List[Base], lb: List[Base]): List[Base] = {
    (pref, lb) match {
      case (Nil, _)                                   => lb
      case (_, Nil)                                   => Nil
      case (firstPREF :: restPREF, firstLB :: restLB) => {
        if (firstPREF != firstLB) {
          suppPrefixe(pref, restLB)
        } else {
          suppPrefixe(restPREF, restLB) match {
            case Nil => suppPrefixe(pref, restLB)
            case value: List[Base] => value
          }
        }
      }
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui EstMarque,
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  def tousLesMatchs(e: RExp, lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case first :: tail => prefixeMatch(e, lb) match {
        case None => (EstNonMarque, first) :: tousLesMatchs(e, tail)
        case Some(prefix) => sequenceDecrite(prefix) ++ tousLesMatchs(e, suppPrefixe(prefix, lb))
      }
    }
  }

  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   *
   * @note Attention, le message doit être un résumé du résultat, et non
   *                  la représentation textuelle de toute la séquence de bases marquées.
   * @note Ce message court sera aussi celui affiché dans l'interface graphique dans la sous-fenêtre
   *                  "Message".
   */
  def messageResultat(lbm: List[(Marqueur, Base)]): String = {
    lbm match {
      case Nil => "Aucune sous-séquence trouvée"
      case (EstNonMarque, _) :: tail => messageResultat(tail)
      case (EstMarque, _) :: _  => "Sous-séquences trouvées"
    }
  }

  /**
   * @param lb une liste de bases azotées marquées
   * @return liste des mêmes bases que lb, mais où tous les marqueurs indiquent
   *         une non-correspondance
   */
  def annulerResultat(lb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case (_, base) :: tail => (EstNonMarque, base) :: annulerResultat(tail)
    }
  }

  /**
   * @param lbm une liste de bases azotées marquées
   * @return la liste des bases de lbm dont on a oublié les marqueurs, en conservant l'ordre
   */
  def sansMarqueurs(lbm: List[(Marqueur, Base)]): List[Base] = {
    lbm match {
      case Nil => Nil
      case (_, base) :: tail => base :: sansMarqueurs(tail)
    }
  }

}