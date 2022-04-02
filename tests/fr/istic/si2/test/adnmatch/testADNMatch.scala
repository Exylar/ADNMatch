package fr.istic.si2.test.adnmatch

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.adnmatch._

class ADNMatchTest {


  /**
   * Tests de comportement de la fonction ListeBasesToString
   */
  @Test
  def TestlisteBasesToString(): Unit = {
    val exampleBase1: List[Base] = Nil
    val exampleBase2: List[Base] = A :: Nil
    val exampleBase3: List[Base] = T :: T :: G :: Nil
    val exampleBase4: List[Base] = A :: T :: G :: C :: Nil
    val exampleBase5: List[Base] = A :: T :: T :: G :: T :: A :: A :: A :: A :: G :: C :: Nil

    assertEquals("Empty", "", listeBasesToString(exampleBase1))
    assertEquals("Base A", "A", listeBasesToString(exampleBase2))
    assertEquals("Base TTG", "TTG", listeBasesToString(exampleBase3))
    assertEquals("Base ATGC", "ATGC", listeBasesToString(exampleBase4))
    assertEquals("Compexe ", "ATTGTAAAAGC", listeBasesToString(exampleBase5))
  }

  /**
   * Tests de comportement de la fonction rExpToString
   */
  @Test
  def TestRExpToString(): Unit = {
    val exampleRExp1 = UneBase(T)
    val exampleRExp2 = Choix(UneBase(C), UneBase(G))
    val exampleRExp3 = Choix(Choix(UneBase(A), UneBase(T)), UneBase(G))
    val exampleRExp4 = Concat(UneBase(T), Choix(UneBase(A), UneBase(T)))
    val exampleRExp5 = Repete(UneBase(T))
    val exampleRExp6 = Repete(Choix(UneBase(A), UneBase(T)))
    val exampleRExp7 = NFois(UneBase(A), 2)
    val exampleRExp8 = NFois(Choix(UneBase(A), UneBase(T)), 3)
    val exampleRExp9 = Impossible
    val exampleRExp10 = Vide

    assertEquals("Base T", "T", rExpToString(exampleRExp1))
    assertEquals("Choix CG", "C|G", rExpToString(exampleRExp2))
    assertEquals("Choix de Choix AT et Base G", "(A|T)|G", rExpToString(exampleRExp3))
    assertEquals("Concat Base T et Choix AT", "T(A|T)", rExpToString(exampleRExp4))
    assertEquals("Répétition Base T", "T*", rExpToString(exampleRExp5))
    assertEquals("Répétition Choix AT", "(A|T)*", rExpToString(exampleRExp6))
    assertEquals("Répétion Base A 2 fois", "A{2}", rExpToString(exampleRExp7))
    assertEquals("Répétion Choix AT 3 fois", "(A|T){3}", rExpToString(exampleRExp8))
    assertEquals("Impossible", "@", rExpToString(exampleRExp9))
    assertEquals("Vide", "%", rExpToString(exampleRExp10))
  }

  /**
   * Tests de comportement de la fonction deroule
   *   Choix: e1 est choisi
   *   Nqb: une base T est choisie
   *   Répétition: boucle 3 fois
   */
  @Test
  def TestDeroule(): Unit = {
    val exampleDeroule1 = UneBase(T)
    val exampleDeroule2 = Choix(UneBase(C), UneBase(G))
    val exampleDeroule3 = Choix(Choix(UneBase(A), UneBase(T)), UneBase(G))
    val exampleDeroule4 = Concat(UneBase(T), Choix(UneBase(A), UneBase(T)))
    val exampleDeroule5 = Repete(UneBase(T))
    val exampleDeroule6 = Repete(Choix(UneBase(A), UneBase(T)))
    val exampleDeroule7 = NFois(UneBase(A), 2)
    val exampleDeroule8 = NFois(Choix(UneBase(A), UneBase(T)), 3)
    val exampleDeroule9 = Impossible
    val exampleDeroule10 = Vide

    assertEquals("Une base T", Some(List(T)), deroule(exampleDeroule1))
    assertEquals("Choix CG", Some(List(C)), deroule(exampleDeroule2))
    assertEquals("Choix de Choix CG et Base A", Some(List(A)), deroule(exampleDeroule3))
    assertEquals("Concat Base T et Choix AT", Some(List(T, A)), deroule(exampleDeroule4))
    assertEquals("Répétition base T", Some(List(T, T, T)), deroule(exampleDeroule5))
    assertEquals("Répétition Choix AT", Some(List(A, A, A)), deroule(exampleDeroule6))
    assertEquals("Répétition Base A 2 fois", Some(List(A, A)), deroule(exampleDeroule7))
    assertEquals("Répétition Choix AT 3 fois", Some(List(A, A, A)), deroule(exampleDeroule8))
    assertEquals("Impossible", None, deroule(exampleDeroule9))
    assertEquals("Vide", None, deroule(exampleDeroule10))
  }

  /**
   * Tests de comportement de la fonction derivee
   */
  @Test
  def TestDerivee(): Unit = {
    assertEquals("Impossible", Impossible, derivee(Impossible, T))
    assertEquals("Vide", Impossible, derivee(Vide, T))
    assertEquals("Une base T valide", Vide, derivee(UneBase(T), T))
    assertEquals("Une base T invalide", Impossible, derivee(UneBase(T), A))
    assertEquals("Répétition une base invalide", Impossible, derivee(Repete(UneBase(C)), A))

    assertEquals("N'importe quelle base", Vide, derivee(Nqb, A))
    assertEquals("N'importe quelle base", Vide, derivee(Nqb, C))
    assertEquals("N'importe quelle base", Vide, derivee(Nqb, G))
    assertEquals("N'importe quelle base", Vide, derivee(Nqb, T))

    assertEquals("Concat de base Example 1", Impossible, derivee(Concat(Concat(UneBase(G), UneBase(T)), UneBase(A)), A))
    assertEquals("Concat de base Example 2", Concat(Concat(UneBase(T), UneBase(G)), UneBase(C)), derivee(Concat(Concat(Concat(UneBase(A), UneBase(T)), UneBase(G)), UneBase(C)), A))
    assertEquals("Concat de base Example 3", UneBase(C), derivee(Choix(Concat(UneBase(A), UneBase(T)), Concat(UneBase(G), UneBase(C))), G))
    assertEquals("Concat de base Example 4", Impossible, derivee(Concat(Concat(Repete(UneBase(A)), UneBase(C)), UneBase(T)), C))
  }

  /**
   * Tests de comportement de la fonction matchComplet
   */
  @Test def TestMatchComplet(): Unit = {
    assertTrue("Une base A", matchComplet(UneBase(A), List(A)))
    assertTrue("Concat Base AT", matchComplet(Concat(UneBase(A), UneBase(T)), List(A, T)))
    assertTrue("Concat une base AT simple T", matchComplet(Choix(UneBase(A), UneBase(T)), List(T)))
    assertTrue("Concat une base AT simple A", matchComplet(Choix(UneBase(A), UneBase(T)), List(A)))
    assertTrue("Répétition une Base A", matchComplet(Repete(UneBase(A)), List(A)))
    assertTrue("Répétition choix AT", matchComplet(Repete(Choix(UneBase(A), UneBase(T))), List(A, T, T, T, A)))
  }

  /**
   * Tests de comportement de la fonction sequenceDecrite
   */
  @Test def TestSequenceDecrite(): Unit = {
    assertEquals("Vide", List(), sequenceDecrite(List()))
    assertEquals("Example 1", List((EstMarque, T)), sequenceDecrite(List(T)))
    assertEquals("Example 2 ", List((EstMarque, T), (EstMarque, A)), sequenceDecrite(List(T, A)))
    assertEquals("Example 3", List((EstMarque, T), (EstMarque, A), (EstMarque, G)), sequenceDecrite(List(T, A, G)))
  }

  /**
   * Tests de comportement de la fonction NonsequenceDecrite
   */
  @Test def TestSequenceNonDecrite(): Unit = {
    assertEquals("Vide", List(), sequenceNonDecrite(List()))
    assertEquals("Example 1", List((EstNonMarque, T)), sequenceNonDecrite(List(T)))
    assertEquals("Example 2 ", List((EstNonMarque, T), (EstNonMarque, A)), sequenceNonDecrite(List(T, A)))
    assertEquals("Example 3", List((EstNonMarque, T), (EstNonMarque, A), (EstNonMarque, G)), sequenceNonDecrite(List(T, A, G)))
  }

  /**
   * Tests de comportement de la fonction prefixeMatch
   */
  @Test def TestPrefixeMatch(): Unit = {
    assertEquals("Une base A dans la liste", Some(List(A)), prefixeMatch(UneBase(A), List(A)))
    assertEquals("Une base T dans la liste ", None, prefixeMatch(UneBase(T), List(A)))
    assertEquals("Une base dans la liste avec plusieurs éléments", Some(List(A)), prefixeMatch(UneBase(A), List(A, G, G, C)))
    assertEquals("Concat d'une base AT", Some(List(A, T)), prefixeMatch(Concat(UneBase(A), UneBase(T)), List(A, T, G, G, T, A)))
    assertEquals("Choix d'une base AG", Some(List(A)), prefixeMatch(Choix(UneBase(A), UneBase(G)), List(A, C, T, C, T, C)))
    assertEquals("Répétition 3 fois une base T", Some(List(T, T, T)), prefixeMatch(NFois(UneBase(T), 3), List(T, T, T, A, G, A)))
    assertEquals("NFois 3 mais pas présent", None, prefixeMatch(NFois(UneBase(T), 3), List(T, T, A, G, A)))
  }

  /**
   * Tests de comportement de la fonction prefixeMatch
   */
  @Test def TestSuppPrefixe(): Unit = {
    assertEquals("1 élément", List(G, C), suppPrefixe(List(A), List(A, G, C)))
    assertEquals("2 éléments", List(G, C), suppPrefixe(List(A, T), List(A, T, G, C)))
    assertEquals("3 éléments", List(G, C), suppPrefixe(List(T, A, G), List(T, A, G, G, C)))
  }

  /**
   * Tests de comportement de la fonction tousLesMatchs
   */
  @Test def TestTousLesMatchs(): Unit = {
    assertEquals("Liste Vide", List(), tousLesMatchs(UneBase(T), List()))
    assertEquals("Un élément marqué", List((EstMarque, A)), tousLesMatchs(UneBase(A), List(A)))
    assertEquals("Un élément non marqué", List((EstNonMarque, A)), tousLesMatchs(UneBase(T), List(A)))
    assertEquals("Deux élément marqué", List((EstMarque, A), (EstMarque, T)), tousLesMatchs(Concat(UneBase(A), UneBase(T)), List(A, T)))
    assertEquals("Deux élément non marqué", List((EstNonMarque, C), (EstNonMarque, G)), tousLesMatchs(Concat(UneBase(G), UneBase(C)), List(C, G)))
    assertEquals("Un élément marqué et le deuxième non", List((EstMarque, A), (EstNonMarque, T)), tousLesMatchs(UneBase(A), List(A, T)))
    assertEquals("Un élément non marqué et le deuxième oui", List((EstNonMarque, C), (EstMarque, G)), tousLesMatchs(UneBase(G), List(C, G)))
  }

  /**
   * Tests de comportement de la fonction annulerResultat
   */
  @Test def TestAnnulerResultat(): Unit = {
    assertEquals("Annule marque 1 élément", List((EstNonMarque, A)), annulerResultat(List((EstMarque, A))))
    assertEquals("Annule marque 2 éléments", List((EstNonMarque, A), (EstNonMarque, G)), annulerResultat(List((EstMarque, A), (EstMarque, G))))
    assertEquals("Annule marque 3 éléments", List((EstNonMarque, A), (EstNonMarque, G), (EstNonMarque, C)), annulerResultat(List((EstMarque, A), (EstMarque, G), (EstMarque, C))))
    assertEquals("Annule marque au milieu", List((EstNonMarque, A), (EstNonMarque, G), (EstNonMarque, C)), annulerResultat(List((EstNonMarque, A), (EstMarque, G), (EstNonMarque, C))))
  }

  /**
   * Tests de comportement de la fonction sansMarqueurs
   */
  @Test def TestSansMarqueurs(): Unit = {
    assertEquals("1 élément", List(A, G, C), sansMarqueurs(List((EstMarque, A), (EstNonMarque, G), (EstNonMarque, C))))
    assertEquals("2 élément", List(A, G, C), sansMarqueurs(List((EstNonMarque, A), (EstMarque, G), (EstNonMarque, C))))
    assertEquals("3 élément", List(A, G, C), sansMarqueurs(List((EstNonMarque, A), (EstNonMarque, G), (EstMarque, C))))
    assertEquals("Marque au milieu", List(A, G, C), sansMarqueurs(List((EstNonMarque, A), (EstMarque, G), (EstMarque, C))))
  }
}