package MowItNow

import scala.collection.immutable.HashSet
import MowItNow.Tondeuse
class Pelouse (private var abscissemax: Int, private var ordonneemax: Int) {

    def setPelouse(abscisse:Int, ordonnee:Int): Unit ={
      abscissemax = abscisse
      ordonneemax = ordonnee
    }

    if (abscissemax == 0 || ordonneemax == 0) {
      throw new IllegalArgumentException("La pelouse doit contenir au moins une ligne et une colonne")
    }
    if (abscissemax < 0 || ordonneemax < 0) {
      throw new IllegalArgumentException("Le nombre de lignes et de colonnes ne peut pas être négatif")
    }

    private val abscissemin = 0
    private val ordonneemin = 0

    //On détermine si une tondeuse de situe en dehors des limites de la pelouse
    private def estHortLimite(tondeuse: Tondeuse) = {
      (abscissemax < tondeuse.getx || ordonneemax < tondeuse.gety || abscissemin > tondeuse.getx || ordonneemin > tondeuse.gety)

    }

    //On introduit une liste contenant les positions des tondeuses
    private var listePositionsTondeuses = new HashSet[(Int, Int)]

    //On détermine si une tondeuse se situe sur un point déjà occupé par une autre tondeuse
    private def estSurUnEndroitOccupé(tondeuse: Tondeuse) = listePositionsTondeuses.contains(tondeuse.obtenirPosition)

    //Lorsqu'on introduit une nouvelle tondeuse, on vérifie que sa position n'est pas hors limite et qu'une autre tondeuse ne s'y trouve pas déjà
    def introduireTondeuse(tondeuse: Tondeuse) {
      if (estSurUnEndroitOccupé(tondeuse)) {
        throw new IllegalArgumentException("Position déjà occupée: " + tondeuse)
      }

      if (estHortLimite(tondeuse)) {
        throw new IllegalArgumentException("Position hors limites: " + tondeuse)
      }
      //On ajoute sa position à la liste des positions occupées
      listePositionsTondeuses += tondeuse.obtenirPosition
  }
    private def enregistrerNouvellePosition(tondeuseAvant: Tondeuse, tondeuseApres : Tondeuse) = {
      listePositionsTondeuses = listePositionsTondeuses + tondeuseApres.obtenirPosition - tondeuseAvant.obtenirPosition
    }

    def deplacerTondeuse (tondeuse: Tondeuse, pelouse: Pelouse, instructions: String) = {
      instructions.foreach {
        case 'A' => {
          // On créer une copie de la tondeuse qu'on fait avancer à sa place
          var tondeuseTest = tondeuse.copy()
          tondeuseTest.avancer
          // Si la copie n'est pas hors limite, alors on fait avancer la "vraie" tondeuse
          // Sinon la tondeuse n'avance pas
          if (estHortLimite(tondeuseTest) == false && pelouse.estSurUnEndroitOccupé(tondeuseTest) == false) {
            enregistrerNouvellePosition(tondeuse, tondeuseTest)
            tondeuse.avancer
          }
        }
        case 'D' => tondeuse.tournerDroite
        case 'G' => tondeuse.tournerGauche
        case _ => throw new IllegalArgumentException("La commande doit être composée des lettres A, D et G")
      }
  }
}


