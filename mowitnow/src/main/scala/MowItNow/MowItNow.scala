package MowItNow

import scala.io.Source.fromResource
import MowItNow.Pelouse
import MowItNow.Tondeuse
class MowItNow {
  def lectureFichier(path : String) {
    val fileLines = fromResource(path).getLines
    var pelouse = new Pelouse(1, 1)
    var tondeuse = new Tondeuse(0, 0, 'N')
    var i = 0
    for (line <- fileLines) {
      val mots = line.split(" ")

      //la première ligne qui correspond aux dimensions de la pelouse
      if (i == 0) {
        val abscissemax = mots(0).toInt
        val ordonneemax = mots(1).toInt
        pelouse.setPelouse(abscissemax, ordonneemax)
      }

      // les lignes 1,3,5 etc... qui correspondent aux positions initiales des tondeuses
      else if (i % 2 == 1) {
        var abscisse = mots(0).toInt
        var ordonnee = mots(1).toInt
        var direction = mots(2)(0)
        tondeuse.setTondeuse(abscisse, ordonnee, direction)
        pelouse.introduireTondeuse(new Tondeuse(abscisse, ordonnee, direction))
      }
      // les lignes 2,4,6 etc... qui correspondent aux instructions de déplacement des tondeuses
      else if (i % 2 == 0) {
        var instructions = line
        pelouse.deplacerTondeuse(tondeuse, pelouse, instructions)
        println("Tondeuse " + i / 2 + " : " + tondeuse)
      }
      i = i + 1
    }
  }
}
