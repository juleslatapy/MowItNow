package MowItNow

case class Tondeuse(private var abscisse : Int, private var ordonnee : Int, private var dir : Char){

  // encapsulation des variables x, y et dir (private), ainsi les tondeuses ne peuvent pas se "téléporter" :
  def getx = {abscisse}
  def gety = {ordonnee}
  def obtenirPosition = {(abscisse, ordonnee)}
  def getdir = {dir}

  // changement de la direction de 90° vers la gauche avec une structure case (préférable)
  def tournerGauche : Unit = dir match {
    case 'N' => dir = 'O'
    case 'O' => dir = 'S'
    case 'S' => dir = 'E'
    case 'E' => dir = 'N'
    case _ => throw new IllegalArgumentException("La direction doit être N, E, S ou O")
  }

  // changement de la direction de 90° vers la droite avec une structure else if (pas préférable)
  def tournerDroite = {
    if (dir == 'N') dir = 'E'
    else if (dir == 'E') dir = 'S'
    else if (dir == 'S') dir = 'O'
    else if (dir == 'O') dir = 'N'
    else throw new IllegalArgumentException("La direction doit être N, E, S ou O")
  }

  // changement de la position selon la direction considérée avec une structure case
  def avancer = dir match {
    case 'N' => ordonnee = ordonnee + 1
    case 'E' => abscisse = abscisse + 1
    case 'S' => ordonnee = ordonnee - 1
    case 'O' => abscisse = abscisse - 1
    case _ => throw new IllegalArgumentException("La direction doit être N, E, S ou O")
  }

  def setTondeuse(newAbscisse:Int, newOrdonnee:Int, newDir: Char): Unit ={
    abscisse = newAbscisse
    ordonnee = newOrdonnee
    dir = newDir  }

  // on retourne les caractéristiques de la tondeuse sous la forme demandée
  override def toString: String = s"$abscisse $ordonnee $dir"
}

