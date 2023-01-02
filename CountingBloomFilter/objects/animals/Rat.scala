package objects.animals

class Rat extends Animal{
  val specieName = "Rat"
  override def getSpecieName(): String = {
    specieName
  }
}