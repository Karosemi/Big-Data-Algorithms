package objects.animals

class Rabbit extends Animal{
  val specieName = "Rabbit"
  override def getSpecieName(): String = {
    specieName
  }
}