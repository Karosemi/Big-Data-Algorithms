package objects.animals

class Wolf extends Animal{
  val specieName = "Rabbit"
  override def getSpecieName(): String = {
    specieName
  }
}