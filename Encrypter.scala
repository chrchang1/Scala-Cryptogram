import scala.swing._
import scala.io._
import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
   /**
   * @author Chris Chang
   * Random encryption generator
   * 
   */



object Encrypter {
   /**
   * Creates a random encoding for an alphabet (all lower case)
   * in the file.
   * @return The Map of the randomly generated encoding.
   */
   val alphabet = (97 to 122)
  def generateRandomMapping() : Map[Char, Char] = {
	  val testMap = Map[Char, Char]()
	  val random = new scala.util.Random
	  val randValue = ListBuffer[Char]()
	  randValue += alphabet(random.nextInt(alphabet length)).toChar
	  val decodeList = ListBuffer[Char]()
	  for (x <- alphabet){
	    
		  while ((decodeList.exists(s => s == randValue(0))) || x == randValue(0)){
			  randValue(0) = alphabet(random.nextInt(alphabet length)).toChar
			  //println(randValue(0))
		  }
		  
		  decodeList += randValue(0)
		  testMap += (x.toChar -> randValue(0))
	  }
	  return testMap
  }
  def readMessage = {
    val chooser = new FileChooser
    val result = chooser.showOpenDialog(null)
    try {
      if (result == FileChooser.Result.Approve)
        Source.fromFile(chooser.selectedFile).getLines.toList
      else List("")
    } finally {
      Source.fromFile(chooser.selectedFile).close
    }
  }
  def main (args: Array[String]) = {
	  val input = readMessage
	  val mapping = generateRandomMapping()
	  for(x <- input){
	    println(x.toLowerCase map (y => mapping.getOrElse(y,y)))
	    println(mapping)
	  }
  }
   
   
}