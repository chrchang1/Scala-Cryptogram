import scala.swing._
import scala.io._
import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer
object Cryptogram {


  val alphabetDic = (("e t a o i n s r h l d c u m f p g w y b v k x j q z").split("[ ]+")).toList
  val oneWordDic = (("a i").split("[ ]+")).toList
  val twoWordDic = (("of to in it is be as at so we he by or on do if me my up an go no us am").split("[ ]+")).toList
  val threeWordDic = (("the and for are but not you all any can had her was one our out day get has him his how man new now old see two way who boy did its let put say she too use").split("[ ]+")).toList
  val fourWordDic = (("that with have this will your from they know want been good much some time very when come here just like long make many more only over such take than them well were").split("[ ]+")).toList
  val validDictionary = (("the of and a to in is you that it he for was on are as with his they at be this from i have or by one had not but what all were when we there can an your which their said if do will each about how up out them then she many some so these would other into has more her two like him see time could no make than first been its who now people my made over did down only way find use may water long little very after words called just where most know get through back much before go good new write our used me man too any day same right look think also around another came come work three word must because does part even place well such here take why things help put years different away again off went old number great tell men say small every found still between name should Mr home big give air line set own under read last never us left end along while might next sound below saw something thought both few those always looked show large often together asked house don't world going want school important until form food keep children feet land side without boy once animals life enough took sometimes four head above kind began almost live page got earth need far hand high year mother light parts country father let night following picture being study second eyes soon times story boys since white days ever paper hard near sentence better best across during today others however sure means knew it's try told young miles sun ways thing whole hear example heard several change answer room sea against top turned learn point city play toward five using himself usually").split("[ ]+")).toList  
  	/**
   * Asks the user to choose an input file, and returns a list of lines
   * in the file.
   * @return The lines in the file.
   */
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
  

  
  
   /**
   * Checks to see if the encrypted word has the same 
   * pattern as a specified word from the dictionary
   * @cypherWord The encrypted word to test
   * @dicWord The word in the dictionary to compare cypherWord to
   * @return True if the cypherWord has the same pattern as dicWord, or false.
   */
  
  def isMatch(cypherWord: String, dicWord: String) : Boolean = {
    if (cypherWord.length != dicWord.length) return false
    for (i <- (0 to cypherWord.length-1)){
      for (j <- (0 to (i-1))){
        if ((cypherWord.charAt(i) == cypherWord.charAt( j)) != (dicWord.charAt(i) == dicWord.charAt(j))){
          return false
        }
      }
    }
    return true
  }
  
  /**
   * checks to see if the specified word pattern appears in the specified word list
   * @word The word you want to check in the wordList
   * @wordList The list of words you want to search through
   * @return True if the word pattern appears in the wordList
   */
  def containsMatching(word: String, wordList: List[String]) : Boolean = {
    for (x <- wordList){
      if (isMatch(word, x)){
        return true
      }
    }
    return false
  }
  
   /**
   * Formats a string so that it is only lowercase text and splits it into a list that is sorted in order of increasing length
   * @sentence The string you want to format
   * @return A list that is the sentence parsed by spacing and filtered for just spaces and lower case letters.
   */
  
  def formatSentence(sentence: String) : List[String] = {
    ((sentence.split("[ ]+")).toList).sortWith((x1, x2) => x1.length < x2.length)  
  }
  
  

   /**
   * Creates a Map that shows the frequency of each unique pattern in a sentence
   * @cipherList The List[String] of unique patterns you want to find the frequency of 
   * @sentence The List[String] you want to analyze
   * @return Map of the frequency of each unique pattern
   */
  
  def findPatternFreq(cipherList: List[String], sentence: List[String]) : Map[String, Int] = {
    var patternMap = Map[String, Int]()
    for (x <- cipherList){
      patternMap += (x -> 1)
    }
    for (x <- cipherList){
      for (y <- sentence){
        if(isMatch(x,y)){
          patternMap += (x -> (patternMap(x) + 1))
        }
      }
    }
    return patternMap
  }
    
   /**
   * Returns the number of unique characters in a string
   * @word The word you want to search
   * @return An Int of the number of unique characters in a string
   */
  
  def uniqueChar(word: String) : Int = {
    val totalChar = ListBuffer[Char]()
    for (x <- word){
      if (!totalChar.contains(x)){
        totalChar += x
      }
    }
    return totalChar.length
  }
  
  var finalMapping = Map[Char,Char]()
  val maxScore = ListBuffer[Double]()
  maxScore += 0.0
  
  
   /**
   * Recursively solves the permutation given a specific number of inputs
   * @currentWords The encrypted word list that you are given
   * @dictionary The dictionary of valid words
   * @sentence The encrypted sentence to decrypt
   * @currentPatternSet The bank of feasible dictionary valid words for each word in the currentWord list
   * @permutation The partial solution to the encryption
   */
  
  def solve(currentWords: List[String], dictionary: List[String], sentence: List[String], currentPatternSet: Map[String, List[String]], permutation: Map[Char,Char], count: Int){
   val vCount = new ListBuffer[Int]()
   vCount += count
    if (getScore(sentence, permutation, validDictionary) > maxScore(0)){
      maxScore(0) = getScore(sentence, permutation, validDictionary)
      finalMapping  = permutation
    }
    //halt when the current words list is empty
    if((currentWords).length < 1){
      return
    }
    if(count > 15){
      return
    }
    //println("CURRENTWORDHEAD : " + currentWords.head)
    //println(currentPatternSet)
    var headValidWords = List[String]()

    for(j <- currentPatternSet){
      if(isMatch(j._1, currentWords.head)){
            headValidWords = currentPatternSet(j._1)
      }
    }
    
    if(headValidWords.length > 0){
    
    
   // println("HEAD" + headValidWords)
    //for each word in the dictionary that the head of the current words could be 
	    for (x <- headValidWords){
		       //update the decoder to reflect the node being mapped as the word x
		      val newPermutation = updatePermutation(currentWords.head, x, permutation)
		      val validList = getValidList(currentWords.filterNot(z => (z==currentWords.head)), validDictionary, newPermutation)
		      //time to recurse on the remaining words
		      if(validList.length > 0){

		        //remove the mapped x word from the dictionary
			      //if(updateAvailableWords(currentPatternSet, newPermutation).contains(validList.head)){
			      vCount(0) = vCount(0) + 1
			      solve(validList, dictionary, sentence, updateAvailableWords(currentPatternSet, newPermutation), newPermutation, (vCount(0)))
			      //}
		      }
	      
	    
	    }
    }
    
    //solve assuming the current word is not mapped
    vCount(0) = vCount(0) + 1
   solve(currentWords.tail, dictionary, sentence, currentPatternSet, permutation, (vCount(0)))

  }
  
  /**
   * Updates the given permutation to reflect additional character
   * entries from binding a given encrypted word to a valid dictionary word
   * @word The encrypted word that you want to bind
   * @validWord The dictionary valid word that you want to bind the word to
   * @permutation The partial solution that you are modifying
   * @return An updated permutation solution
   */
  
  def updatePermutation(word: String, validWord: String, permutation: Map[Char, Char]) : Map[Char,Char] = {
    var newPermutation = permutation
    val isMapped = new ListBuffer[Boolean]()
    isMapped += false
    for (x <- (0 to (word.length - 1))){
      //if the current char is not mapped yet, add it to the map
      if (!permutation.contains(word.charAt(x))){
        isMapped(0) = false
        for (y <- permutation){
        	if(y._2 == validWord.charAt(x)){
        	  isMapped(0) = true
        	}
        }
        if(!isMapped(0)){
        	newPermutation += (word.charAt(x) -> validWord.charAt(x))
        }
      }
    }
    //println(newPermutation)
    return newPermutation
  }
  
   /**
   * Returns the number of unique characters in a string
   * @word The word you want to search
   * @return An Int of the number of unique characters in a string
   */
  
  def decodeWord(word: String, permutation: Map[Char, Char]) : String = {
    word map (x => permutation.getOrElse(x,x))
  }

   /**
   * Checks to see if a given word (when using a partial mapping) can exist in the dictionary
   * @word The word you want to find
   * @dictionary The dictionary you use
   * @permutation The partial permutation 
   * @return True if the given word when partially permuted can exist in the dictionary
   */
  
  def isValidWord(word: String, dictionary: List[String], permutation: Map[Char, Char]) : Boolean = {
    var isValid = false
    for (validWord <- dictionary){
      if(validWord.length == word.length){
	      isValid = true
		    for (x <- (0 to (word.length - 1))){
		      if (permutation.contains(word.charAt(x))){
		        if (permutation(word.charAt(x)) != validWord.charAt(x)){
		          //println("word is: " + word)
		          //println("mapping is: " + permutation)
		        isValid = false
			    }
			  }
		    }
	      if(isValid){ return true}
	  }
    }
    return false
  }
  
   /**
   * Takes a list of words and a new permutation and calculates the new list of valid words
   * @fList Original list of encrypted valid words
   * @dictionary The dictionary you will use
   * @permutation The new permutation to filter the list
   * @return A new list of encrypted words that are consistent with the dictionary and permutation
   */
  //get list of valid words after updating the words
  def getValidList(fList: List[String], dictionary: List[String], permutation: Map[Char, Char]) : List[String] = {
    var newFilteredList = List[String]()
    //for each item in fList
    for (pattern <- fList){
      if(isValidWord(pattern, dictionary, permutation)){
        newFilteredList = pattern :: newFilteredList
      }
    }
    //println("NEWFILTERED: " + filteredList)
    return newFilteredList.reverse
 }

   /**
   * Takes a list of words and a new permutation and calculates the new Map of valid encrypted words and their possible dictionary words
   * @patternSet Original map of possible dictionary words 
   * @permutation The new permutation to filter the list
   * @return A new map of encrypted words and their possible dictionary words that are consistent with the permutation
   */
  
  def updateAvailableWords(patternSet: Map[String, List[String]], permutation: Map[Char, Char]) : Map[String, List[String]] = {
    var newPattern = Map[String, List[String]]()
    //println(permutation)
    for (x <- patternSet){
     // println(x._1)
      var newList = List[String]()
      for (word <- x._2){
    	  if(isExtendable(x._1, word, permutation)){
    	    if(!validDictionary.contains(x._1 map (y => permutation.getOrElse(y,y)))){
    	    	newList = word :: newList
    	    }
    	  }
      }
      if(newList.length > 0){
    	  newPattern += (x._1 -> newList)
      }
    }
    //Remove words that are already fully mapped by the permutation
    
    return newPattern
  }
  
     /**
   * Checks to see if the given scrambled word can match the given valid word with the new permutation
   * @scrambleWord The word you want to check
   * @validWord The dictionary word you want to compare to
   * @permutation The permutation you are checking with
   * @return True or false
   */
  
  def isExtendable(scrambleWord: String, validWord: String, permutation: Map[Char, Char]) : Boolean = {
    var isValid = false
      if(validWord.length == scrambleWord.length){
	      isValid = true
		    for (x <- (0 to (scrambleWord.length - 1))){
		      if (permutation.contains(scrambleWord.charAt(x))){
		        if (permutation(scrambleWord.charAt(x)) != validWord.charAt(x)){
		          //println("word is: " + word)
		          //println("mapping is: " + permutation)
		        isValid = false
			    }
			  }
		    }
	      if(isValid){ return true}
      }
    return false
  }
  
     /**
   * Initializes the tree of permutations that you will recurse on to find the best possible permutation.
   * @cipherWords The list of encrypted words you will use to test
   * @dictionary The dictionary you will use
   * @sentence The complete list of words you want to decode
   */
 
  def createTree(cipherWords: List[String], dictionary: List[String], sentence: List[String]){
    val patternList = (((findPatternFreq(cipherWords, sentence)).toList).sortBy(x => uniqueChar(x._1) / x._2)).reverse
    val filteredList = ListBuffer[String]()
    //println(patternList)
    
    //get the list of unique patterns that are in the sentence and are also valid dictionary words
    for (x <- dictionary){
      for(y <- patternList){
         if(isMatch(y._1, x)){
          if(!filteredList.contains(y._1)){
        	  filteredList += y._1
          }
    	}
      }
    }
    //println("FILTEREDLIST: " + filteredList)
    var patternMap = Map[String, List[String]]()
    for (x <- filteredList.reverse){
      for (y <- dictionary){
        if(isMatch(x,y)){
          if(!patternMap.contains(x)){
   	       patternMap += (x -> (y :: List[String]()))        
          }
          else{
	       patternMap += (x -> (y :: patternMap(x)))  
          }
        }
      }
    }
    var newFilteredList = List[String]()
    for(x <- filteredList.reverse){
      for(y <- dictionary){
	      for(z <- sentence){
	        if(isMatch(x,y)){
		        if(isMatch(y,z)){
		          if(!newFilteredList.contains(z)){
		            newFilteredList = z :: newFilteredList
		          }
		        }
	        }
	      }
      }
    }
    
    for (x <- patternMap){
      patternMap += (x._1 -> (x._2).reverse)
    }
    newFilteredList = (newFilteredList.distinct).reverse
    solve(newFilteredList, dictionary, sentence, patternMap, Map[Char, Char](), 1)


  }
  
  /**
   * Calculates the accuracy of the current permutation
   * @sentence The sentence you are comparing to
   * @partialMapping The partial permutation you are translating with
   * @dictionary The dictionary you are using
   * @return A score (higher is better)
   */
  def getScore(sentence: List[String], partialMapping: Map[Char, Char], dictionary: List[String]) : Double = {
    val listBuffer = new ListBuffer[Double]()
    listBuffer += (0.0)
    listBuffer += (0.0)
    listBuffer += (0.0)
    var translation = ""
    for (x <- sentence){
      translation = x map(y=> partialMapping.getOrElse(y,y))
      listBuffer(1) = 0.0
      for (y <- dictionary){
        listBuffer(2) = 0.0
        if (translation.length == y.length){
          for(z <- (0 to (y.length - 1))){
        	  if(translation.charAt(z) == y.charAt(z)){
        	    listBuffer(2) += 1.0
        	  }
          }
          if(listBuffer(2) > listBuffer(1)){
            listBuffer(1) = listBuffer(2)
          }
        }
      }
     listBuffer(0) = listBuffer(0) + (listBuffer(1) / x.length)


    }
    //println(listBuffer(0))
    return listBuffer(0)
  }
  
  def main (args: Array[String]) {
	  val encoding = readMessage
	  val result = encoding(0)
	  createTree(formatSentence(result), validDictionary, ((result).split("[ ]+")).toList)
	  val guessedLetters = ListBuffer[Char]()
	  val rawString = (result).toList
	  val unmappedChars = ((rawString.filter(y => (y!= 32) && (!finalMapping.contains(y)))).distinct).filter(x => (97 to 122).contains(x))
	  for(x <- finalMapping){
	    guessedLetters += x._2
	  }
	  for (x <- unmappedChars){
	    for (y <- alphabetDic){
	      if(!guessedLetters.contains(y.charAt(0)) && (x!=y.charAt(0)) && (!finalMapping.contains(x))){
	        finalMapping += (x -> y.charAt(0))
	        guessedLetters += y.charAt(0)
	      }
	    }
	  }
	  val decode = result map (x => finalMapping.getOrElse(x,x))
	  println(decode)
  }
  
}