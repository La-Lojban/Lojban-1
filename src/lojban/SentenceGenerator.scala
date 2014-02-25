package lojban

import scala.util.Random

object SentenceGenerator {

   /**
   * Chooses a vowel at random
   * @return a vowel of type String
   */
  def makeA(): String = {
    val vowels: List[String] = List("a", "e", "i", "o", "u")
    
    vowels(Random.nextInt(5))
  }

   /**
   * Chooses a consonant at random
   * @return a consonant of type String
   */
  def selectConsonant(): String = {
    val consonants: List[String] = List("b", "c", "d", "f", "g", "j", 
      "k", "l", "m", "n", "p", "r", "s", "t", "v", "x", "y", "z")
      
      consonants(Random.nextInt(18))
  }  

   /**
   * Chooses a string of two unique consonants at random
   * @return a pair of unique consonants of type String
   */  
  def selectTwoUniqueConsonants(): String = {
    var letters = selectConsonant()
    var temp = ""
    
    do {
      temp = selectConsonant()
    } while (letters.endsWith(temp))
   
    letters + temp
  }

   /**
   * Makes a PRED of the type CCVCV
   * @return a String in the PRED form: CCVCV
   */  
  def makePREDCCVCV(): String = {
    selectTwoUniqueConsonants() + makeA() + selectConsonant() + 
    makeA() 
  }

   /**
   * Makes a PRED of the type CVCCV
   * @return a String in the PRED form: CVCCV
   */  
  def makePREDCVCCV(): String = {
    selectConsonant() + makeA() + selectTwoUniqueConsonants() + 
    makeA()       
  }

   /**
   * Makes a PRED of the type CCVCV or CVCCV
   * @return a String in the PRED form: CCVCV or CVCCV
   */  
  def makePRED(): String = {
    if (Random.nextInt(2) == 0) makePREDCCVCV()
    else makePREDCVCCV()      
  }

   /**
   * Makes a NAME
   * @return a String of random length that ends in a consonant
   */  
  def makeNAME(): String = {
    var name = ""
    var getNextLetter = true

    do {
      //Adds another letter 70% of the time
      getNextLetter = Random.nextInt(10) > 2
      
      //If this is the last letter, appends a consonant to the end of the String
      if (getNextLetter == false) name = name + selectConsonant()
      
      //Appends a consonant to the string 13/18 * 100% of the time
      else if (Random.nextInt(18) > 5) name = name + selectConsonant()
      
      //Appends a vowel otherwise
      else name = name + makeA()
      
    } while(getNextLetter)
      
    name
  }

   /**
   * Selects a random LA string
   * @return a String with a single LA
   */  
  def makeLA(): String = {
    val littleWords: List[String] = List("la", "le", "li", "lo", "lu")
    littleWords(Random.nextInt(5))
  }

   /**
   * Selects a random BA string
   * @return a String with a single BA
   */    
  def makeBA(): String = {
    val littleWords: List[String] = List("ba", "be", "bi", "bo", "bu")
    littleWords(Random.nextInt(5))    
  }

   /**
   * Selects a random DA string
   * @return a String with a single DA
   */    
  def makeDA(): String = {
    val littleWords: List[String] = List("da", "de", "di", "do", "du")
    littleWords(Random.nextInt(5))
  }

   /**
   * Selects a random MOD string (G and a Vowel)
   * @return a String with a single MOD
   */      
  def makeMOD(): String = {
    val littleWords: List[String] = List("ga", "ge", "gi", "go", "gu")
    littleWords(Random.nextInt(5))
  }

   /**
   * Makes a Verbpred: MOD Predstring
   * @return a Verbpred of type String
   */     
  def makeVerbpred(): String = {
    makeMOD() + " " + makePredstring()
  }

   /**
   * Makes a Predstring: Pred or Predstring Pred
   * @return a Predstring of type String
   */     
  def makePredstring(): String = {
    if (Random.nextBoolean) makePRED()
    else makePredstring() + " " + makePRED()
  }

   /**
   * Makes a Predname of the form: 
   * LA Predstring OR Name
   * @return a Predname of type String
   */    
  def makePredname(): String = {
    if (Random.nextBoolean) makeLA() + " " + makePredstring()
    else makeNAME()
  }

   /**
   * Makes a Preds of the form: 
   * Predstring OR Preds Vowel Predstring
   * @return a Preds of type String
   */    
  def makePreds(): String = {
    if (Random.nextBoolean) makePredstring()
    else makePreds() + " " + makeA() + " " + makePredstring
  }

   /**
   * Randomly generates a sentence of the form: 
   * Predname Verbpred Predname OR Predname Verbpred
   * @return a statement of type String
   */    
  def makeStatement(): String = {
    if (Random.nextBoolean) {
      makePredname() + " " + makeVerbpred + " " + makePredname()
    }
    else {
      makePredname() + " " + makeVerbpred
    }
  }
  
   /**
   * Randomly generates a Predclaim in the form:
   * Predname BA Preds OR DA Preds
   * @return a Predclaim of type String
   */    
  def makePredclaim(): String = {
    if (Random.nextBoolean) {
      makePredname() + " " + makeBA() + " " + makePreds()
    }
    else {
      makeDA() + " " + makePreds()
    }
  }

   /**
   * Makes a Sentence: Randomly makes either a Statement or Predclaim
   * @return a sentence of type String
   */    
  def makeSentence(): String = {
    if (Random.nextBoolean) makeStatement()
    else makePredclaim()
  }
}