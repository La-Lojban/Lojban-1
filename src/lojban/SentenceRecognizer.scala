package lojban


object SentenceRecognizer {

  def main(args: Array[String]): Unit = {}
  /**
   * Generate a list of vowel and a list of consonant
   */
  val vowel: List[Char] = List('a', 'e', 'i', 'o', 'u')
  val letter: List[Char] = (for (i <- 97 to 122) yield i.toChar).toList
  val consonant: List[Char] = letter.filterNot(
    vowel.contains(_) == true
    ).filterNot(
    _ == 'h'
    ).filterNot(
    _ == 'q'
    ).filterNot(
    _ == 'w'
    )
  
  /**
   * @return a string without period
   */  
  def dropPeriod(n: String): String = {
    if (n.endsWith(".")) n.dropRight(1)
    else n
  }
  
  /**
   * Test whether giver character is vowel or consonant
   * @return "C" if consonant, "V" if consonant
   */    
  def consonantOrVowel(n: Char): String = {
    if (vowel.contains(n)) {
      "V"
    } else if (consonant.contains(n)) {
      "C"
    } else {
      println ("error")
      "not vowel or consonant"
    }
  }
  
  /**
   * Given a string
   * @return a boolean shows whether a PRED or not
   */  
  def isPRED(n: String): Boolean = {
      var pred = ""
      for (i <- n) {
        pred += consonantOrVowel(i)
      }
      if (pred == "CCVCV" || pred == "CVCCV") {
        n.apply(0) != n.apply(1) && n.apply(2) != n.apply(3)
        }
      else false
  }

  /**
   * Given a string
   * @return a boolean shows whether a NAME or not
   */    
  def isNAME(n: String): Boolean = { 
    var isName = true
    var count = 0
    while (isName && count < n.length) {
      if (!(consonant.contains(n(count)) || vowel.contains(n(count)))) isName = false
      count += 1
    }
    if (n.isEmpty) false
    else consonant.contains(n.last) && isName
  }
  
  /**
   * Given a string
   * @return a boolean shows whether a A or not
   */    
  def isA(n: String): Boolean = {
    if (n.isEmpty) false
    else n.length == 1 && vowel.contains(n.head)
  }
  
  /**
   * Given a string
   * @return a boolean shows whether a MOD or not
   */   
  def isMOD(n: String): Boolean = {
    if (n.isEmpty) false
    else n.head == 'g' && n.length == 2 && isA(n.last.toString)
  }

  /**
   * Given a string
   * @return a boolean shows whether a BA or not
   */   
  def isBA(n: String): Boolean = {
    if (n.isEmpty) false
    else n.head == 'b' && n.length == 2 && isA(n.last.toString)
  }
  
  /**
   * Given a string
   * @return a boolean shows whether a DA or not
   */   
  def isDA(n: String): Boolean = {
    if (n.isEmpty) false
    else n.head == 'd' && n.length == 2 && isA(n.last.toString)
  }  
  
  /**
   * Given a string
   * @return a boolean shows whether a LA or not
   */   
  def isLA(n: String): Boolean = {
    if (n.isEmpty) false
    else n.head == 'l' && n.length == 2 && isA(n.last.toString)
  }
  
  /**
   * Given a string
   * @return a boolean shows whether Predstring  or not
   */   
  def isPredstring(n: String): Boolean = {
    var isPredstring = false
    if (n.length != 0) {
      isPredstring = true
      var text = n.split(' ')
      for (i <- 0 to text.length - 1) {
        if (!isPRED(text(i))) isPredstring = false
      }
    }
    isPredstring
  }

  /**
   * Given a string
   * @return a boolean shows whether a Verbpred or not
   */   
  def isVerbpred(n: String): Boolean = {
    var text = n.split(' ')
    isMOD(text(0)) == true && isPredstring(text.tail.mkString(" "))
  }

  /**
   * Given a string
   * @return a boolean shows whether a A or not
   */ 
  def isPredname(n: String): Boolean = {
      var text = n.split(' ')      
      (isLA(text(0)) && isPredstring(text.tail.mkString(" "))) || isNAME(n)
  }

  /**
   * Given a string
   * @return a boolean shows whether a Statement or not
   */ 
  def isStatement(n: String): Boolean = {
    var text: Array[String] = n.trim.split("\\W+")
    var positionOne = 1
    var positionTwo = 1
    var isStatement = false
    var done = false
    
    while (positionOne < text.length && !done) {
      if (isPredname(text.take(positionOne).mkString(" ")) &&
          isMOD(text(positionOne))) { 
            text = text.drop(positionOne)
            while (positionTwo <= text.length && !done) {           
              if (isVerbpred(text.take(positionTwo).mkString(" ")) &&
                  (isPredname(text.drop(positionTwo).mkString(" ")) ||
                    text.drop(positionTwo).isEmpty)) {               
                isStatement = true
              } 
              positionTwo += 1
            }
            done = true   
      }
      positionOne += 1
    }
    isStatement
  }

  /**
   * Given a string
   * @return a boolean shows whether a Preds or not
   */   
  def isPreds(n: String): Boolean = {
    var textSplit = n.split("\\W+")
    var i = 0
    var isPreds = true
    if (isPRED(textSplit.head) && isPRED(textSplit.last)) {
      while (i < textSplit.length && isPreds) {
        
        if (isPredstring(textSplit(i)) == false) {
          if (isA(textSplit(i)) && isPredstring(textSplit(i + 1))) {
            textSplit = textSplit.drop(i + 1)
            i = 0
          } else {
            isPreds = false
          }
        } else {
          i = i + 1
        }
      }
    } else {
      isPreds = false
    }
    isPreds
  }  

  /**
   * Given a string
   * @return a boolean shows whether a DAPreds or not
   */   
  def isDAPreds(n: Array[String]): Boolean = {
    val textSplit = n
    isDA(textSplit.head) && isPreds(textSplit.tail.mkString(" "))
  }

  /**
   * Given a string
   * @return a boolean shows whether a BAPreds or not
   */   
  def isPrednameBAPreds(n: Array[String]): Boolean = {
    var textSplit = n
    var predFound = false
    var s = ""
    var BAfound = false

    if (isPredname(textSplit.take(2).mkString(" "))) {
      predFound = true
      s = textSplit.take(2).mkString(" ")
      textSplit = textSplit.drop(2)
    } else if (isPredname(textSplit(0))) {
      predFound = true
      s = textSplit.head
      textSplit = textSplit.tail
    }

    if (predFound) {
      var stillPred = true

      for (i <- 1 to textSplit.length if stillPred) {
        s = s + " " + textSplit.head
        if (!isPredname(s.trim)) {
          stillPred = false
          BAfound = isBA(textSplit.head)
        } else textSplit = textSplit.tail
      }
    }
    predFound && BAfound && isPreds(textSplit.tail.mkString(" "))
  }

  /**
   * Given a string
   * @return a boolean shows whether a Predclaim or not
   */   
  def isPredclaim(n: String): Boolean = {
    val textSplit = n.trim.split(" ")
    
    if (textSplit.length < 2) false
    else (isPrednameBAPreds(textSplit) || isDAPreds(textSplit))
  }
  
  /**
   * Given a string
   * @return a boolean shows whether a Sentence or not
   */     
  def isSentence(n: String): Boolean = {
    val s = dropPeriod(n)
    if (isStatement(s) || isPredclaim(s)) true
    else false
  } 
 
}

