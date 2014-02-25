/**
 * Converes with a user in Lojban
 * @authors Mark Mercurio and Yupeng Lu
 * @version October 3, 2013
 */
package lojban

/**
 * Controls the flow of conversation between user and computer
 */
object Lojban { 
  def main(args: Array[String]): Unit = {

    var keepConversing = true
    
    println("coi.")
        
    while(keepConversing) {
      val userInput = readLine()
      if (userInput == "co'o." || userInput == "co'o") {
        keepConversing = false
      }
      else if (SentenceRecognizer.isSentence(userInput)) {        
        println(SentenceGenerator.makeSentence)
      }
      else println("i mi na jimpe.")
    }
  }

}