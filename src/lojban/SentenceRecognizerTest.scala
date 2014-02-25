package lojban

import org.scalatest._

class SentenceRecognizerTest extends FunSuite{

  test("Recognize A") {
    for (word <- List("a", "e", "i", "o", "u")) {
      assert(SentenceRecognizer.isA(word))
    } 
    assert(! SentenceRecognizer.isA("d"))
    assert(! SentenceRecognizer.isA("q"))
    assert(! SentenceRecognizer.isA("ee"))
    assert(! SentenceRecognizer.isA(""))
  }
  
  test("Recognize MOD") {
    for (word <- List("ga", "ge", "gi", "go", "gu")) {
      assert(SentenceRecognizer.isMOD(word))
    } 
    assert(! SentenceRecognizer.isMOD("gd"))
    assert(! SentenceRecognizer.isMOD("ba"))
    assert(! SentenceRecognizer.isMOD("gat"))
    assert(! SentenceRecognizer.isMOD(""))
  }
  
  test("Recognize BA") {
    for (word <- List("ba", "be", "bi", "bo", "bu")) {
      assert(SentenceRecognizer.isBA(word))
    } 
    assert(! SentenceRecognizer.isBA("bd"))
    assert(! SentenceRecognizer.isBA("ga"))
    assert(! SentenceRecognizer.isBA("bat"))
    assert(! SentenceRecognizer.isBA(""))
  }
  
  test("Recognize DA") {
    for (word <- List("da", "de", "di", "do", "du")) {
      assert(SentenceRecognizer.isDA(word))
    } 
    assert(! SentenceRecognizer.isDA("dd"))
    assert(! SentenceRecognizer.isDA("ba"))
    assert(! SentenceRecognizer.isDA("dat"))
    assert(! SentenceRecognizer.isDA(""))
  }
  
  test("Recognize LA") {
    for (word <- List("la", "le", "li", "lo", "lu")) {
      assert(SentenceRecognizer.isLA(word))
    } 
    assert(! SentenceRecognizer.isLA("ld"))
    assert(! SentenceRecognizer.isLA("da"))
    assert(! SentenceRecognizer.isLA("lat"))
    assert(! SentenceRecognizer.isLA(""))
  }
  
  test("Recognize NAME") {
    for (word <- List("abc", "dddd", "g", "jklmnoprs", "tuvxyz")) {
      assert(SentenceRecognizer.isNAME(word))
    } 
    assert(! SentenceRecognizer.isNAME("h"))
    assert(! SentenceRecognizer.isNAME("q"))
    assert(! SentenceRecognizer.isNAME("w"))
    assert(! SentenceRecognizer.isNAME("a"))
    assert(! SentenceRecognizer.isNAME("e"))
    assert(! SentenceRecognizer.isNAME("i"))
    assert(! SentenceRecognizer.isNAME("o"))
    assert(! SentenceRecognizer.isNAME("u"))
    assert(! SentenceRecognizer.isNAME("abcdh"))
    assert(! SentenceRecognizer.isNAME("wsz"))
    assert(! SentenceRecognizer.isNAME(""))
  }  
  
  test("Recognize PRED") {
    for (word <- List("cdece", "gbega", "vevlu")) {
      assert(SentenceRecognizer.isPRED(word))
    } 
    assert(! SentenceRecognizer.isPRED("ccece"))
    assert(! SentenceRecognizer.isPRED("cecce"))
    assert(! SentenceRecognizer.isPRED("cdfece"))
    assert(! SentenceRecognizer.isPRED("cedfaa"))
    assert(! SentenceRecognizer.isPRED("cdfe"))
    assert(! SentenceRecognizer.isPRED(""))
  }
  
  test("Recognize Verbpred") {
    for (word <- List("ga cdece", "ga cdece cdece", "ge dedce", "gu dedce cdece")) {
      assert(SentenceRecognizer.isVerbpred(word))
    } 
    assert(! SentenceRecognizer.isVerbpred("la cdece"))
    assert(! SentenceRecognizer.isVerbpred("ga ga cdece"))
    assert(! SentenceRecognizer.isVerbpred("ga ccece"))
    assert(! SentenceRecognizer.isVerbpred("go cdopuu"))
    assert(! SentenceRecognizer.isVerbpred("cdece"))
    assert(! SentenceRecognizer.isVerbpred(""))
  }    
  
  test("Recognize Statement") {
    assert(SentenceRecognizer.isStatement("la cdece cdece ga cdeca"))
    assert(SentenceRecognizer.isStatement("la cdece cdece ga cdeca cdeca la cdece cdece"))
    assert(SentenceRecognizer.isStatement("abcdef ge cdece cdeca"))
    assert(SentenceRecognizer.isStatement("abcdef ge cdece cdeca abcdef"))    
    assert(SentenceRecognizer.isStatement(SentenceGenerator.makeStatement))
    assert(! SentenceRecognizer.isStatement("ga cdeca la cdece"))
    assert(! SentenceRecognizer.isStatement("ga cdeca abcdef"))
    assert(! SentenceRecognizer.isStatement("abcdef ga cdece abcdef ga cdece"))
    assert(! SentenceRecognizer.isStatement("abcdef"))
    assert(! SentenceRecognizer.isStatement("la cdece"))
    assert(! SentenceRecognizer.isStatement(""))
  }   
  
  test("Recognize Predstring") {
    assert(SentenceRecognizer.isPredstring("cdeca"))
    assert(SentenceRecognizer.isPredstring("cicdo"))
    assert(SentenceRecognizer.isPredstring("cucdu cdace cucdu"))
    assert(SentenceRecognizer.isPredstring(SentenceGenerator.makePredstring))
    assert(! SentenceRecognizer.isPredstring("ccece"))
    assert(! SentenceRecognizer.isPredstring("cecce"))
    assert(! SentenceRecognizer.isPredstring("cdececdece"))
    assert(! SentenceRecognizer.isPredstring("cecec"))
    assert(! SentenceRecognizer.isPredstring(""))
  }   
  
  test("Recognize Predname") {
    assert(SentenceRecognizer.isPredname("la cdece"))
    assert(SentenceRecognizer.isPredname("lo cdece cecde cdece cecde"))
    assert(SentenceRecognizer.isPredname("abcdefg"))
    assert(SentenceRecognizer.isPredname("g"))   
    assert(SentenceRecognizer.isPredname("gggggggggggggggggggggggggggg"))
    assert(SentenceRecognizer.isPredname(SentenceGenerator.makePredname))
    assert(! SentenceRecognizer.isPredname("aeiou"))
    assert(! SentenceRecognizer.isPredname("ga cdece"))
    assert(! SentenceRecognizer.isPredname("la ccece"))
    assert(! SentenceRecognizer.isPredname("la la cdece"))
    assert(! SentenceRecognizer.isPredname(""))
  }   
  
  test("Recognize Preds") {
    assert(SentenceRecognizer.isPreds("cdece"))
    assert(SentenceRecognizer.isPreds("cdece cecde cdece cecde"))
    assert(SentenceRecognizer.isPreds("cdece cecde i cdece cecde"))
    assert(SentenceRecognizer.isPreds("cdece cecde a cdece o cdece cdece"))   
    assert(SentenceRecognizer.isPreds("cdece a cdece cdece cdece o cdece i cdece cdece"))
    assert(SentenceRecognizer.isPreds(SentenceGenerator.makePreds))
    assert(! SentenceRecognizer.isPreds("a"))
    assert(! SentenceRecognizer.isPreds("a cdece"))
    assert(! SentenceRecognizer.isPreds("cdece a"))
    assert(! SentenceRecognizer.isPreds("cdece a e cdece"))
    assert(! SentenceRecognizer.isPreds(""))
  }   
  
  test("Recognize Predclaim") {
    assert(SentenceRecognizer.isPredclaim("la cdece cdece ba cdece cdece"))
    assert(SentenceRecognizer.isPredclaim("la cdece cdece ba cdece cdece a cdece"))
    assert(SentenceRecognizer.isPredclaim("abcdef ba cdece e cdece cdece"))
    assert(SentenceRecognizer.isPredclaim("da cdece cdece"))   
    assert(SentenceRecognizer.isPredclaim("da cdece cdece a cdece a cdece cdece"))
    assert(SentenceRecognizer.isPredclaim(SentenceGenerator.makePredclaim))
    assert(! SentenceRecognizer.isPredclaim("da"))
    assert(! SentenceRecognizer.isPredclaim("abcdef la cdece ba cdece cdece"))
    assert(! SentenceRecognizer.isPredclaim("la cdece cdece a cdece"))
    assert(! SentenceRecognizer.isPredclaim("la cdece cdece"))
    assert(! SentenceRecognizer.isPredclaim("ba cdece cdece"))
    assert(! SentenceRecognizer.isPredclaim(""))
  }    
  
  test("Recognize Sentence") {
    assert(SentenceRecognizer.isSentence("la cdece cdece ga cdeca"))  // statement
    assert(SentenceRecognizer.isSentence("la cdece cdece ga cdeca cdeca la cdece cdece."))  // statement
    assert(SentenceRecognizer.isSentence("la cdece cdece ba cdece cdece"))  // predclaim
    assert(SentenceRecognizer.isSentence("la cdece cdece ba cdece cdece a cdece."))  // predclaim
    assert(SentenceRecognizer.isSentence("da cdece cdece"))  // predclaim
    assert(SentenceRecognizer.isSentence(SentenceGenerator.makeSentence))  // generator
    assert(! SentenceRecognizer.isSentence("cdece cdece a cdece"))  // Preds
    assert(! SentenceRecognizer.isSentence("la cdece cdece"))  // Predname
    assert(! SentenceRecognizer.isSentence("cdece cdece cdece"))  // Predstring
    assert(! SentenceRecognizer.isSentence("ga cdece cdece"))  // Verbpred
    assert(! SentenceRecognizer.isSentence("a"))
    assert(! SentenceRecognizer.isSentence(""))
  }  


}