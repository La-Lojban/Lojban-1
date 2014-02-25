package lojban

import org.scalatest._

class SentenceGeneratorTest extends FunSuite{

  test("Recognize A") {
    assert(SentenceRecognizer.isA(SentenceGenerator.makeA))
  }
  
  test("Recognize MOD") {
    assert(SentenceRecognizer.isMOD(SentenceGenerator.makeMOD))
  }
  
  test("Recognize BA") {
    assert(SentenceRecognizer.isBA(SentenceGenerator.makeBA))
  }
  
  test("Recognize DA") {
    assert(SentenceRecognizer.isDA(SentenceGenerator.makeDA))
  }
  
  test("Recognize LA") {
    assert(SentenceRecognizer.isLA(SentenceGenerator.makeLA))
  }
  
  test("Recognize NAME") {
    assert(SentenceRecognizer.isNAME(SentenceGenerator.makeNAME))
  }  
  
  test("Recognize PRED") {
    assert(SentenceRecognizer.isPRED(SentenceGenerator.makePRED))
  }
  
  test("Recognize Verbpred") {
    assert(SentenceRecognizer.isVerbpred(SentenceGenerator.makeVerbpred))
  }    
  
  test("Recognize Statement") {
    assert(SentenceRecognizer.isStatement(SentenceGenerator.makeStatement))
  }   
  
  test("Recognize Predstring") {
    assert(SentenceRecognizer.isPredstring(SentenceGenerator.makePredstring))
  }   
  
  test("Recognize Predname") {
    assert(SentenceRecognizer.isPredname(SentenceGenerator.makePredname))
  }   
  
  test("Recognize Preds") {
    assert(SentenceRecognizer.isPreds(SentenceGenerator.makePreds))
  }   
  
  test("Recognize Predclaim") {
    assert(SentenceRecognizer.isPredclaim(SentenceGenerator.makePredclaim))
  }    
  
  test("Recognize Sentence") {
    assert(SentenceRecognizer.isSentence(SentenceGenerator.makeSentence))  // generator

  }  


}