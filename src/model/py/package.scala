package model

package object models {
  type Word = String
  type Entity = String
  
  class Model(entities: Map[Word, Entity], relations1: Map[Word, Entity => Boolean], relations2: Map[Word, Entity => Entity => Boolean]) {
    
    val F_e: Word => Entity = (w: Word) => this.entities(w)
    val F_1: Word => Entity => Boolean = (w: Word) => this.relations1(w)
    val F_2: Word => Entity => Entity => Boolean = (w: Word) => this.relations2(w)
    
    def Sem1(n: Word, vi: Word): Boolean = F_1(vi)(F_e(n))
    def Sem2(n1: Word, vt: Word, n2: Word): Boolean = F_2(vt)(F_e(n1))(F_e(n2))
  }
  
  def tests = {
    val textbook_entities = Map("Anwar" -> "AS", "Mohammed" -> "MA", "Noam" -> "NC", "John" -> "JW")
    val is_bald = Map("AS" -> false, "MA" -> true, "NC" -> false, "JW" -> true)
    
    val does_love: Entity => Entity => Boolean = (e1: Entity) => (e2: Entity) => List(e1,e2) match {
      case List(e1, e2)  if e1 == e2 => true
      case _ => false
    }
    val textbook_r1 = Map("bald" -> is_bald)
    val textbook_r2 = Map("loves" -> does_love)
    lazy val textbook_model = new Model(textbook_entities, textbook_r1, textbook_r2)
    
    println(is_bald("JW"))
    println(does_love("NC")("NC"))
    println(does_love("MA")("JW"))
    println(textbook_model.Sem1("John","bald"))
    println(textbook_model.Sem2("Noam","loves","Noam"))
    println(textbook_model.Sem2("Mohammed","loves","John"))
  }
}

