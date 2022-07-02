package ai.economicdatasciences.dsa.hash

class HashTableMutableImpl[Key, Value](size: Int)
    extends HashTable[Key, Value] {
  private val myHashArray = Array.fill(size)(List[(Key, Value)]())

  def hashCode[Key](myKey: Key) = {
    val tempHashCode = myKey.## % size
    if (tempHashCode < 0) {
      tempHashCode + size
    } else {
      tempHashCode
    }
  }

  override def insert(myKey: Key, myValue: Value): Unit = {
    val myList = myHashArray(hashCode(myKey))
    myHashArray(hashCode(myKey)) =
      (myKey, myValue) +: myList.filter(x => x._1 != myKey)
  }

  override def search(myKey: Key): Option[Value] = {
    val myList = myHashArray(hashCode(myKey))
    myList.find(x => x._1 == myKey).map(Y => y._2)
  }

  override def delete(myKey: Key): Option[Value] = {
    val myList = myHashArray(hashCode(myKey))
    myHashArray(hashCode(myKey)) = myList.filter(x => {
      x._1 != myKey
    })
    myList.find(x => x._1 == myKey).map(y => y._2)
  }
}
