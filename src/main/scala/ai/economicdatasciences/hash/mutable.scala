package ai.economicdatasciences.dsa.hash

trait HashTable[Key, Value] {
  def insert(myKey: Key, myValue: Value): Unit
  def search(myKey: Key): Option[Value]
  def delete(myKey: Key): Option[Value]
}

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
    myList.find(x => x._1 == myKey).map(y => y._2)
  }

  override def delete(myKey: Key): Option[Value] = {
    val myList = myHashArray(hashCode(myKey))
    myHashArray(hashCode(myKey)) = myList.filter(x => {
      x._1 != myKey
    })
    myList.find(x => x._1 == myKey).map(y => y._2)
  }
}

object HashTableMutableApp {
  def main(args: Array[String]): Unit = {
    val myHashTable: HashTable[Int, String] =
      new HashTableMutableImpl[Int, String](17)

    myHashTable.insert(123456789, "Martin")
    myHashTable.insert(987654321, "James")
    myHashTable.insert(123454321, "Brian")
    myHashTable.insert(432112345, "Albert")
    myHashTable.insert(776612345, "Richie")

    println(s" Martin search, ${myHashTable.search(123456789)}")
    println(s" James search, ${myHashTable.search(987654321)}")
    println(s" Brian search, ${myHashTable.search(123454321)}")
    println(s" Albert search, ${myHashTable.search(432112345)}")
    println(s" Richie search, ${myHashTable.search(776612345)}")

    println(s" Richie Delete, ${myHashTable.delete(776612345)}")
    println(s"Non-existing delete, ${myHashTable.delete(886612345)}")
    println(s"Richie search, ${myHashTable.search(776612345)}")
  }
}
