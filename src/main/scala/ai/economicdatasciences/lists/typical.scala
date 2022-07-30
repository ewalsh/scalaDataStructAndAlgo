package ai.economicdatasciences.dsa.lists

abstract class Employee(
    firstName: String,
    lastName: String,
    department: String,
    salary: Double
)

case class Engineer(
    firstName: String,
    lastName: String,
    department: String,
    salary: Double,
    group: String
) extends Employee(firstName, lastName, department, salary)

case class Doctor(
    firstName: String,
    lastName: String,
    department: String,
    salary: Double,
    group: String
) extends Employee(firstName, lastName, department, salary)

object TypicalListExample {
  val eng1 = Engineer("Isaac", "Newton", "IT", 4500.50, "Engineering")
  val eng2 = Engineer("Albert", "Einstein", "Infra", 4600.50, "Engineering")

  val doc1 = Doctor("Michael","Young","Cardio", 5000.5,"Medicine")
  val doc2 = Doctor("Jeffrey","Hall","Pathology", 5100.5,"Medicine")

  val engineers = List(eng1, eng2)
  val doctors = List(doc1, doc2)

  // flat list
  val employees = engineers ::: doctors
  // list of lists
  val emp2 = List(  engineers, doctors)
}
