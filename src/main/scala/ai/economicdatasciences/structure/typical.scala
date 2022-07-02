package ai.economicdatasciences.dsa.structure

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
