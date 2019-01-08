package com.fr.upem.partiel.model

import scala.io.Source
import scala.util.{Failure, Success, Try}

case class BankAccount(transactions : Map[TransactionId, CategorizedTransaction]) {

  // 2.2 Create api
  // Create an api that allows for:
  // - Adding a transaction to a bank account
  // - Adding a transaction to a bank account with it's category
  // - Categorizing or recategorizing an existing transaction

  def addTransaction(t : Transaction) : BankAccount = {
    // Check if transaction Id is unique
    val transa = transactions.get(t.id)
    transa match {
      case Some(_) => this
      case _       => BankAccount(transactions + (t.id -> CategorizedTransaction(t, None)))
    }
  }

  def addCategorizedTransaction(t : CategorizedTransaction) : BankAccount = {
    // Check if transaction Id is unique
    val transa = transactions.get(t.transaction.id)
    transa match {
      case Some(_) => this
      case _       => BankAccount(transactions + (t.transaction.id -> t))
    }
  }

  def categorizeTransaction(id : TransactionId, c : Category): BankAccount = {
    val transaction = transactions.get(id)

    transaction match {
      case Some(t) =>
        val newTransaction = t.copy(t.transaction, Some(c))
        BankAccount(transactions + (t.transaction.id -> newTransaction))
      case None => this
    }
  }



  // 2.4 CSV Export
  // Could have used a typeclass, I'm too slow
  def show : String = {
    transactions.values.toList.map(t => t.show).mkString("\n")
  }



  // 2.6 Extend the api data analysis features
  // It should allow for:
  // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
  // - List all check (deposit and payment) operations
  // - Compute the account balance

  def computeIncomes : Int = {
    transactions.values.filter(t => t.category match {
      case Some(Salary) => true
      case Some(CheckDeposit) => true
      case None if t.transaction.amount > 0 => true
      case _ => false
    }).map(t => t.transaction.amount).sum
  }

  def getAllCheckOperations : List[CategorizedTransaction] = {
    transactions.values.filter(t => t.category match {
      case Some(CheckDeposit) => true
      case Some(CheckPayment) => true
      case _ => false
    }).toList
  }


  def computeAccountBalance : Int = {
    transactions.values.map(t => t.transaction.amount).sum
  }
}




// 2.5 CSV Import
// Users want to be able to import transactions from a CSV.
// Write code to parse and validate csv files
// Validation: The input data should be validated

object importBankAccount {


  def createTransactionFromList(l : List[String]) : Option[CategorizedTransaction] = {
    val category = Try(Category(l(1))) match {
      case Success(c) => Right(c)
      case Failure(_) => Left(None)
    }

    // I convert the date to an Int then back to a String to make sure it is a numerical string, not pretty
    category match {
      case Right(c) => Try(CategorizedTransaction(Transaction(TransactionId(l(0).toInt), l(2).toInt, l(3).toInt.toString), c)).toOption
      case Left(_) => None
    }
  }


  def createAccountFromLists(l : List[List[String]], acc : BankAccount): BankAccount = l match {
    case x :: xs =>
      val transa = createTransactionFromList(x)
      transa match {
        case Some(t) => createAccountFromLists(xs, acc.addCategorizedTransaction(t))
        case None => createAccountFromLists(xs, acc)
      }
    case Nil => acc
  }


  def createAccountFromFile(path : String): BankAccount = {
    val readFile = Try(Source.fromFile(path))

    val lines = readFile match {
      case Success(f) => f.getLines().toList
      case Failure(_) => List()
    }

    createAccountFromLists(lines.map(l => l.split(",").toList), BankAccount(Map()))
  }


}

