package com.fr.upem.partiel.model


case class TransactionId(id : Int) {
  def show : String = id.toString
}

/*
I'm defining date as a String because i ran into problems while trying to import transactions from file
Could not find a way to convert a String to an Instant
*/
case class Transaction(id : TransactionId, amount : Int, date : String)


case class CategorizedTransaction(transaction: Transaction, category: Option[Category]) {

  def show : String = {
    val categoryString = category match {
      case Some(c) => c.show
      case _ => ""
    }
    transaction.id.show + "," + categoryString + "," + transaction.amount.toString + "," + transaction.date
  }
}
