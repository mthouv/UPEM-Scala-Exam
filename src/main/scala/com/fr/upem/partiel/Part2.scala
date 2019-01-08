package com.fr.upem.partiel

import java.time.Instant

import com.fr.upem.partiel.model._

// Part2 (10pts)
/**
  *
  * The goal is to create a system that allows users to handle their personal finances.
  * Each user has an account with his UNIQUELY IDENTIFIED transactions.
  * Each transaction can be categorized with categories such as: Salary, Purchase, Withdrawal, Checks (deposits and payments) etc.
  *
  */
object Part2 extends App {

  // 2.1 Modelling.
  // Create a model for the user's bank account
  // Create a model for a transaction (has an amount and a date)
  // Create a model for the following categories [salary, purchase, check deposit, check payment, withdrawal]

  // 2.2 Create api
  // Create an api that allows for:
  // - Adding a transaction to a bank account
  // - Adding a transaction to a bank account with it's category
  // - Categorizing or recategorizing an existing transaction
  //
  // help: The bank account must save, for each transaction id, the transaction and it's eventual category
  // This could be achieved through a structure of this kind:
  // BankAccount(transactions: Map[TransactionId, CategorizedTransaction])

  // 2.3 Use the api that you just created.
  // - Create an empty account
  // - Add a transaction with id 1 of amount -13 (and any date)
  // - Add a transaction with id 2 of amount -50 (and any date)
  // - Add a check payment with id 3 of amount 650 (and any date)
  // - Categorize the second transaction (id "2") as a withdrawal
  // - (Re)categorize the third transaction (id "3") as check deposit
  //
  // help: After the above operations the bank account should hold:
  // TransactionId(1) -> (Transaction(1, -13, date), None)
  // TransactionId(2) -> (Transaction(2, -50, date), Some(Withdrawal))
  // TransactionId(3) -> (Transaction(3, 650, date), Some(CheckDeposit)

  val account = BankAccount(Map[TransactionId, CategorizedTransaction]())
  val t1 = Transaction(TransactionId(1), -13, "1111111")
  val t2 = Transaction(TransactionId(2), -50, "1111111")
  val t3 = Transaction(TransactionId(3), 650, "1111111")


  val updatedAccount =
    account.addTransaction(t1).addTransaction(t2).addTransaction(t3)
      .categorizeTransaction(TransactionId(2), Withdrawal).categorizeTransaction(TransactionId(3), CheckDeposit)

  // 2.4 CSV Export
  // Users want to be able to export their accounts in CSV (Comma-Separated Values) format.
  // A line is structured as follows: Id, Type, Amount, Date
  // Allow exporting a bank account as a CSV (no need to write a file, just write a String).
  // Amounts do not need to be formatted, write dates in any valid format (timestamp, ISO-8601 ...)
  //
  // Example output:
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770

  val export = updatedAccount.show
  println(export)

  // 2.5 CSV Import
  // Users want to be able to import transactions from a CSV.
  // Write code to parse and validate csv files
  // Validation: The input data should be validated
  //
  // Example valid input
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770
  //
  // Example invalid input
  // 1,invalid type,invalid amount,invalid date

  // 2.6 Extend the api data analysis features
  // It should allow for:
  // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
  // - List all check (deposit and payment) operations
  // - Compute the account balance
  val income = updatedAccount.computeIncomes
  val allChecks = updatedAccount.getAllCheckOperations
  val balance = updatedAccount.computeAccountBalance

}
