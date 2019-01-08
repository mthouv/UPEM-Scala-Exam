package com.fr.upem.partiel.model


sealed trait Category {
  // Could have used a typeclass, I'm too slow
  def show : String
}


case object Salary extends Category {
  override def show: String = "salary"
}

case object Purchase extends Category {
  override def show: String = "purchase"
}

case object CheckDeposit extends Category {
  override def show: String = "check-deposit"
}

case object CheckPayment extends Category {
  override def show: String = "check-payment"
}

case object Withdrawal extends Category {
  override def show: String = "withdrawal"
}

object Category {
  def apply(s : String): Option[Category] = s match {
    case "salary" => Some(Salary)
    case "purchase" => Some(Purchase)
    case "check-deposit" => Some(CheckDeposit)
    case "check-payment" => Some(CheckPayment)
    case "withdrawal" => Some(Withdrawal)
    case _ => None
  }
}