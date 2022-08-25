trait Subscriber:
    def handler(pub: Publisher): Unit

trait Publisher:
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscirber: Subscriber): Unit =
        subscribers += subscirber

    def unsubscribe(subscirber: Subscriber): Unit =
        subscribers -= subscirber

    def publish(): Unit =
        subscribers.foreach(_.handler(this))
end Publisher

class BankAccount extends Publisher:
    private var balance = 0

    def currentBalance: Int = balance

    def deposit(amount: Int): Unit =
        if amount > 0 then balance += amount
        publish()

    def withdraw(amount: Int): Unit =
        if 0 < amount && amount <= balance then 
            balance -= amount
            publish()
        else throw Error("insufficient funds")
end BankAccount

class Consolidator(observed: List[BankAccount]) extends Subscriber:
    observed.foreach(_.subscribe(this))

    private var total: Int = _
    compute()

    private def compute() =
        total = observed.map(_.currentBalance).sum
    
    def handler(pub: Publisher) = compute()
    def totalBalance = total
end Consolidator
