trait Signal[+T]:
    def apply(): T = ???

object Signal:
    opaque type Observer = AbstractSignal[?]
    def caller: Observer = ???

    abstract class AbstractSignal[+T] extends Signal[T]:
        private var currentValue: T = _
        private var observers: Set[Observer] = Set()

        override def apply(): T =
            observers += caller
            currentValue

        protected def eval: () => T

    def apply[T](expr: => T) = ???

    class Var[T](expr: => T) extends Signal[T]:
        def update(expr: => T): Unit = ???
