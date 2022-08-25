import javax.swing.text.Position
/**
 *  RP is about reacting to seq of events that happen in time
 *  e.g.: aggregate an event sequence into a signal.
 *      > a signal is a value that changes over time
 *      > it is represented as a function from time to value domain
 *      > instead of propagating updates to mutable states, we define new
 *        signals in terms of existing ones.
*/

/**
 * Two fundamental operations over signals
 *      1. Get the value of a signal at the current time.
 *             For example, for us now by () application.
 *      2. Define a signal in terms of other signals.
 *             For example, for us now by the Signal constructor. 
*/

class Position:
    def <= (x: Position): Boolean = ???

class Signal[T](value: => T):
    def apply = value

def mousePosition(): Position = ???

def inRectangle (LL: Position, UR: Position): Signal[Boolean] =
    Signal {
        val pos = mousePosition()
        LL <= pos && pos <= UR
    }

// Scala syntax for a method 'update':
// 
// intArray(i) = 8 is the same as intArray.update(i, 8)
// f(A,B,C,...) = Z is the same as f.update(A,B,C,...,Z) therefore
// someObject.update(someValue) is the same as someObject() = someValue
