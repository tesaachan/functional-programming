def iterate(n: Int, f: Int => Int, x: Int): Int =
    if n == 0 then x else iterate(n - 1, f, f(x))

def whileDo(condition: => Boolean)(command: => Unit): Unit =
    if condition then
        command
        whileDo(condition)(command)
    else ()

class Repeat(command: => Unit):
    def until(condition: => Boolean): Unit = 
        if condition then
            command
            Repeat(command) until (condition)
        else ()

def repeat(command: => Unit) = Repeat(command)

var count = 0
whileDo (count != 5) {
    println(count)
    count += 1
}

count = 0
repeat {
    println(count)
    count += 1
} until (count != 5)
            