import scala.util.parsing.combinator._

object BF extends App {
  val raw = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  val program = BFParser.parseAll(BFParser.program, raw).get.toArray
  val vm = new VM(program)
  vm.run
}

sealed abstract class Command
case class Move(n: Int) extends Command
case class Add(n: Int) extends Command
case object Read extends Command
case object Write extends Command
case object LoopIn extends Command
case object LoopOut extends Command

object BFParser extends JavaTokenParsers {
  def add     = "+" ^^ { s => Add(1) }
  def dec     = "-" ^^ { s => Add(-1) }
  def left    = "<" ^^ { s => Move(-1) }
  def right   = ">" ^^ { s => Move(1) }
  def read    = "," ^^ { s => Read }
  def write   = "." ^^ { s => Write }
  def loopin  = "[" ^^ { s => LoopIn }
  def loopout = "]" ^^ { s => LoopOut }

  def operator = (add | dec | left | right | read | write | loopin | loopout)
  def program = operator *
}

class VM(program: Array[_ <: Command]) {
  var mem: Array[Integer] = Array.fill[Integer](100)(0)
  var mp = 0
  var pc = 0

  def run = while (pc < program.size) step(program(pc))

  def findLoop(direction: Int): Int = {
    def go(current: Int, loop: Int): Int = {
      if (loop <= 0) current
      else {
        val next = current + direction
        program(current + direction) match {
          case LoopIn  => go(next, loop + direction)
          case LoopOut => go(next, loop - direction)
          case _         => go(next, loop)
        }
      }
    }

    go(pc + direction, 1)
  }

  def step(command: Command) = {
    command match {
      case Add(n)  => mem(mp) += n
      case Move(n) => mp += n
      case Write   => print(mem(mp).toChar)
      case Read    => mem(mp) = readInt()
      case LoopIn  => if (mem(mp) == 0) pc = findLoop(1)
      case LoopOut => if (mem(mp) != 0) pc = findLoop(-1)
    }

    pc += 1
  }
}
