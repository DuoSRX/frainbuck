object BF extends App {
  val raw = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.".toCharArray()
  val program = raw.map(Parser.charToCommand)
  val vm = new VM(program)
  vm.run
}

sealed abstract class Command
case object Left extends Command
case object Right extends Command
case object Add extends Command
case object Sub extends Command
case object Read extends Command
case object Write extends Command
case object LoopIn extends Command
case object LoopOut extends Command

object Parser {
  def charToCommand(c: Char): Command = c match {
    case '<' => Left
    case '>' => Right
    case '+' => Add
    case '-' => Sub
    case ',' => Read
    case '.' => Write
    case '[' => LoopIn
    case ']' => LoopOut
  }
}

class VM(program: Array[Command]) {
  var mem: Array[Integer] = Array.fill[Integer](100)(0)
  var mp = 0
  var pc = 0

  def run = while (pc < program.size) step(program(pc))

  def findLoop(direction: Int): Int = {
    def go(current: Int, loop: Int): Int = {
      if (loop <= 0) current
      else {
        next = current + direction
        program(current + direction) match {
          case LoopIn  => go(next, loop + direction)
          case LoopOut => go(next, loop - direction)
          case _       => go(next, loop)
        }
      }
    }

    go(pc + direction, 1)
  }

  def step(command: Command) {
    command match {
      case Add     => mem(mp) += 1
      case Sub     => mem(mp) -= 1
      case Left    => mp -= 1
      case Right   => mp += 1
      case Write   => print(mem(mp).toChar)
      case Read    => mem(mp) = readInt()
      case LoopIn  => if (mem(mp) == 0) pc = findLoop(1)
      case LoopOut => if (mem(mp) != 0) pc = findLoop(-1)
    }

    pc += 1
  }
}
