import scala.util.parsing.combinator.RegexParsers

object BF extends App {
  val raw = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  val program = BFParser.parseAll(BFParser.program, raw).get
  // println(program)
  val vm = new VM(program)
  vm.run
}

trait Command
case class Move(n: Int) extends Command
case class Add(n: Int) extends Command
case class Read() extends Command
case class Write() extends Command
case class Loop(commands: List[Command]) extends Command

object BFParser extends RegexParsers {
  def ignore = """[^<>+-\.,\[\]]""".r
  def add     = "+" ^^ { s => Add(1) }
  def dec     = "-" ^^ { s => Add(-1) }
  def left    = "<" ^^ { s => Move(-1) }
  def right   = ">" ^^ { s => Move(1) }
  def read    = "," ^^ { s => Read() }
  def write   = "." ^^ { s => Write() }

  def operator = (add | dec | left | right | read | write)

  def loop    = "[" ~> (operator *) <~ "]" ^^ { l => Loop(l) }
  def program = (operator | loop) *
}

class VM(program: List[Command]) {
  var mem: Array[Integer] = Array.fill[Integer](100)(0)
  var mp = 0

  def run: Unit = run(program)

  private def run(commands: List[Command]): Unit = {
    commands.map(step)
  }

  private def step(command: Command) = command match {
    case Add(n)  => mem(mp) += n
    case Move(n) => mp += n
    case Write() => print(mem(mp).toChar)
    case Read()  => mem(mp) = readInt()
    case Loop(l) => do { run(l) } while (mem(mp) != 0)
  }
}
