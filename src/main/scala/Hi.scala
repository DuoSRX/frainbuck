import scala.util.parsing.combinator.RegexParsers

object BF extends App {
  val raw = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  val program = BFParser.parseAll(BFParser.program, raw).get
  println(program)
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
  def add   = """\++""".r ^^ { s => Add(s.length) }
  def dec   = """-+""".r  ^^ { s => Add(-s.length) }
  def left  = """<+""".r  ^^ { s => Move(-s.length) }
  def right = """>+""".r  ^^ { s => Move(s.length) }
  def read    = "," ^^ { s => Read() }
  def write   = "." ^^ { s => Write() }
  def operator = add | dec | left | right | read | write
  def loop    = "[" ~> (operator *) <~ "]" ^^ { l => Loop(l) }
  def program = (operator | loop) *
}

class VM(program: List[Command]) {
  var mem: Array[Integer] = Array.fill[Integer](100)(0)
  var mp = 0

  def run: Unit = run(optimize(program))

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

  private def optimize(program: List[Command]): List[Command] = program match {
    case Add(x)::Add(y)::xs   => optimize(Add(x+y)::xs)
    case Move(x)::Move(y)::xs => optimize(Move(x+y)::xs)
    case Nil => Nil
    case x::xs => x::optimize(xs)
  }
}
