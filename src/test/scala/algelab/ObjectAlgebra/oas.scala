package algelab.ObjectAlgebra

/**
  * Created by dpm on 10/12/16.
  */
import scala.io.StdIn.readLine
import org.scalatest._

object OAS extends OAS

class OAS extends FlatSpec with Matchers{

  // IO algebras

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]
  }

  object IO{

    def apply[P[_]](implicit IO: IO[P]) = IO

    object Syntax{
      def read[P[_]]()(implicit IO: IO[P]): P[String] =
        IO.read()

      def write[P[_]](msg: String)(implicit IO: IO[P]): P[Unit] =
        IO.write(msg)
    }

  }

  // Particular Monadic IO Program

  object SingleEffectProgram{
    import scalaz.Monad
    import IO.Syntax._, scalaz.syntax.monad._

    def echo[P[_]: IO: Monad](): P[String] = for{
      msg <- read()
      _ <- write(msg)
    } yield msg
  }

  // Console-based interpretation of IO Programs

  import scalaz.Id, Id.Id

  object Console{
    implicit object ConsoleIO extends IO[Id]{
      def read() = readLine()
      def write(msg: String) = println(msg)
    }
  }

  // State-based interpretation of IO Programs

  import scalaz.State

  case class IOState(in: List[String], out: List[String])

  type IOStateAction[T]=State[IOState,T]

  object IOStateAction{
    implicit object IOStateActionIO extends IO[IOStateAction]{
      import scalaz.syntax.monad._

      def read() = State.get[IOState] >>= {
        case IOState(msg::reads,written) =>
          State.put(IOState(reads,written)) >| msg
      }

      def write(msg: String) = State.get[IOState] >>= {
        case IOState(reads, written) =>
          State.put(IOState(reads, msg::written))
      }
    }
  }

  // Echo interpretations

  object SingleEffectInterpretations{
    import SingleEffectProgram._
    import scalaz.Monad

    def consoleEcho(): String =
      echo()(Console.ConsoleIO, Monad[Id])

    import IOStateAction._
    def stateEcho(): IOStateAction[String] =
      echo[IOStateAction]()
  }

  "State-based echo" should "work" in {
    import SingleEffectInterpretations._

    stateEcho().eval(IOState(List("hi"),List())) shouldBe
      "hi"

    stateEcho().exec(IOState(List("hi"),List())) shouldBe
      IOState(List(),List("hi"))
  }

  // Interpretations over IOState

  sealed abstract class Level
  case object WARNING extends Level
  case object DEBUG extends Level
  case object INFO extends Level
  case object TRACE extends Level

  trait Log[P[_]]{
    def log(level: Level, msg: String): P[Unit]
    def trace(msg: String): P[Unit] = log(TRACE,msg)
  }

  object Log{
    object Syntax{
      def log[P[_]: Log](level: Level, msg: String) =
        implicitly[Log[P]].log(level,msg)
      def trace[P[_]: Log](msg: String) =
        implicitly[Log[P]].trace(msg)
    }

    implicit object IOStateActionLog extends Log[IOStateAction]{
      def log(level: Level, msg: String) =
        IOStateAction.IOStateActionIO.write(s"$level: $msg")
    }

    implicit object ConsoleLog extends Log[Id]{
      def log(level: Level, msg: String) =
        println(s"$level: $msg")
    }
  }

  // Generic programs

  object MultipleEffectAbstractProgram{
    import scalaz.Monad
    import IO.Syntax._, Log.Syntax._, scalaz.syntax.monad._

    def echo[P[_]: IO: Log: Monad](): P[String] = for {
      msg <- read()
      _ <- trace(s"read '$msg'")
      _ <- write(msg)
      _ <- trace(s"written '$msg'")
    } yield msg
  }

  "Emmbedding Log inside IO" should "work" in {
    import scalaz.Monad, scalaz.syntax.monad._
    import Log.Syntax._

    object Syntax2 {
      def read[P[_] : IO : Log: Monad](): P[String] =
        for {
          _ <- implicitly[Log[P]].trace("going to read")
          _msg <- implicitly[IO[P]].read()
        } yield _msg

      def write[P[_]](msg: String)(implicit IO: IO[P]): P[Unit] =
        IO.write(msg)
    }

    import Syntax2._
    def echo2[P[_]: IO: Log: Monad](): P[String] = for {
      msg <- read()
      _ <- trace(s"read '$msg'")
      _ <- write(msg)
      _ <- trace(s"written '$msg'")
    } yield msg

    import IOStateAction._

    val init: IOState = IOState(List("hi"),List())

    echo2[IOStateAction]().eval(init) shouldBe
      "hi"

    echo2[IOStateAction]().exec(init) shouldBe
      IOState(List(), List("TRACE: written 'hi'", "hi", "TRACE: read 'hi'", "TRACE: going to read"))
  }

  "IO with logging programs" should "work with io actions" in {
    import MultipleEffectAbstractProgram._
    import IOStateAction._

    val init: IOState = IOState(List("hi"),List())

    echo[IOStateAction]().eval(init) shouldBe
      "hi"

    echo[IOStateAction]().exec(init) shouldBe
      IOState(List(), List("TRACE: written 'hi'", "hi", "TRACE: read 'hi'"))
  }

  // Other kinds of programs: applicative, monad error

  object ApplicativePrograms{
    import IO.Syntax._
    import scalaz.Apply, scalaz.syntax.apply._

    def sayWhat[P[_]: IO: Apply]: P[String] =
      write("Say what?") *> read()
  }


  object MonadErrorPrograms{
    import IO.Syntax._
    import scalaz.MonadError, scalaz.syntax.monadError._

    // Only echoes if it reads a non-empty string; otherwise, raises error

    def echo[P[_]: IO: MonadError[?[_],Exception]](): P[String] = for {
      msg <- read()
      _ <- if (msg == "")
        new Exception("empty string").raiseError[P,Unit]
      else write(msg)
    } yield msg

    // IO instantiation for Either

    implicit object ConsoleOptional extends IO[Either[Exception,?]]{
      def read(): Either[Exception,String] = Right(readLine())
      def write(msg: String): Either[Exception,Unit] = Right(println(msg))
    }
  }

  "IO with monad error" should "work" in {
    import MonadErrorPrograms._
    import scalaz.std.either._

    // type "hi" for this test to succeed
    echo[Either[Exception,?]]() shouldBe
      Right("hi")

    // Type the empty string for this test to succeed
    echo[Either[Exception,?]]().isLeft shouldBe true
  }

  object IOTaskAction {
    import monix.eval.Task
    implicit object IOTaskActionIO extends IO[Task] {
      override def read() = Task { "hi" }
      override def write(msg: String) = Task {}
    }
  }

  object LogTaskAction {
    import monix.eval.Task

    implicit object LogTaskActionLog extends Log[Task] {
      override def log(level: Level, msg: String): Task[Unit] = Task { println(s"TASK $level: $msg") }
    }
  }

  "Simple Effect with Monix tasks" should "work" in {
    import IOTaskAction._
    import monix.eval.Task
    import monix.scalaz._
    import SingleEffectProgram._
    import monix.execution.Scheduler.Implicits.global

    echo[Task]().runAsync.foreach({_ shouldBe "hi"})
  }

  "Multiple Effect with Monix tasks" should "work" in {
    import IOTaskAction._
    import LogTaskAction._
    import monix.eval.Task
    import monix.scalaz._
    import MultipleEffectAbstractProgram._
    import monix.execution.Scheduler.Implicits.global

    echo[Task]().runAsync.foreach({_ shouldBe "hi"})
  }
}