package net.degoes.zio

import zio.{ExitCode, ZEnv, _}
import zio.duration.Duration

import java.io.IOException

object HelloWorld_1 extends App {

  import zio.console._

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    putStrLn("Hello World!").exitCode
}



object Sequence_2 extends App {
  import zio.console._

  // SYH: zipLeft zipRight *> <* to decide the type of the final effect/IO monad
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    (putStrLn("Hello ").zipLeft(putStrLn("World!"))).exitCode
    //(putStrLn("Hello ") *> (putStrLn("World!"))).exitCode

    //putStrLn("Hello ").zipRight(putStrLn("World!")).exitCode
    //(putStrLn("Hello ") <* (putStrLn("World!"))).exitCode
  }
}

object Error_Recovery_3 extends App {
  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!").exitCode


  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    failed orElse(ZIO.succeed(ExitCode (0)))
    /*
    failed.catchAllCause(cause =>
      putStrLn(s"${cause.prettyPrint}") *>
      ZIO.succeed(ExitCode (0)))
    */
  }
}

object ForComprehension_4 extends App {
  import zio.console._

  val readInt = getStrLn.flatMap(string => ZIO(string.toInt)).orDie

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      _   <- putStrLn("How old are you?")
      age <- readInt
      _ <- if (age < 18) putStrLn("You are a kid!")
      else putStrLn("You are all grown up!")
    } yield ExitCode.success
}


object AlarmApp_5 extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.effect(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        ).refineToOrDie[NumberFormatException]

    val fallback = putStrLn("You didn't enter a number of seconds!") *> getAlarmDuration

    for {
      _        <- putStrLn("Please enter the number of seconds to sleep: ")
      input    <- getStrLn
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }


  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for{
      duration <- getAlarmDuration
      fiber    <- (putStr(".") *> ZIO.sleep(1.second)).forever.fork
      _        <- ZIO.sleep(duration)
      _        <- putStrLn("Time to wakeup!!!\n")
      _        <- fiber.interrupt

    } yield ExitCode (0)) orElse(ZIO.succeed(ExitCode (0)))
}

object ComputePi_6 extends App {
  import zio.random._
  import zio.console._
  import zio.clock._
  import zio.duration._
  import zio.stm._

  final case class PiState(inside: Long, total: Long)

  def estimatePi(inside: Long, total: Long): Double = (inside.toDouble / total.toDouble) * 4.0

  def insideCircle(x: Double, y: Double): Boolean = Math.sqrt(x * x + y * y) <= 1.0

  val randomPoint: ZIO[Random, Nothing, (Double, Double)] = nextDouble zip nextDouble

  def updateOnce(ref: Ref[PiState]): ZIO[Random, NoSuchElementException, Unit] =
    for {
      (x, y)   <- randomPoint
      inside   = if (insideCircle(x, y)) 1 else 0
      _        <- ref.update(state => PiState(state.inside + inside, state.total +1))
    } yield ()


  def printEstimatedPi (ref: Ref[PiState]): ZIO[Console, Nothing, Unit] =
    for {
      state  <- ref.get
      _       <- putStrLn(s"${estimatePi(state.inside, state.total)}")
    } yield ()

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for{
      ref <- Ref.make(PiState(0, 0))
      worker = updateOnce(ref).forever
      workers = List.fill(4)(worker)
      fiber1  <- ZIO.forkAll(workers)
      fiber2  <- (printEstimatedPi(ref) *> ZIO.sleep(1.second)).forever.fork
      _       <- putStrLn("Enter to terminate ... ")
      _       <- getStrLn *> fiber1.interrupt *> fiber2.interrupt
    } yield ()).exitCode //orElse(ZIO.succeed(ExitCode(0)))
}
//3.1415926535897...

object More_Fiber_Usage extends App{
  import zio.duration._

  def join:ZIO[Any, Nothing, String] =
    for {
      fiber   <- IO.succeed("Hi!").fork
      message <- fiber.join
    } yield message

  def await:ZIO[Any, Nothing, Exit[Nothing, String]] =
    for {
      fiber <- IO.succeed("Hi!").fork
      exit  <- fiber.await
    } yield exit

  // If either fiber fails, then the composed fiber will fail.
  def compose1:ZIO[Any, Nothing, String] =
    for {
      fiber1 <- IO.succeed("Hi!").fork
      fiber2 <- IO.succeed("Bye!").fork
      fiber   = fiber1.zip(fiber2)
      tuple  <- fiber.join
    } yield tuple

  // If the first fiber succeeds, the composed fiber will succeed with its result;
  // otherwise, the composed fiber will complete with the exit value of the second fiber (whether success or failure).
  def compose2:ZIO[Any, Nothing, String] =
    for {
      fiber1 <- IO.fail("Uh oh!").fork
      fiber2 <- IO.succeed("Hurray!").fork
      fiber   = fiber1.orElse(fiber2)
      tuple  <- fiber.join
    } yield tuple

  // returning the first successful result:
  def racing:ZIO[Any, Nothing, String] =
    for {
      winner <- IO.succeed("Hello").race(IO.succeed("Goodbye"))
    } yield winner

  //IO.succeed("Hello").timeout(10.seconds)

  def run (args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    IO.succeed(ExitCode(0))

}