package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var _batteryLife: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    if(_batteryLife > 0)
      robot.act()
      println(robot.toString)
      _batteryLife = _batteryLife - 1
    else
      println("WARNING: insufficient battery")

class RobotCanFail(val robot: Robot) extends Robot:
  private val randomError = new Random();
  export robot.{position, direction, turn}
  override def act(): Unit =
    if(randomError.nextBoolean())
      robot.act()
      println(robot.toString)
    else
      println("ERR: failed act")

class RobotRepeated(val robot: Robot, val _repetitions: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    val range = 1 until (_repetitions)
    range.foreach(iteration =>robot.act());

@main def testRobot(): Unit =
  println("#testRobot - LoggingRobot")
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
  println("#testRobot - RobotWithBattery")
  val robot2 = RobotWithBattery(SimpleRobot((0, 0),Direction.North),2);
  robot2.act() // robot at (0, 1) facing North
  robot2.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot2.act() // robot at (1, 1) facing East
  robot2.act() // WARNING: insufficient battery
  println("#testRobot - RobotCanFail")
  val robot4 = RobotRepeated(SimpleRobot((0, 0),Direction.North),5)
  robot.act() // robot at (2, 1) facing East