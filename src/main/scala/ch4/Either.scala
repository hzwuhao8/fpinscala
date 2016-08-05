package ch4

import scala.collection.mutable.Buffer

trait Either[+E, +A] {
  def isRight(): Boolean = this match {
    case Left(_) => false
    case _       => true
  }
  def isLeft(): Boolean = this match {
    case Left(_) => true
    case _       => false
  }

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e)  => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e)  => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e) => Left(e)
    case Right(aa) => b match {
      case Left(be)  => Left(be)
      case Right(bb) => Right(f(aa, bb))
    }
  }

  def get(): A = this match {
    case Right(a) => a
    case Left(e)  => throw new RuntimeException("Left")
  }
  def traverse[A, E, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @annotation.tailrec
    def tt(list: List[A], res: Either[E, Buffer[B]])(f: A => Either[E, B]): Either[E, Buffer[B]] = {
      list match {
        case Nil => res
        case h :: t =>
          println(h)
          val v = f(h)
          v match {
            case Left(ee) => Left(ee)
            case Right(x) => tt(t, Right(res.get += x))(f)
          }
      }
    }
    val r = tt(list, Right(Buffer[B]()))(f)
    r.map(_.toList)
  }

}

case class Left[E, A](e: E) extends Either[E, A]
case class Right[E, A](a: A) extends Either[E, A]
