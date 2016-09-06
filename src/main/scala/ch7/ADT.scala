package ch7

trait ADT {

}

object ADT {
  import Par._

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = { es =>
    if (run(es)(cond).get) {
      t(es)
    } else {
      f(es)
    }
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
    val i: Int = run(es)(n).get
    choices(i)(es)
  }
  def choiceMap[K, V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] = { es =>
    val key = run(es)(k).get
    choices(key)(es)
  }

  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = { es =>
    val a = run(es)(pa).get
    f(a)(es)
  }
}

object ADT2 {
  import Par._
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = { es =>
    val a = run(es)(pa).get
    f(a)(es)
  }
  def choiceMap[K, V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    val f = (a) => choices(a)
    flatMap(k)(f)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    val f = (n) => choices(n)
    flatMap(n)(f)
  }

}