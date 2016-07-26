package leon.lang

object ArrayEqual1 {

  def f: Boolean = {
    Array(1,2,3) == Array(1,2,3)
  } ensuring(res => res)

}

