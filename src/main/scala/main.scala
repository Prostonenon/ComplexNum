import scala.math._

case class ComplexNum(real: Double, image: Double) {

  def unary_- =
    new ComplexNum(-real, -image)

  def unary_~ =
    new ComplexNum(real, -image)

  def unary_! =
    sqrt(pow(real, 2) + pow(image, 2)) //Модуль Комплексного числа

  def +(compl: ComplexNum) =
    new ComplexNum(real + compl.real, image + compl.image)

  def -(compl: ComplexNum) =
    new ComplexNum(real - compl.real, image - compl.image)

  def *(comp: ComplexNum) = {
    val r = real * comp.real - image * comp.image
    val i = real * comp.image + comp.real * image
    new ComplexNum(r, i)
  }

  def /(comp: ComplexNum) =  {
    if (comp.real == 0 || comp.image == 0 )
      "Error! Wrong 2nd complex number"

    else
      val k = pow(comp.real, 2) + pow(comp.image, 2)
      val r = (real * comp.real + image * comp.image) / k
      val i = (comp.real * image - real * comp.image) / k
      new ComplexNum(r, i)
  }

  override def toString = {

    if (image >= 0) s"$real+$image*i"
    else s"$real$image*i"
  }
}


@main def hello(): Unit = {
  val c1: ComplexNum = new ComplexNum(1, 2)
  val c2: ComplexNum = new ComplexNum(2, 3)
  val c3: ComplexNum = new ComplexNum(3, 0)
  val c4: ComplexNum = new ComplexNum(0, 1)
  println(c1)
  println(-c2)
  println(~c1)
  println(!c2)
  println(c1+c2)
  println(c2-c1)
  println(c1*c2)
  println (c2/c3) //Error
  println (c2/c4) //Error
  println (c2/c1)

}