
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val pixel = src.apply(x, y)

    val leftTopCoordinate = (x - radius, y - radius)
    val rightDownCoordinate = (x + radius, y + radius)

    val rgbaOfPixels = for {
      byX <- leftTopCoordinate._1 to rightDownCoordinate._1 // if byX != x
      byY <- leftTopCoordinate._2 to rightDownCoordinate._2 // if byY != y
      clampedX = clamp(byX, 0, src.width)
      clampedY = clamp(byY, 0, src.height)
      currentPixel = src.apply(clampedX, clampedY)
    } yield (red(currentPixel), green(currentPixel), blue(currentPixel), alpha(currentPixel))

    val sum = rgbaOfPixels.foldLeft((0, 0, 0, 0))( (acc, v) => {
      (acc._1 + v._1, acc._2 + v._2, acc._3 + v._3, acc._4 + v._4)
    })

    rgba(sum._1 / rgbaOfPixels.length,
         sum._2 / rgbaOfPixels.length,
         sum._3 / rgbaOfPixels.length,
         sum._4 / rgbaOfPixels.length)
  }

}
