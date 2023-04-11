import javafx.scene.Node
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, Shape3D}

import java.io.{File, FileNotFoundException, IOException}
import java.util.Scanner

object OctreeUtils {
  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size

  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D
  type Section = (Placement, List[Node]) //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))


  //T1

  def isRgb(string: String): Boolean = {
    if (string(0) == '(' && string(string.length - 1) == ')') {
      val splt = string.substring(1, string.length - 1).split(",")
      if (splt.length == 3 && splt(0).forall(Character.isDigit) && splt(1).forall(Character.isDigit) && splt(2).forall(Character.isDigit)
        && splt(0).nonEmpty && splt(1).nonEmpty && splt(2).nonEmpty) {
        if (splt(0).toInt >= 0 && splt(0).toInt <= 255 && splt(1).toInt >= 0 && splt(1).toInt <= 255 && splt(2).toInt >= 0 && splt(2).toInt <= 255) {
          true
        }
        else false
      }
      else false
    }
    else false
  }


  def isDouble(string: String): Boolean = {
    if (string.toDoubleOption.isEmpty) false
    else true
  }


  def ReadFile(path: String): List[Shape3D] = {

    def aux(line: List[String]): Shape3D = {

      def setSettings(node: Shape3D, line: List[String]): Shape3D = {
        node.setTranslateX(line(2).toDouble)
        node.setTranslateY(line(3).toDouble)
        node.setTranslateZ(line(4).toDouble)
        node.setScaleX(line(5).toDouble)
        node.setScaleY(line(6).toDouble)
        node.setScaleZ(line(7).toDouble)
        val givenColors = line(1).substring(1, line(1).length() - 1).split(",")
        val color = new PhongMaterial()
        color.setDiffuseColor(Color.rgb(givenColors(0).toInt, givenColors(1).toInt, givenColors(2).toInt))
        node.setMaterial(color)
        node
      }

      if (line.length == 8 && isRgb(line(1)) && isDouble(line(2)) && isDouble(line(3)) && isDouble(line(4))
        && isDouble(line(5)) && isDouble(line(6)) && isDouble(line(7))) {
        if (line(0) == "Cylinder") {
          val cyl = new Cylinder(0.5, 1, 10)
          setSettings(cyl, line)
        }
        else if (line(0) == "Box") {
          val box = new Box(1, 1, 1)
          setSettings(box, line)
        }
        else {
          println("args errados")
          System.exit(1)
          null
        }
      }
      else {
        println("args errados")
        System.exit(1)
        null
      }
    }

    def addToList(scanner: Scanner, lst: List[Shape3D]): List[Shape3D] = {
      if (scanner.hasNextLine()) {
        val token = aux(scanner.nextLine().split(' ').toList) :: lst
        addToList(scanner, token)
      }
      else
        lst

    }

    try {
      val file = new File(path)
      val scanner = new Scanner(file)
      val lst: List[Shape3D] = List()
      val res = addToList(scanner, lst)
      scanner.close()
      res
    }
    catch {
      case e: FileNotFoundException => {
        println("File doesn't exist.")
        System.exit(1)
        null
      }
      case e: IOException => {
        println("IOException while trying to read file")
        System.exit(1)
        null
      }

    }

  }


  //T4
  def scalePlacement(placement: Placement, factor: Double): Placement = {
    if (factor == 2 && placement._2 * 2  <= 32)
      ((placement._1._1 * factor, placement._1._2 * factor, placement._1._3 * factor), math.pow(placement._2, 3))
    else
      ((placement._1._1 * factor, placement._1._2 * factor, placement._1._3 * factor), math.cbrt(placement._2))
  }

  def scaleNodes(lst: List[Node], factor: Double): List[Node] = {
    def treatNode(node: Node, factor: Double): Node = {
      node.setScaleX(node.getScaleX * factor)
      node.setScaleY(node.getScaleY * factor)
      node.setScaleZ(node.getScaleZ * factor)
      node
    }

    lst match {
      case x :: lst => treatNode(x, factor) :: scaleNodes(lst, factor)
      case Nil => Nil
    }
  }

  def scale(octree: Octree[Placement], factor: Double): Octree[Placement] = {
    if(factor == 2 || factor == 0.5) {
      octree match {
        case OcEmpty => OcEmpty
        case OcLeaf(section: Section) => OcLeaf(scalePlacement(section._1, factor), scaleNodes(section._2, factor))
        case OcNode(placement, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => OcNode(scalePlacement(placement, factor),
          scale(up_00, factor), scale(up_01, factor), scale(up_10, factor), scale(up_11, factor), scale(down_00, factor), scale(down_01, factor),
          scale(down_10, factor), scale(down_11, factor))
      }
    }
    else {
      println("Somente é aceite factor = 2 ou factor = 0.5")
      return octree;
    }
  }

  //T5
  def ChangeColorNodes(lst: List[Node], func: Color => Color): List[Node] = {
    def treatNode(node: Shape3D, func: Color => Color): Node = {
      val cor = func(node.getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor)
      node.setMaterial(new PhongMaterial(cor))
      node
    }

    lst match {
      case x :: lst => treatNode(x.asInstanceOf[Shape3D], func) :: ChangeColorNodes(lst, func)
      case Nil => Nil
    }
  }


  def mapColourEffect(func: Color => Color, oct: Octree[Placement]): Octree[Placement] = {
    oct match {
      case OcEmpty => OcEmpty
      case OcLeaf(section: Section) => OcLeaf(section._1, ChangeColorNodes(section._2, func))
      case OcNode(placement, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => OcNode(placement, mapColourEffect(func, up_00),
        mapColourEffect(func, up_10), mapColourEffect(func, up_01), mapColourEffect(func, up_11), mapColourEffect(func, down_00),
        mapColourEffect(func, down_01), mapColourEffect(func, down_10), mapColourEffect(func, down_11))
    }
  }

  def Sepia(color: Color): Color = {
    val (r, g, b) = (color.getRed, color.getGreen, color.getBlue)
    val Red = scala.math.min(0.393 * r + 0.769 * g + 0.189 * b, 1)
    val Green = scala.math.min(0.349 * r + 0.686 * g + 0.168 * b, 1)
    val Blue = scala.math.min(0.272 * r + 0.534 * g + 0.131 * b, 1)
    new Color(Red, Green, Blue, 1.0)
  }

  def greenRemove(color: Color): Color = {
    new Color(color.getRed, 0.0, color.getBlue, 1.0)
  }

  //T3
  def setBoxColor(box: Shape3D, camVolume: Cylinder): Unit = {
    if (camVolume.getBoundsInParent.contains(box.getBoundsInParent)
      || camVolume.getBoundsInParent.intersects(box.getBoundsInParent))
      box.setMaterial(new PhongMaterial(new Color(1, 1, 1, 1)))
    else
      box.setMaterial(new PhongMaterial(new Color(1, 0, 0, 1)))
  }



}
