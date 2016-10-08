package com.github.agrits.gameoflife

import java.awt.{Dimension, Graphics}
import javax.swing.{JFrame, JPanel}

/**
  * Created by grits on 02.10.16.
  */
object Main extends JPanel{
  //hard-coded settings
  val squareSize = 5
  val W = 640
  val H = 480
  val toReborn = Array[Int](3)
  val toSurvive = Array[Int](2,3)

  var array = Array.ofDim[Boolean](W/squareSize, H/squareSize)
  for (x <- 0 to array.size - 1)
    for (y <- 0 to array(0).size - 1)
      array.update(x, array(x).updated(y, false))
  var generation = 0;
  def addAlive(x : Int, y : Int) : Unit = array.update(x, array(x).updated(y, true))
  randomAlives
  def main(args: Array[String]) = {
    val f = new JFrame("Game of life")
    this.setPreferredSize(new Dimension(W, H))
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.add(this)
    f.pack()
    f.setLocationRelativeTo(null)
    f.setResizable(false)
    f.setVisible(true)
    while(true){
      Thread.sleep(1)
      repaint()
      array = updatedBoard(array)
      if(array.filter(_.distinct.size!=1).isEmpty) randomAlives
    }
  }
  def randomAlives : Unit = for(x <- 1 to 5000) addAlive((Math.random()*(W/squareSize)).asInstanceOf[Int], (Math.random()*(H/squareSize)).asInstanceOf[Int])
  this.setSize(100, 100)

  override def paint(graphics: Graphics): Unit = {
    super.paint(graphics)
    for (x <- 0 to array.size - 1) {
      for (y <- 0 to array(0).size - 1) {
        if (array(x)(y)) graphics.fillRect(squareSize * x, squareSize * y, squareSize, squareSize)
      }
    }
    graphics.drawString("Generation: "+generation, 10, 10)
    generation+=1
  }
  //I needed to use argument storing first version of the board (protoBoard), because change of board during this recursion
  // will also affect neigbours count.
  def updatedBoard(board : Array[Array[Boolean]]): Array[Array[Boolean]] = {
    def goRecursive(b: Array[Array[Boolean]], pos : (Int, Int), protoBoard : Array[Array[Boolean]]) : Array[Array[Boolean]] = {
        if (pos._1 == b.size - 1 && pos._2 == b(0).size - 1) return b
        else return goRecursive(updatedCell(pos._1, pos._2, b, protoBoard), incrementPosition(pos, b(0).size - 1), protoBoard)
    }
    return goRecursive(board, (0, 0), board)
  }
  def incrementPosition(pos : (Int, Int), maxY : Int) = {
    if(pos._2==maxY) (pos._1+1, 0) else (pos._1, pos._2+1)
  }
  def updatedCell(x : Int, y : Int, board : Array[Array[Boolean]], protoBoard : Array[Array[Boolean]]) : Array[Array[Boolean]]=
    return board.updated(x, board(x).updated(y, switchState(board(x)(y), countAliveNeighbours(x, y, protoBoard))))

  def switchState(current : Boolean, neighbours : Int) : Boolean =
    if(current) return toSurvive.contains(neighbours) else return toReborn.contains(neighbours)


  def countAliveNeighbours(x : Int, y : Int, board : Array[Array[Boolean]]): Int ={
    def inBounds(x : Int, y : Int) = x >= 0 && x < board.size && y > 0 && y < board(0).size
    val neighbours = Array((x-1, y-1),(x-1, y),(x-1, y+1),
                            (x, y-1),(x, y+1),(x+1, y-1),
                            (x+1, y),(x+1, y+1))
    return neighbours.count(z => inBounds(z._1, z._2) && board(z._1)(z._2))
  }
}