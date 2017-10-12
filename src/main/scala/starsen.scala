/*
Author
    Abdul Moeed Bin Babar
    BSCS-5B
    Reg # 140212
    Lab 4
 */

object starsen {
  // main function
  def main(args:Array[String]): Unit ={

    println ("Marix Multipliation")
    var mat1 = Array.ofDim[Int](2,2)
    var mat2 = Array.ofDim[Int](2,2)
    // initilize mat1
    for (i <- 0 until mat1.length) {
        for (j <- 0 until mat1.length) {
          mat1(i)(j) = i-j
        }
    }
    // initilize mat2
    for (i <- 0 until mat2.length) {
      for (j <- 0 until mat2.length) {
        mat2(i)(j) = i+j
      }
    }
    var mat3 = StrassMatMul(mat1,mat2)
    var mat4 = SimpleMul(mat1 ,mat2)
    println ("Marix 1")
    printMat(mat1)
    println ("Marix 2")
    printMat(mat2)
    println ("Marix Multipliation >>>")
    printMat(mat3)
    println("With simple method")
    printMat(mat4)


  }
  //======================================================================================
  // Matrix Multiplication by Stressen formula
  def StrassMatMul(a:Array[Array[Int]], b:Array[Array[Int]]):Array[Array[Int]]= {
    var size = if(a.length == b.length) a.length else 0
    var mat = Array.ofDim[Int](size, size)
    if(size == 1) {// if matrix is of order 1x1
        mat(0)(0) = a(0)(0)* b(0)(0)
     return mat
    }else if(math.pow(2,math.log(size)/math.log(2)) == size){// if matrix is of order  2^n x 2^n
    /*Divide array into 4 arrays*/
    var nSize = size/2
    // matrix 1
    var A = getSubmat(a, 1,nSize)
    var B = getSubmat(a, 2,nSize)
    var C = getSubmat(a, 3,nSize)
    var D = getSubmat(a, 4,nSize)
    // matrix 2
    var E = getSubmat(b, 1,nSize)
    var F = getSubmat(b, 2,nSize)
    var G = getSubmat(b, 3,nSize)
    var H = getSubmat(b, 4,nSize)
    /*========================*/
    var p1 = StrassMatMul(A,subMat(F , H))
    var p2 = StrassMatMul(addMat(A , B),H )
    var p3 = StrassMatMul(addMat(C,D),E)
    var p4 = StrassMatMul(D , subMat(G,E))
    var p5 = StrassMatMul(addMat(A,D), addMat(E,H))
    var p6 = StrassMatMul(subMat(B,D), addMat(G,H))
    var p7 = StrassMatMul(subMat(A,C), addMat(E,F))

    var q1 = addMat(subMat(addMat(p5,p4),p2),p6)
    var q2 = addMat(p1 , p2)
    var q3 = addMat(p3 , p4)
    var q4 = subMat(subMat(addMat(p1,p5),p3),p7)

    mat =joinMatrix(q1,q2,q3,q4)
    return mat
    }else {
      println("Matrix is not of order 2^n")
     return mat
    }
  }
  // function End
  //======================================================================================

  //======================================================================================
  // matrix addition
  def addMat(a:Array[Array[Int]],b:Array[Array[Int]]) = {
    val size = if(a.length == b.length) a.length else 0
    var result = Array.ofDim[Int](size ,size)
    for (i <- 0 until a.length) {
      for (j <- 0 until a.length) {
        result(i)(j) = a(i)(j) + b(i)(j)
      }
    }
    result
  }

  //======================================================================================
  // matrix subtraction
  def subMat(a:Array[Array[Int]],b:Array[Array[Int]]) = {
    val size = if(a.length == b.length) a.length else 0
    var result = Array.ofDim[Int](size ,size)
    for (i <- 0 until a.length) {
      for (j <- 0 until a.length) {
        result(i)(j) = a(i)(j) - b(i)(j)
      }
    }
    result
  }

  //======================================================================================
  // print a matrix
  def printMat(a: Array[Array[Int]]): Unit = {
    for (i <- 0 until a.length) {
      for (j <- 0 until a.length) {
        printf("%6d ",a(i)(j))
      }
      println("")
    }
  }

  //======================================================================================
  // get 1 by 4 of a matrx matrix division is shown below
  /*
      |   1     ,     2     |
      |         ,           |
      |---------,-----------|
      |   3     ,     4     |
      |         ,           |
   */
  // pattern for subdivision to retun from givn matrix  in NUM(2nd) parameter
  def getSubmat(mat: Array[Array[Int]], num:Int , size:Int )= {
    var sub = Array.ofDim[Int](size , size)
    var x1 :Int =0
    var y1 :Int =0
    var x2 :Int =size
    var y2 :Int =size
    if(num == 1){

    }else if(num == 2){
      y1 = size
      y2 = mat.length
    }else if(num == 3){
      x1 = size
      x2 = mat.length
    }else if(num == 4){
      y1 = size
      y2 = mat.length
      x1 = size
      x2 = mat.length
    }

    var x =0
    var y =0
    for(i <- x1 until x2){
      y=0
      for(j <- y1 until y2){
        sub(x)(y) = mat(i)(j)
        y += 1
      }
      x += 1
    }
    sub
  }

  //======================================================================================
  // Join square matrix of Same Order
  def joinMatrix(m1 : Array[Array[Int]] ,m2:Array[Array[Int]] , m3 :Array[Array[Int]] , m4: Array[Array[Int]])={
    var size = m1.length*2
    var HalfSize = m1.length
    var retMat = Array.ofDim[Int](size ,size)
    for(i <- 0 until size){
      for(j <- 0 until size){
        if(i>=HalfSize && j>= HalfSize){
          retMat(i)(j) = m4(i-HalfSize)(j-HalfSize)
        }else if(i >= HalfSize) {
          retMat(i)(j) = m3(i - HalfSize)(j)
        }else if(j >= HalfSize){
          retMat(i)(j) = m2(i)(j-HalfSize)
        }else{
          retMat(i)(j)= m1(i)(j)
        }
      }
    }
    retMat
  }

  //======================================================================================
  // Simple Matrix multiplication by 3 loops
  // For testing purpose
  def SimpleMul(a:Array[Array[Int]],b:Array[Array[Int]]):Array[Array[Int]] = {
    val size  = a.length
    var mat = Array.ofDim[Int](size,size)
    for(i <- 0 until size){
      for(j <- 0 until size){
        for(k <- 0 until size) {
          mat(i)(j) += a(i)(k) * b(k)(j)
        }
      }
    }
    return mat
  }
  // ======================  The End ======================
}
