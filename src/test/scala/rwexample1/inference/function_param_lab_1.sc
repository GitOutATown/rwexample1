package lab.concurrency

object function_param_lab_1 {

  /* Scala's is able to infer
   * function parameter types when the function parameter is itself an anonymus
   * function (Example 1). By contrast, Scala does not infer the type of a
   * prameter to a function when that parameter itself is not a function as in
   * Example 2.
   */

  // ----- Example 1 ----- //
  
  class Foo()
  class Bar()
  
  def func1(f: Foo => Int) {}                     //> func1: (f: lab.concurrency.function_param_lab_1.Foo => Int)Unit
  def func2(f: Bar => Int) {}                     //> func2: (f: lab.concurrency.function_param_lab_1.Bar => Int)Unit
  
  func1(x => 3) // x is recognized as type Foo by ScalaIDE (eg: hover)
  func2(x => 3) // x is recognized as type Bar by ScalaIDE (eg: hover)
  
  // Error scenarios
  // func1(x => Int) // error: found type Int, requires Int
  // func1(new Foo => 3) // error: param is not a function
  // func1(3) // required: Foo => Int
  // etc.
    
  // ----- Example 2 ----- //
  
  def func3(f: Foo) {}                            //> func3: (f: lab.concurrency.function_param_lab_1.Foo)Unit
  // func3(x) // not found: value y
}