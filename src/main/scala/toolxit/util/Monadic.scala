/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.util

import scala.language.higherKinds

/** A generic monadic interface. By implementing this trait, one can use
 *  this data together in for-comprehensions
 *
 *  @author Lucas Satabin
 */
trait Monadic[+T, M[+T] <: Monadic[T, M]] {
  self =>

  class WithFilter(p: T => Boolean) {
    @inline
    def map[U](f: T => U): M[U] =
      self filter p map f
    @inline
    def flatMap[U](f: T => M[U]): M[U] =
      self filter p flatMap f
    /*@inline
    def foreach(f: T => Unit): Unit =
      self filter p foreach f*/
    def withFilter(q: T => Boolean): WithFilter =
      new WithFilter(t => p(t) && q(t))
  }

  def map[U](f: T => U): M[U]
  def flatMap[U](f: T => M[U]): M[U]
  //def foreach(f: T => Unit): Unit
  def filter(p: T => Boolean): M[T]

  def withFilter(p: T => Boolean): WithFilter =
    new WithFilter(p)

}
