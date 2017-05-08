/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package todo
package http
package apis

import cats.~>
import cats.instances.list._
import cats.syntax.traverse._
import com.twitter.util.Future
import io.finch._
import io.finch.circe._
import io.circe.generic.auto._
import freestyle._
import freestyle.implicits._
import freestyle.http.finch._
import freestyle.logging._

import todo.definitions.models.Tag
import todo.definitions.persistence._
import todo.runtime.implicits._

class TagApi[F[_]](implicit repo: TagRepository[F], log: LoggingM[F], handler: F ~> Future) {
  val prefix = "tags"
  val model  = "Tag"

  type A = Tag

  val resetProgram: FreeS[F, Int] =
    for {
      _ <- log.debug(s"Trying to reset $model in repository")
      r <- repo.init
      _ <- log.warn(s"POST /$prefix/reset: Initialize the $model table")
    } yield r

  def retrieveProgram(id: Int): FreeS[F, Option[A]] =
    for {
      _    <- log.debug(s"Trying to retrieve an $model")
      item <- repo.get(id)
      _    <- log.info(s"GET /$prefix/$id: Found $item")
    } yield item

  val listProgram: FreeS[F, List[A]] =
    for {
      _     <- log.debug(s"Trying to get all $model models")
      items <- repo.list
      _     <- log.info(s"GET /$prefix: Found all the $model models")
    } yield items

  def insertProgram(item: A): FreeS[F, Option[A]] =
    for {
      _ <- log.debug(s"Trying to insert a $model")
      r <- repo.insert(item)
      _ <- log.info(s"POST /$prefix with $item: Tried to add $model")
    } yield r

  def updateProgram(item: A): FreeS[F, Option[A]] =
    for {
      _ <- log.debug(s"Trying to update a $model")
      r <- repo.update(item)
      _ <- log.info(s"PUT /$prefix/${item.id} with $item: Tried to update $model")
    } yield r

  def destroyProgram(id: Int): FreeS[F, Int] =
    for {
      _ <- log.debug(s"Trying to delete a $model")
      r <- repo.delete(id)
      _ <- log.info(s"DELETE /$prefix/$id: Tried to delete $model")
    } yield r

  def insertBatchProgam(items: List[A]): FreeS[F, List[Option[A]]] =
    for {
      _ <- log.debug(s"Trying to insert batch $model")
      r <- items.map(i => repo.insert(i)).sequence
    } yield r

  def updateBatchProgram(items: List[A]): FreeS[F, List[Option[A]]] =
    for {
      _ <- log.debug(s"Trying to update batch $model")
      r <- items.map(i => repo.update(i)).sequence
    } yield r

  def destroyBatchProgam(ids: List[Int]): FreeS[F, Int] =
    for {
      _ <- log.debug(s"Trying to destroy batch $model")
      r <- ids.map(repo.delete(_)).sequence
    } yield r.sum

  val reset: Endpoint[Int] =
    post(prefix :: "reset") {
      resetProgram.map(Ok(_))
    }

  val retrieve: Endpoint[A] =
    get(prefix :: int) { id: Int =>
      retrieveProgram(id) map (item =>
        item.fold[Output[A]](NotFound(new NoSuchElementException(s"Could not find $model with $id")))(Ok(_)))
    } handle {
      case nse: NoSuchElementException => NotFound(nse)
    }

  val list: Endpoint[List[A]] =
    get(prefix) {
      listProgram.map(Ok(_))
    }

  val insert: Endpoint[Option[A]] =
    post(prefix :: jsonBody[A]) { item: A =>
      insertProgram(item).map(Ok(_))
    }

  val update: Endpoint[Option[A]] =
    put(prefix :: int :: jsonBody[A]) { (id: Int, item: A) =>
      updateProgram(item.copy(id = Some(id))).map(Ok(_))
    }

  val destroy: Endpoint[Int] =
    delete(prefix :: int) { id: Int =>
      destroyProgram(id).map(Ok(_))
    }

  val endpoints = reset :+: retrieve :+: list :+: insert :+: update :+: destroy
}

object TagApi {
  implicit def instance[F[_]](
      implicit repo: TagRepository[F],
      log: LoggingM[F],
      handler: F ~> Future): TagApi[F] =
    new TagApi[F]
}
