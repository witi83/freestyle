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
import com.twitter.util.Future
import io.finch._
import io.finch.circe._
import io.circe.generic.auto._
import freestyle._
import freestyle.implicits._
import freestyle.http.finch._
import freestyle.logging._

import todo.definitions.models.TodoItem
import todo.definitions.persistence._
import todo.runtime.implicits._

class TodoItemApi[F[_]](
    implicit repo: TodoItemRepository[F],
    log: LoggingM[F],
    handler: F ~> Future) {
  val resetProgram: FreeS[F, Int] =
    for {
      _ <- log.debug("Trying to reset TodoItem in repository")
      r <- repo.init
      _ <- log.warn("POST /items/reset: Initialize the TodoItem table")
    } yield r

  val reset: Endpoint[Int] =
    post("items" :: "reset") {
      resetProgram.map(Ok(_))
    }

  val retrieve: Endpoint[TodoItem] =
    get("items" :: int) { id: Int =>
      for {
        _    <- log.debug("Trying to retrieve an TodoItem")
        item <- repo.get(id)
        _    <- log.info(s"GET /items/$id: Found $item")
      } yield item.fold[Output[TodoItem]](NotFound(new NoSuchElementException))(Ok(_))
    }

  val list: Endpoint[List[TodoItem]] =
    get("items") {
      for {
        _     <- log.debug("Trying to get all TodoItems")
        items <- repo.list
        _     <- log.info(s"GET /items: Found all the TodoItems")
      } yield Ok(items)
    }

  val insert: Endpoint[Int] =
    post("items" :: jsonBody[TodoItem]) { item: TodoItem =>
      for {
        _ <- log.debug("Trying to insert a TodoItem")
        r <- repo.insert(item)
        _ <- log.info(s"POST /items with $item: Tried to add TodoItem")
      } yield Ok(r)
    }

  val update: Endpoint[Int] =
    put("items" :: int :: jsonBody[TodoItem]) { (id: Int, item: TodoItem) =>
      for {
        _ <- log.debug("Trying to update a TodoItem")
        r <- repo.update(item.copy(id = Some(id)))
        _ <- log.info(s"PUT /items/$id with $item: Tried to update TodoItem")
      } yield Ok(r)
    }

  val destroy: Endpoint[Int] =
    delete("items" :: int) { id: Int =>
      for {
        _ <- log.debug("Trying to delete a TodoItem")
        r <- repo.delete(id)
        _ <- log.info("DELETE /items/$id: Tried to delete TodoItem")
      } yield Ok(r)
    }

  val endpoints = reset :+: retrieve :+: list :+: insert :+: update :+: destroy
}

object TodoItemApi {
  implicit def instance[F[_]](
      implicit repo: TodoItemRepository[F],
      log: LoggingM[F],
      handler: F ~> Future): TodoItemApi[F] =
    new TodoItemApi[F]
}
