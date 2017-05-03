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

import todo.definitions.models.TodoList
import todo.definitions.persistence._
import todo.runtime.implicits._

class TodoListApi[F[_]](
    implicit repo: TodoListRepository[F],
    log: LoggingM[F],
    handler: F ~> Future) {
  val resetProgram =
    for {
      _ <- log.debug("Trying to reset TodoList in repository")
      r <- repo.init
      _ <- log.warn("POST /lists/reset: Initialize the TodoList table")
    } yield r

  val reset: Endpoint[Int] =
    post("lists" :: "reset") {
      resetProgram.map(Ok(_))
    }

  val retrieve: Endpoint[TodoList] =
    get("lists" :: int) { id: Int =>
      for {
        _    <- log.debug("Trying to retrieve an TodoList")
        item <- repo.get(id)
        _    <- log.info(s"GET /lists/$id: Found $item")
      } yield item.fold[Output[TodoList]](NotFound(new NoSuchElementException))(Ok(_))
    }

  val list: Endpoint[List[TodoList]] =
    get("lists") {
      for {
        _     <- log.debug("Trying to get all TodoLists")
        items <- repo.list
        _     <- log.info(s"GET /lists: Found all the TodoLists")
      } yield Ok(items)
    }

  val insert: Endpoint[Int] =
    post("lists" :: jsonBody[TodoList]) { item: TodoList =>
      for {
        _ <- log.debug("Trying to insert a TodoList")
        r <- repo.insert(item)
        _ <- log.info(s"POST /lists with $item: Tried to add TodoList")
      } yield Ok(r)
    }

  val update: Endpoint[Int] =
    put("lists" :: int :: jsonBody[TodoList]) { (id: Int, item: TodoList) =>
      for {
        _ <- log.debug("Trying to update a TodoList")
        r <- repo.update(item.copy(id = Some(id)))
        _ <- log.info(s"PUT /lists/$id with $item: Tried to update TodoList")
      } yield Ok(r)
    }

  val destroy: Endpoint[Int] =
    delete("lists" :: int) { id: Int =>
      for {
        _ <- log.debug("Trying to delete a TodoList")
        r <- repo.delete(id)
        _ <- log.info("DELETE /lists/$id: Tried to delete TodoList")
      } yield Ok(r)
    }

  val endpoints = reset :+: retrieve :+: list :+: insert :+: update :+: destroy
}

object TodoListApi {
  implicit def instance[F[_]](
      implicit repo: TodoListRepository[F],
      log: LoggingM[F],
      handler: F ~> Future): TodoListApi[F] =
    new TodoListApi[F]
}
