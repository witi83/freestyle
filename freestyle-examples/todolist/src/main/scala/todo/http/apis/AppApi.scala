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
import cats.instances.option._
import cats.syntax.traverse._
import com.twitter.util.Future
import io.finch._
import io.finch.circe._
import io.circe.generic.auto._
import freestyle._
import freestyle.implicits._
import freestyle.http.finch._
import freestyle.logging._
import todo.definitions.models._
import todo.runtime.implicits._

class AppApi[F[_]](
    implicit todoItemApi: TodoItemApi[F],
    todoListApi: TodoListApi[F],
    tagApi: TagApi[F],
    log: LoggingM[F],
    handler: F ~> Future) {
  val reset: Endpoint[Int] =
    post("reset") {
      for {
        tags  <- tagApi.resetProgram
        lists <- todoListApi.resetProgram
        items <- todoItemApi.resetProgram
      } yield Ok(tags + lists + items)
    }

  val create: Endpoint[TodoForm] =
    post("create" :: jsonBody[TodoForm]) { form: TodoForm =>
      for {
        list <- todoListApi.insertProgram(form.list)                // Option[TodoList]
        i    <- todoItemApi.insertBatchProgam(form.items, list.get) // List[Option[TodoItem]]
        items = i.sequence // Option[List[TodoItem]]
//        tag <- form.tag.map(tagApi.insertProgram(_))
      } yield Ok(form.copy(list = list getOrElse form.list, items = items getOrElse form.items))

    }

  val endpoints = reset :+: create
}

object AppApi {
  implicit def instance[F[_]](
      implicit genericApi: GenericApi[F],
      todoItemApi: TodoItemApi[F],
      todoListApi: TodoListApi[F],
      tagApi: TagApi[F],
      log: LoggingM[F],
      handler: F ~> Future): AppApi[F] =
    new AppApi[F]
}
