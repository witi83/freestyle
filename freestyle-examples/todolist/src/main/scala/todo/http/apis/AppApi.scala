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
import freestyle.effects.error._
import todo.definitions.models._
import todo.runtime.implicits._

class AppApi[F[_]](
    implicit todoItemApi: TodoItemApi[F],
    todoListApi: TodoListApi[F],
    tagApi: TagApi[F],
    log: LoggingM[F],
    error: ErrorM[F],
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
        tag <- tagApi.insertProgram(form.tag)
        t   <- error.either[Tag](tag.toRight(new NoSuchElementException))

        list <- todoListApi.insertProgram(form.list.copy(tagId = t.id))
        l    <- error.either[TodoList](list.toRight(new NoSuchElementException))

        i <- todoItemApi.insertBatchProgam(form.items, l)
        items = i.sequence
      } yield Ok(form.copy(list = l, tag = t, items = items getOrElse form.items))
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
      error: ErrorM[F],
      handler: F ~> Future): AppApi[F] =
    new AppApi[F]
}
