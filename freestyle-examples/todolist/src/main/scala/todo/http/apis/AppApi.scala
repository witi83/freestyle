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
import cats.syntax.semigroup._
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
import todo.definitions.persistence.AppRepository
import todo.runtime.implicits._

class AppApi[F[_]](
    implicit repo: AppRepository[F],
    todoItemApi: TodoItemApi[F],
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

  val list: Endpoint[List[TodoForm]] =
    get("list") {
      val program: FreeS[F, List[(TodoList, Tag, TodoItem)]] = for {
        l <- repo.list
      } yield l

      program
        .map(_.groupBy(x => (x._1, x._2)).map {
          case ((todoList, tag), list) =>
            TodoForm(todoList, tag, list.map(_._3))
        }.toList)
        .map(Ok(_))
    }

  val insert: Endpoint[TodoForm] =
    post("insert" :: jsonBody[TodoForm]) { form: TodoForm =>
      for {
        tag <- tagApi.insertProgram(form.tag)
        t   <- error.either[Tag](tag.toRight(new NoSuchElementException("Could not create Tag")))

        list <- todoListApi.insertProgram(form.list.copy(tagId = t.id))
        l    <- error.either[TodoList](list.toRight(new NoSuchElementException("Could not create TodoList")))

        i <- todoItemApi.insertBatchProgam(form.items.map(_.copy(todoListId = l.id)))
        items = i.sequence
      } yield Ok(form.copy(list = l, tag = t, items = items getOrElse form.items))
    } handle {
      case nse: NoSuchElementException => InternalServerError(nse)
    }

  val update: Endpoint[TodoForm] =
    put("update" :: jsonBody[TodoForm]) { form: TodoForm =>
      for {
        tag <- tagApi.updateProgram(form.tag)
        t   <- error.either[Tag](tag.toRight(new NoSuchElementException("Could not update Tag")))

        list <- todoListApi.updateProgram(form.list.copy(tagId = t.id))
        l    <- error.either[TodoList](list.toRight(new NoSuchElementException("Could not update TodoList")))

        i <- todoItemApi.updateBatchProgram(form.items.map(_.copy(todoListId = l.id)))
        items = i.sequence
      } yield Ok(form.copy(list = l, tag = t, items = items getOrElse form.items))
    } handle {
      case nse: NoSuchElementException => InternalServerError(nse)
    }

  val destroy: Endpoint[Int] =
    delete("delete" :: jsonBody[TodoForm]) { form: TodoForm =>
      val todoItemIds: Option[List[Int]] = form.items.map(_.id).sequence

      val program: Option[List[FreeS[F, Int]]] = for {
        items <- todoItemIds.map(todoItemApi.destroyBatchProgam(_))
        list  <- form.list.id.map(todoListApi.destroyProgram(_))
        tags  <- form.tag.id.map(tagApi.destroyProgram(_))
      } yield List(items, list, tags)

      program.fold[FreeS[F, Output[Int]]](
        FreeS.pure[F, Output[Int]](BadRequest(new NoSuchElementException("Could not delete")))) { x =>
        x.sequenceU
          .map(_.sum)
          .map(Ok(_))
      }
    } handle {
      case nse: NoSuchElementException => InternalServerError(nse)
    }

  val endpoints = reset :+: list :+: insert :+: update :+: destroy
}

object AppApi {
  implicit def instance[F[_]](
      implicit repo: AppRepository[F],
      genericApi: GenericApi[F],
      todoItemApi: TodoItemApi[F],
      todoListApi: TodoListApi[F],
      tagApi: TagApi[F],
      log: LoggingM[F],
      error: ErrorM[F],
      handler: F ~> Future): AppApi[F] =
    new AppApi[F]
}
