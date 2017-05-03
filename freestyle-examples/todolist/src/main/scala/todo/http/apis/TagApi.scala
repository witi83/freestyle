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

import todo.definitions.models.Tag
import todo.definitions.persistence._
import todo.runtime.implicits._

class TagApi[F[_]](implicit repo: TagRepository[F], log: LoggingM[F], handler: F ~> Future) {
  val resetProgram: FreeS[F, Int] =
    for {
      _ <- log.debug("Trying to reset Tag in repository")
      r <- repo.init
      _ <- log.warn("POST /tags/reset: Initialize the Tag table")
    } yield r

  val reset: Endpoint[Int] =
    post("tags" :: "reset") {
      resetProgram.map(Ok(_))
    }

  val retrieve: Endpoint[Tag] =
    get("tags" :: int) { id: Int =>
      for {
        _    <- log.debug("Trying to retrieve an Tag")
        item <- repo.get(id)
        _    <- log.info(s"GET /tags/$id: Found $item")
      } yield item.fold[Output[Tag]](NotFound(new NoSuchElementException))(Ok(_))
    }

  val list: Endpoint[List[Tag]] =
    get("tags") {
      for {
        _     <- log.debug("Trying to get all Tags")
        items <- repo.list
        _     <- log.info(s"GET /tags: Found all the Tags")
      } yield Ok(items)
    }

  val insert: Endpoint[Int] =
    post("tags" :: jsonBody[Tag]) { item: Tag =>
      for {
        _ <- log.debug("Trying to insert a Tag")
        r <- repo.insert(item)
        _ <- log.info(s"POST /tags with $item: Tried to add Tag")
      } yield Ok(r)
    }

  val update: Endpoint[Int] =
    put("tags" :: int :: jsonBody[Tag]) { (id: Int, item: Tag) =>
      for {
        _ <- log.debug("Trying to update a Tag")
        r <- repo.update(item.copy(id = Some(id)))
        _ <- log.info(s"PUT /tags/$id with $item: Tried to update Tag")
      } yield Ok(r)
    }

  val destroy: Endpoint[Int] =
    delete("tags" :: int) { id: Int =>
      for {
        _ <- log.debug("Trying to delete a Tag")
        r <- repo.delete(id)
        _ <- log.info("DELETE /tags/$id: Tried to delete Tag")
      } yield Ok(r)
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
