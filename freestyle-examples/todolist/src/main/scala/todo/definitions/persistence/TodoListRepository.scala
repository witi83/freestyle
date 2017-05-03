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
package definitions
package persistence

import models.TodoList
import doobie.imports._
import freestyle._

@free
trait TodoListRepository {
  def drop: FS[Int]
  def create: FS[Int]
  def get(id: Int): FS[Option[TodoList]]
  def insert(input: TodoList): FS[Option[TodoList]]
  def list: FS[List[TodoList]]
  def update(input: TodoList): FS[Option[TodoList]]
  def delete(id: Int): FS[Int]
  def init: FS.Seq[Int] =
    for {
      drops   <- drop
      creates <- create
    } yield drops + creates
}

class H2TodoListRepositoryHandler extends TodoListRepository.Handler[ConnectionIO] {
  val drop: ConnectionIO[Int] = sql"""DROP TABLE todo_lists IF EXISTS""".update.run

  val create: ConnectionIO[Int] =
    sql"""CREATE TABLE todo_lists (id INT AUTO_INCREMENT PRIMARY KEY, title VARCHAR, tag_id INT, FOREIGN KEY (tag_id) REFERENCES TAGS(id))""".update.run

  def get(id: Int): ConnectionIO[Option[TodoList]] =
    sql"""SELECT title, tag_id, id FROM todo_lists WHERE id = $id"""
      .query[TodoList]
      .option

  def insert(input: TodoList): ConnectionIO[Option[TodoList]] =
    for {
      id <- sql"""INSERT INTO todo_lists (title, tag_id) VALUES (${input.title}, ${input.tagId})""".update
        .withUniqueGeneratedKeys[Int]("id")
      item <- get(id)
    } yield item

  def list: ConnectionIO[List[TodoList]] =
    sql"""SELECT title, tag_id, id FROM todo_lists ORDER BY id ASC"""
      .query[TodoList]
      .list

  def update(input: TodoList): ConnectionIO[Option[TodoList]] =
    for {
      id <- sql"""UPDATE todo_lists SET title = ${input.title}, tag_id = ${input.tagId} WHERE id = ${input.id}""".update
        .withUniqueGeneratedKeys[Int]("id")
      item <- get(id)
    } yield item

  def delete(id: Int): ConnectionIO[Int] =
    sql"""DELETE FROM todo_lists WHERE id = $id""".update.run
}
