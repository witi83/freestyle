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
  def insert(input: TodoList): FS[Int]
  def list: FS[List[TodoList]]
  def update(input: TodoList): FS[Int]
  def delete(id: Int): FS[Int]
  def init: FS.Seq[Int] =
    for {
      drops   <- drop
      creates <- create
    } yield drops + creates
}

class H2TodoListRepositoryHandler extends TodoListRepository.Handler[ConnectionIO] {
  val drop = sql"""DROP TABLE todo_lists IF EXISTS""".update.run

  val create =
    sql"""CREATE TABLE todo_lists (id INT AUTO_INCREMENT PRIMARY KEY, title VARCHAR)""".update.run

  def get(id: Int) =
    sql"""SELECT title, id FROM todo_lists WHERE id = $id"""
      .query[TodoList]
      .option

  def insert(input: TodoList) =
    sql"""INSERT INTO todo_lists (title) VALUES (${input.title})""".update.run

  def list =
    sql"""SELECT title, id FROM todo_lists ORDER BY id ASC"""
      .query[TodoList]
      .list

  def update(input: TodoList) =
    sql"""UPDATE todo_lists SET title = ${input.title} WHERE id = ${input.id}""".update.run

  def delete(id: Int) = sql"""DELETE FROM todo_lists WHERE id = $id""".update.run
}
