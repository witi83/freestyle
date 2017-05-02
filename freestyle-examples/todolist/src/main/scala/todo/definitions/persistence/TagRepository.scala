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

import models.Tag
import doobie.imports._
import freestyle._

@free
trait TagRepository {
  def drop: FS[Int]
  def create: FS[Int]
  def get(id: Int): FS[Option[Tag]]
  def insert(input: Tag): FS[Int]
  def list: FS[List[Tag]]
  def update(input: Tag): FS[Int]
  def delete(id: Int): FS[Int]
  def init: FS.Seq[Int] =
    for {
      drops   <- drop
      creates <- create
    } yield drops + creates
}

class H2TagRepositoryHandler extends TagRepository.Handler[ConnectionIO] {
  val drop = sql"""DROP TABLE tags IF EXISTS""".update.run

  val create =
    sql"""CREATE TABLE tags (id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR)""".update.run

  def get(id: Int) =
    sql"""SELECT name, id FROM tags WHERE id = $id"""
      .query[Tag]
      .option

  def insert(input: Tag) =
    sql"""INSERT INTO tags (name) VALUES (${input.name})""".update.run

  def list =
    sql"""SELECT name, id FROM tags ORDER BY id ASC"""
      .query[Tag]
      .list

  def update(input: Tag) =
    sql"""UPDATE tags SET name = ${input.name} WHERE id = ${input.id}""".update.run

  def delete(id: Int) = sql"""DELETE FROM tags WHERE id = $id""".update.run
}
