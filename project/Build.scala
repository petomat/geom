/*
 * Copyright (c) 2011-15 Peter Schmitz
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

import sbt._
import Keys._

import com.typesafe.sbt.SbtGit._
import GitKeys._

import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseCreateSrc


import sbtbuildinfo._
import sbtbuildinfo.BuildInfoKeys._


object GeomBuild extends Build {

  lazy val geom = (project in file(".")
    aggregate (core, examples)
    dependsOn (core, examples/*, scratch*/)
    settings (commonSettings: _*)
    settings (moduleName := "geom-root")
  )

 lazy val core = (project
    enablePlugins(BuildInfoPlugin)
    settings(commonSettings: _*)
    settings(
      moduleName := "geom",
      buildInfoPackage := "de.petomat.geom",
      buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
      buildInfoKeys ++= Seq[BuildInfoKey](
        version,
        scalaVersion,
        gitHeadCommit,
        BuildInfoKey.action("buildTime") {
          System.currentTimeMillis
        }
      ),
      EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed
    )
  )

  lazy val examples = (project
    dependsOn core
    settings (commonSettings: _*)
    settings (moduleName := "geom-examples")
  )

  def commonSettings =
    Seq(
      organization := "de.petomat",
      scalaVersion := "2.11.6",
      
      // (unmanagedSourceDirectories in Compile) <<= (scalaSource in Compile)(Seq(_)),
      // (unmanagedSourceDirectories in Test) <<= (scalaSource in Test)(Seq(_)),

      scalacOptions := Seq(
        "-feature",
//        "-language:higherKinds",
//        "-language:implicitConversions",
        "-Xfatal-warnings",
        "-deprecation",
        "-unchecked"),

      initialCommands in console := """import de.petomat.geom._"""
    ) ++
    addCommandAlias("refresh", ";reload;update;clean;compile;eclipse;version") ++
    addCommandAlias("cc", ";clean;~compile")
}