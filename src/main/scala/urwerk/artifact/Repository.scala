package urwerk.artifact

import urwerk.source.Source
import urwerk.io.ByteString

trait Repository:

  def artifactVersions(id: String): Source[String]


  def artifact(id: String, version: String): Source[ByteString]

// id:ur.urwerk.module:
//   version:1.0
//     6ec8f8e717cc839c29087811fb2a6ba18389e1bdf21bd6f681b680ffb509a0d5
//     6ec8f8e717cc839c29087811fb2a6ba18389e1bdf21bd6f681b680ffb509a0d5.metadata
//     tags:
//       platform: windows-10
//       version: 1.0
//       id: ur.urwerk.module
//       type: jar
//       classifier: sources
//     source: http://kdkd

//     play_2.13-2.8.8-javadoc.jar                       2021-04-08 13:22   5984428
//     tags:
//       version: 2.8.8
//       type: javadoc/jar

//     play_2.13-2.8.8.jar                               2021-04-08 13:22   2919436
//     tags:
//       version: 2.8.8
//       type: jar

//     play_2.13-2.8.8.pom
//     tags:
//       version: 2.8.8
//       type: pom

//     play_2.13-2.8.8-javadoc.jar.asc                   2021-04-08 13:22       473
//     play_2.13-2.8.8-javadoc.jar.asc.md5               2021-04-08 13:22        32
//     play_2.13-2.8.8-javadoc.jar.asc.sha1              2021-04-08 13:22        40
//     play_2.13-2.8.8-javadoc.jar.md5                   2021-04-08 13:22        32
//     play_2.13-2.8.8-javadoc.jar.sha1                  2021-04-08 13:22        40
//     play_2.13-2.8.8-playdoc.jar                       2021-04-08 13:22   2994699
//     play_2.13-2.8.8-playdoc.jar.asc                   2021-04-08 13:22       473
//     play_2.13-2.8.8-playdoc.jar.asc.md5               2021-04-08 13:22        32
//     play_2.13-2.8.8-playdoc.jar.asc.sha1              2021-04-08 13:22        40
//     play_2.13-2.8.8-playdoc.jar.md5                   2021-04-08 13:22        32
//     play_2.13-2.8.8-playdoc.jar.sha1                  2021-04-08 13:22        40
//     play_2.13-2.8.8-sources.jar                       2021-04-08 13:22    525873
//     play_2.13-2.8.8-sources.jar.asc                   2021-04-08 13:22       473
//     play_2.13-2.8.8-sources.jar.asc.md5               2021-04-08 13:22        32
//     play_2.13-2.8.8-sources.jar.asc.sha1              2021-04-08 13:22        40
//     play_2.13-2.8.8-sources.jar.md5                   2021-04-08 13:22        32
//     play_2.13-2.8.8-sources.jar.sha1                  2021-04-08 13:22        40
//     play_2.13-2.8.8.jar                               2021-04-08 13:22   2919436
//     play_2.13-2.8.8.jar.asc                           2021-04-08 13:22       473
//     play_2.13-2.8.8.jar.asc.md5                       2021-04-08 13:22        32
//     play_2.13-2.8.8.jar.asc.sha1                      2021-04-08 13:22        40
//     play_2.13-2.8.8.jar.md5                           2021-04-08 13:22        32
//     play_2.13-2.8.8.jar.sha1                          2021-04-08 13:22        40
//     play_2.13-2.8.8.pom                               2021-04-08 13:22      9001
//     play_2.13-2.8.8.pom.asc                           2021-04-08 13:22       473
//     play_2.13-2.8.8.pom.asc.md5                       2021-04-08 13:22        32
//     play_2.13-2.8.8.pom.asc.sha1                      2021-04-08 13:22        40
//     play_2.13-2.8.8.pom.md5                           2021-04-08 13:22        32
//     play_2.13-2.8.8.pom.sha1

