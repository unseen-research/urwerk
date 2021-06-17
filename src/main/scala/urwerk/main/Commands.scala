package urwerk.main

import urwerk.source.Source
import urwerk.source.Source.Sink

trait ParamSpec[V, C]:
  def commit(using c: Command[?, ?])(op: (V, c.CC) => c.CC): ParamSpec[V, c.CC]


trait Command[C, R]:
  type RR = R
  type CC = C
  def apply[CC](config: CC): RR


given z: Command[Long, Boolean] = new Command[Long, Boolean]:
    def apply[Long](config: Long): Boolean = ???


def param[V](using c: Command[?, ?])(names: String*): ParamSpec[V, c.CC] = ???

object Command:
  def apply[C, R](using c: Command[C, R])(specs: ParamSpec[?, C]*): Command[C, R] = 
    ???
    
val xx = param[String]("")
    .commit  

val yy = xx((s, l)=> l)

val x = Command(
  param[String]("")
    .commit((value, config) => config)
)