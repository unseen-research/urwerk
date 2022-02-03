package urwerk.cli

import urwerk.test.TestBase

import Parameter.*

class ParameterTest extends TestBase:
  
  Seq(("-", true), ("--", true), ("---", true), ("-n", false), ("--name", false), ("value", false), ("'--'", false), ("\"--\"", false)).foreach{(givenArg, result)=>
    s"given arg $givenArg is separator $result" in {
      isSeparator(givenArg) should be (result)
    }}

  Seq(("-n", true), ("--name", true), ("---name", false), ("-name", false), ("-", false), ("--", false)).foreach{(givenArg, result)=>
    s"given arg $givenArg is name $result" in {
      isName(givenArg) should be (result)
    }}

  Seq(("-flags", true), ("-f", true), ("-123", false), ("--name", false), ("-", false), ("--", false)).foreach{(givenArg, result)=>
    s"given arg $givenArg is flags $result" in {
      isFlags(givenArg) should be (result)
    }}


  Seq(("-n", "n"), ("--n", "n"), ("--name", "name")).foreach{(givenArg, result)=>
    s"given arg $givenArg  name $result" in {
      toName(givenArg) should be (result)
    }}

  Seq(("value", "value"), ("\"\"", ""), ("''", ""), ("\"--\"", "--"), ("'--'", "--"), ("\"--name\"", "--name"), ("'--name'", "--name"), ("\"value", "\"value"), ("'value", "'value"))
    .foreach{(givenArg, result) =>
    s"given arg $givenArg unquoted $result" in {
      stripQuotes(givenArg) should be (result)
    }}

  "value to parameter" in {
    given WithConfig[String] with {}

    val param = "value".toParam
    param.label should be ("STRING")
  }

  "int param" in {
    given WithConfig[Seq[Int]] with {}

    val param = "value".toParam
    param.label should be ("STRING")
  }