package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.io.Import
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 22/03/2016.
 */
class FormulaTest extends FunSuite {

  test("multiple variables") {
    val form = new Formula("a = b,c", true)
    assert(!form.leftMember.wildcard)
    assert(!form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.equals(List(Raw("a"))))
    assert(form.rightMember.terms.equals(List(Raw("b"), Raw("c"))))
  }

  test("multiple responses") {
    val form = new Formula("a,b = c", true)
    assert(!form.leftMember.wildcard)
    assert(!form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.equals(List(Raw("a"),Raw("b"))))
    assert(form.rightMember.terms.equals(List(Raw("c"))))
  }

  test("wildcard response") {
    val form = new Formula("* = c", true)
    assert(form.leftMember.wildcard)
    assert(!form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.isEmpty)
    assert(form.rightMember.terms.equals(List(Raw("c"))))
  }

  test("wildcard response with exception") {
    val form = new Formula("* - a = c", true)
    assert(form.leftMember.wildcard)
    assert(!form.rightMember.wildcard)
    assert(form.leftMember.exceptions.equals(List("a")))
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.isEmpty)
    assert(form.rightMember.terms.equals(List(Raw("c"))))
  }

  test("wildcard response with multiple exceptions") {
    val form = new Formula("* - a,b = c", true)
    assert(form.leftMember.wildcard)
    assert(!form.rightMember.wildcard)
    assert(form.leftMember.exceptions.equals(List("a","b")))
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.isEmpty)
    assert(form.rightMember.terms.equals(List(Raw("c"))))
  }

  test("wildcard variable") {
    val form = new Formula("a = *", true)
    assert(!form.leftMember.wildcard)
    assert(form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.isEmpty)
    assert(form.leftMember.terms.equals(List(Raw("a"))))
    assert(form.rightMember.terms.isEmpty)
  }

  test("wildcard variable with exception") {
    val form = new Formula("a = * - b", true)
    assert(!form.leftMember.wildcard)
    assert(form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.equals(List("b")))
    assert(form.leftMember.terms.equals(List(Raw("a"))))
    assert(form.rightMember.terms.isEmpty)
  }

  test("wildcard variable with multiple exceptions") {
    val form = new Formula("a = * - b,c", true)
    assert(!form.leftMember.wildcard)
    assert(form.rightMember.wildcard)
    assert(form.leftMember.exceptions.isEmpty)
    assert(form.rightMember.exceptions.equals(List("b","c")))
    assert(form.leftMember.terms.equals(List(Raw("a"))))
    assert(form.rightMember.terms.isEmpty)
  }

}
