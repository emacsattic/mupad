// $Id$
// test-indentation.mu --- test file for the automatic indentation
// Copyright (C) 2002, François Maltey, Nicolas Thiéry, Olivier Ramaré
// Maintainer: Olivier Ramare <ramare@agat.univ-lille1.fr>

//////////////////////////////////////////////////////////////////////////////
// Indentation in domains
//////////////////////////////////////////////////////////////////////////////

/**
 * 2004/01/11 Bug: the indentation of axiom and bla below is incorrect
 * See email discussion of 12/01/2004
 **/
domain bla
  category ble, bli;
  axiom bla;
  bla;
end_domain;

// Variants with other characters
domain bla
  category ble([1]);
  axiom bla;
end_domain;

domain bla
  category ble({1});
  axiom bla;
end_domain;

domain bla
  category ble("a");
  axiom bla;
end_domain;

/**
 * 2003/09/19 Bug: the indentation of axiom and bla below is incorrect
 * 2004/01/10 Seems to be fixed, though I don't known when.
 **/
domain combinat::subClass(class : DOM_DOMAIN)
  category op(class::getCategories());
  axiom op(class::allAxioms());
  
  bla;
begin
end_domain;

/**
 * 2003/09/01 Bug: the indentation of inherits and bla below is incorrect
 * 2003/09/02 Fixed.
 **/
domain HSGA
  category CatAlgebraWithBasis();
  inherits experimental::AlgebraWithBasis();
  
  bla;
end_domain;

/**
 * Bug: the indentation of bla below is incorrect (fixed 2003/08/29)
 **/
domain bla
  inherits zut;
  category ble::bli;
  
  bla := 1;
end_domain;

category bla
  category truc;
  category tric;
  axiom tric;
  axiom bla;
  
  bla := 1;
begin
end_category:

category bla;
  axiom ble;
  axiom ble;
begin
end_category:

domain bla
  axiom ble;
begin
end_domain:

domain bla
  inherits zut;
  axiom bli;
  category ble;
  
  bla :=proc()
	begin
	  1+x;
	  if
	     bla then
	    bli;
	  else
	    truc;
	  end_if;
	end_proc;
  
  ble := 1;
begin
end_domain:

domain bla;
  axiom ble;
  axiom bli;
  
begin
end_domain:

category bla;
  category ble;
  
  truc := 1;
end;

category ble
  axiom bla;
  category bla;
  
  bla :=
  proc()
  begin
  end_proc:
  
begin
end_category

axiom blo
begin
  1+1;
end_axiom;

if bla then
  1;
else
  2;
elif x=2 then
  3;
end_if;

proc(x)
  local bla;
  local ble;
  option truc;
  local bla;
begin
end_proc:

domain bla;
  category ble;
  category ble;
  axiom truc;
  
  x :=1;
begin
end_domain:

bla :=
proc()
begin
  for i from 1 to 10 do
    print(coucou);
  end_for;
end_proc:

ble :=
proc()
begin
  bla()
end_proc:

domain bla
  /**
   * Bugs:
   * indent-region with all this domain selected raises an error
   * (2003/08/29: fixed)
   * Hitting return below this comment does not indent properly
   * (2003/08/28: seems to be fixed)
   **/
  bla := 1;
end;

/**
 * Embedded proc's with end inside instead of end_proc
 * Bug: Hitting return after end does not indent properly
 * (2003/08/28: fixed)
 **/
proc()
begin
  proc()
  begin
  end:
  bla;
end_proc:

/**
 * Hitting tab inside a non closed comment raises the following error
 * (sli-indent-line): (wrong-type-argument number-or-marker-p nil)
 * (2003/12/02: discovered)
 * (2004/01/10: fixed)
 
// Local Variables:
// mode: mupad
// mupad-domain-indent: 2
// mupad-indent-level: 2
// End:
